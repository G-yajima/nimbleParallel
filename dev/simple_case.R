devtools::load_all()

library(nimble)
# data -------------------------------------------------------------------------
R <- 200
J <- 3

# パラメータ
lambda <- 5
p <- 0.5

# 局所個体数
set.seed(1234)
N <- rpois(n = R, lambda = lambda)

# 計測値データの生成
y <- array(NA, dim = c(R, J))
for (j in 1:J) {
  y[,j] <- rbinom(n = R, size = N, prob = p)
}

# code -------------------------------------------------------------------------
code <- nimbleCode(
  {
    # Priors
    lambda ~ dgamma(0.005, 0.005) # Standard vague prior for lambda
    p ~ dunif(0, 1)

    # Likelihood
    # Biological model for true abundance
    for (i in 1:R) {
      N[i] ~ dpois(lambda)
      # Observation model for replicated counts
      for (j in 1:J) {
        y[i,j] ~ dbin(p, N[i])
      } # j
    } # i
  }
)

# run MCMC ---------------------------------------------------------------------
## Parameters monitored
params <- c("lambda", "p")

## MCMC settings
ni <- 1000
nt <- 1
nb <- 500
nc <- 4

## Initial values
inits <-function() {
  list(p = runif(1, 0, 1), lambda = runif(1, 0, 1), N = apply(y, 1, max) + 1)
}

info <- list(
  list(seed = sample(1:9999, 1),
       inits = inits()),
  list(seed = sample(1:9999, 1),
       inits = inits()),
  list(seed = sample(1:9999, 1),
       inits = inits())
)

##
data <- list(y = y)
constants <- list(R = R, J = J)

# out <- runMCMC_fn(info[[1]], data, constants, code, params, ni, nt, nb)
out <- runMCMC_para(info, data, constants, code, params, ni, nt, nb)

plot(out)
