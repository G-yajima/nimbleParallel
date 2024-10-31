#' @title base function
#' @description \code{runMCMC_fn} base function
#'
#' @importFrom nimble nimbleModel
#' @importFrom nimble compileNimble
#' @importFrom nimble configureMCMC
#' @importFrom nimble configureMCMC
#' @importFrom nimble buildMCMC
#' @importFrom nimble compileNimble
#' @importFrom nimble runMCMC
#' @param info XXX
#' @param data XXX
#' @param constants XXX
#' @param code XXX
#' @param params XXX
#' @param ni XXX
#' @param nt XXX
#' @param nb XXX
#' @return result of one chain
#' @export
runMCMC_fn <- function(info, data, constants, code, params, ni, nt, nb) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("The nimble package is required. Please install it.")
  }
  myModel <- nimble::nimbleModel(code = code,
                         data = data,
                         constants = constants,
                         inits = info$inits)

  CmyModel <- nimble::compileNimble(myModel)

  configModel <- nimble::configureMCMC(myModel, monitors = params)

  myMCMC <- nimble::buildMCMC(configModel, monitors = params)
  CmyMCMC <- nimble::compileNimble(myMCMC)

  results <- nimble::runMCMC(CmyMCMC, niter = ni, nburnin = nb, thin = nt,nchains = 1, setSeed = info$seed, samplesAsCodaMCMC = TRUE)

  results
}

#' @title Function to run NIBMLE in parallel
#' @description \code{runMCMC_para} runs any NIMBLE model in parallel using the \code{parallel} package.
#'
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom nimble configureMCMC
#' @importFrom nimble nimbleModel
#' @importFrom nimble compileNimble
#' @importFrom nimble configureMCMC
#' @importFrom nimble configureMCMC
#' @importFrom nimble buildMCMC
#' @importFrom nimble compileNimble
#' @importFrom nimble runMCMC
#' @param info XXX
#' @param data XXX
#' @param constants XXX
#' @param code XXX
#' @param params XXX
#' @param ni XXX
#' @param nt XXX
#' @param nb XXX
#' @return out
#' @export
runMCMC_para <- function(info, data, constants, code, params, ni, nt, nb) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("The nimble package is required. Please install it.")
  }

  nc <- length(info)
  this_cluster <- parallel::makeCluster(nc)

  # Load required packages on each worker
  parallel::clusterEvalQ(this_cluster, {
    library(nimble)
  })

  # Export necessary objects to the workers
  parallel::clusterExport(this_cluster,
                          varlist = c("runMCMC_fn"),
                          envir = environment())

  # Run the parallel computation
  chain_output <- parallel::parLapply(cl = this_cluster,
                                      X = info,
                                      fun = runMCMC_fn,
                                      data = data,
                                      code = code,
                                      constants = constants,
                                      params = params,
                                      ni = ni,
                                      nt = nt,
                                      nb = nb)

  parallel::stopCluster(this_cluster)

  # Combine results
  out <- coda::mcmc.list(chain_output)
  out
}
