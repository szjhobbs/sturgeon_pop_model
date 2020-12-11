# ******************************************************************************
# Created: 30-Apr-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file contains functions used (mostly) to run simulations for the
#          White Sturgeon population model (S. Blackburn, Univ. of Idaho); some
#          functions are wrappers for functions found in popbio package
# ******************************************************************************

# notes -------------------------------------------------------------------

# section contains notes to provide more background

# from results of BetavalSims we need the following

# simulate environmental stochasticity for age-0 survival only; WST only having
# successfull recruitment every 5 years (on average)

# # isolate age-0 simulated survival
# recruit.rate = surv_obs_sims[1,]
#
# # on avg. successful recruitent every 5 years
# recruit.dat = c(rep(0,iters*9), recruit.rate)
#
# # randomnly sample from recruit.dat
# V = sample(recruit.dat, iters, replace=T)
#
# # replace age-0 simulated survival rates
# surv_obs_sims[1,] = V

# (1) assume for now output of BetavalSims() is matrix
# (2) recruit.rate is first row of matrix (in this case = age0)
# (3) recruit.dat should be *4 if every 5 years - in other words
#     multiplier should be (I think) 1 less than year span
# (4) V random selection with replacement, should be preceeded by
#     set.seed() [I think]

# vectorize functions -----------------------------------------------------

# use popbio::betaval() for this but need to vectorize for ages (0-19)
bval <- Vectorize(
  FUN = popbio::betaval,
  vectorize.args = c("mn", "sdev")
)

sbval <- Vectorize(
  FUN = popbio::stretchbetaval,
  vectorize.args = c("mn", "std", "minb", "maxb")
)

# vectorized simulation functions -----------------------------------------

BetavalSims <- function(data, mn, sdev, iterations = 10) {

  m <- as.character(substitute(mn))
  s <- as.character(substitute(sdev))

  means <- data[[m]]
  sds <- data[[s]]

  # slow when n is large, may want to try using vapply
  replicate(n = iterations, expr = bval(mn = means, sdev = sds))
  # Replicate(n = iterations, nRow = 20, expr = bval(mn = means, sdev = sds))

}
# end BetavalSims

SBetavalSims <- function(data, mn, sdev, minb = 0, maxb = NULL,
                         iterations = 10) {

  m <- as.character(substitute(mn))
  s <- as.character(substitute(sdev))

  means <- data[[m]]
  stds <- data[[s]]

  if (is.null(maxb)) {
    maxb <- 3 * means
    warning("`maxb` set at 3 * ", m, call. = FALSE)
  }

  replicate(
    n = iterations,
    expr = sbval(
      mn = means,
      std = stds,
      minb = minb,
      maxb = maxb,
      fx = runif(n = 1)
    )
  )

}
# end SBetavalSims

Replicate <- function(n, nRow, expr) {
  # currently not used, but is similar to R's replicate()

  # sapply(integer(n), eval.parent(substitute(function(...) expr)),
  #        simplify = simplify)

  # fun_val <- matrix(nrow = nRow, ncol = n)
  # fun_val <- matrix(data = 0, nrow = nRow)

  vapply(
    integer(n),
    FUN = eval.parent(substitute(function(...) expr)),
    # FUN.VALUE = fun_val
    FUN.VALUE = double(nRow)
  )

}
# end Replicate

# sample simulation functions ---------------------------------------------

# used to change first row of simulated probabilities of survival
SampleSims <- function(data, iterations, rowNum = 1, span = 5, seed = NULL) {

  sims <- data[rowNum, ]

  sim_data <- c(rep(0, times = iterations * (span - 1)), sims)

  # randomnly sample from sim_data
  # set.seed(seed = )
  rnd = sample(sim_data, size = iterations, replace = TRUE)

  # rnd = V is S. Blackburn model
  rnd

  # left off here --> how to replace first row of data?
  # 18-Apr-2018 1647 -- handled in SurvivalSims()

}
# end SampleSims

# model output functions --------------------------------------------------

# need to rethink GetLambda needs a bit of tweaking and may need to create
# another function which is handled by vapply()

GetLambda <- function(data, mn, sdev, fecundSims, survSims, ...) {

  if (!identical(dim(fecundSims), dim(survSims))) {
    stop("Sim dimensions must be equal.", call. = FALSE)
  }

  # for setting size of matrix & looping
  dims <- dim(fecundSims)
  n <- dims[1]
  sims <- dims[2]

  # variable to hold the Leslie matrix (where nrows = ncols)
  A <- matrix(data = 0, nrow = n, ncol = n)

  # for use in getting simulated survival of last age
  m <- data[n, as.character(substitute(mn))]
  s <- data[n, as.character(substitute(sdev))]

  # for use in vapply below
  out_value <- list(
    lambda = 0,
    stable.stage = 0,
    stage.vector = 0,
    pop.sizes = 0,
    pop.changes = 0
  )

  out <- vapply(seq_len(sims), FUN = function(j) {

    # resets the matrix to all 0s
    A <- A

    # fecundity in first row of Leslie matrix A
    A[1, ] <- fecundSims[, j] * survSims[, j]

    # survival rates for ages
    diag(A[-1, ]) <- survSims[1:(n - 1), j]

    # last age does not die
    A[n, n] <- popbio::betaval(mn = m, sdev = s)

    # get lambda & the important 'pop.changes' using the Leslie matrix (A)
    res <- popbio::pop.projection(A = A, ...)

    # FUN output
    res

  }, FUN.VALUE = out_value)

  # vapply function output - mean log lambda for each simulation; pop.changes is
  # lambda between ages
  mll <- vapply(
    out["pop.changes", ],
    FUN = MeanLogLambda,
    FUN.VALUE = numeric(1L)
  )

  bounds <- exp(quantile(mll, probs = c(0.025, 0.975), names = FALSE))

  # function output
  data.frame(
    Level = "TBD",
    Sims = length(mll),
    MeanLambda = exp(mean(mll)),
    MedLambda = exp(median(mll)),
    LBLambda = bounds[1],
    UBLambda = bounds[2],
    stringsAsFactors = FALSE
  )
}
# end GetLambda

MeanLogLambda <- function(popChanges) {
  mean(log(popChanges))
}

# getting all prob_surv ---------------------------------------------------

# for now using S. Blackburn nomenclature -->
# (1) run surv_nomu_sims using prob_survival_obs & prob_survival_nomu
# (2) get sample sims (V) using first row (1) of surv_obs_sims &
#     surv_nomu_sims
# (3) use V to replace first row (1) of surv_obs_sims & surv_nomu_sims
# (4) repeat BetavalSims() on each prob_survival_<01-30>mu
# (5) for each simulation surv_<01-30>mu_sims, use V (from surv_nomu_sims)
#     to replace first row (1)

SurvivalSims <- function(probSurv, env, iterations, span) {

  # get simulations from known probabilities
  sims <- lapply(probSurv, FUN = function(x) {
    BetavalSims(
      data = get(x, envir = env),
      mn = prob,
      sdev = SE,
      iterations = iterations
    )
  })

  # after getting simulations (sims) we need to replace first row of each item
  # in sims accordingly; per S. Blackburn we replace obs with obs & all other mu
  # with nomu; below works but could bog down when iterations is large
  obs <- grep(pattern = "obs$", x = probSurv, value = TRUE)
  nomu <- grep(pattern = "nomu$", x = probSurv, value = TRUE)

  # sims[[30]][1, ]
  # sims[[nomu]]

  V_obs <- SampleSims(
    data = sims[[obs]],
    iterations = iterations,
    rowNum = 1,
    span = span
  )

  V_nomu <- SampleSims(
    data = sims[[nomu]],
    iterations = iterations,
    rowNum = 1,
    span = span
  )

  lapply(probSurv, FUN = function(x) {
    if (x %in% obs)
      sims[[x]][1, ] <<- V_obs
    else
      sims[[x]][1, ] <<- V_nomu
  })

  # function output
  sims
}
# end SurvivalSims
