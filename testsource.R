

testfun <- function (A, n, iterations = 20) 
{
  x <- length(n)
  t <- iterations
  stage <- matrix(numeric(x * t), nrow = x)
  pop <- numeric(t)
  pop2 <- numeric(t)
  change <- numeric(t - 1)
  for (i in 1:t) {
    stage[, i] <- n
    pop[i] <- sum(n)
    pop2[i] <- sum(n[, 2])
    if (i > 1) {
      change[i - 1] <- pop[i]/pop[i - 1]
    }
    n <- A %*% n
  }
  # rownames(stage) <- rownames(A)
  # colnames(stage) <- 0:(t - 1)
  # w <- stage[, t]
  # pop.proj <- list(lambda = pop[t]/pop[t - 1], stable.stage = w/sum(w), 
  #                  stage.vectors = stage, pop.sizes = pop, pop.changes = change)
  # pop.proj
  
  list(
    Pop = pop,
    Pop2 = pop2,
    Lambda = pop[t]/pop[t - 1],
    Lambda2 = pop2[t]/pop2[t - 1]
    
  )
}