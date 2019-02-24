library("docstring")
library("tictoc")

DE <- function(S, N, w, p, XMin, XMax, TrueMin=0, Threshold=1e-8) {
  
  #' Differential Evolution (DE)
  #'
  #' Returns the best solution (minimum) and function value of a function S on a given
  #' interval [XMin,XMax] after MaxIter iterations found using the DE method.
  #'
  #' @param S objective function.
  #' @param N sample size.
  #' @param w scale factor.
  #' @param p crossover factor.
  #' @param XMin minimum of range of x (e.g. rep(-1,10)).
  #' @param XMax maximum of range of x (e.g. rep(1,10)).
  #' @param TrueMin true minimum of S.
  #' @param ThresHold accuracy of optimum.
  #' 
  #' @references Duan, Q.; Kroese, D.P. (2016). Splitting for optimization.
  #' Elsevier, Computers & Operations Research 73: 119-131.
  #' 
  #' @examples
  #' DE(DeJong1,30,0.5,0.2,rep(-100,30),rep(100,30))
  
  tic()
  t <- 0
  n <- length(XMin)
  MaxIter <- 250000
  
  # generate set of points Chi via uniform sampling
  Chi <- matrix(nrow=N,ncol=n)
  for (i in 1:n) {
    Chi[,i] <- runif(N,min=XMin[i],max=XMax[i])
  }
  
  # evaluate each point of Chi
  evals <- numeric(N)
  for (j in 1:N) {
    evals[j] <- S(Chi[j,])
  }
  
  FuncEvals <- N
  
  # best value
  b <- min(evals)
  
  # run with simple stopping criterion
  while (b > (TrueMin + Threshold) && t < MaxIter) {
    
    for (i in 1:N) {
      
      Xi <- Chi[i,]
      SXi <- evals[i]
      
      # mutation
      r <- sample((1:N)[1:N!=i],3)
      Yi <- Chi[r[1],] + w*(Chi[r[2],] - Chi[r[3],])
      
      # crossover
      Bi <- rbinom(n,1,p)
      XStar <- Bi*Yi + (1 - Bi)*Xi
      
      # selection
      SXstar <- S(XStar)
      FuncEvals <- FuncEvals + 1
      
      if (SXstar <= SXi) {
        Chi[i,] <- XStar
        evals[i] <- SXstar
        b <- min(SXstar,b)
      }
    }
    t <- t + 1
  }
  
  time = toc()
  return(list("time"=mean(time$toc - time$tic), "evals"=FuncEvals, "iters"=t, "bestEval"=b))
}
