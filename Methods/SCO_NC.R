library("docstring")
library("tictoc")

SCO_NC <- function(S, N, rarity=0.8, w=0.5, MaxTry=5, XMin, XMax, TrueMin=0, ThresHold=1e-8) {
  
  #' Splitting for Continuous Optimization, No Componentwise update (SCO_NC)
  #'
  #' Returns the best solution (minimum) and function value of a function S on a given
  #' interval [XMin,XMax] after MaxIter iterations found using the SCO_NC method.
  #'
  #' @param S objective function.
  #' @param N sample size.
  #' @param rarity rarity parameter on the interval (0,1].
  #' @param w scale factor.
  #' @param MaxTry maximum number of component optimization attempts per iteration.
  #' @param XMin minimum of range of x (e.g. rep(-1,10)).
  #' @param XMax maximum of range of x (e.g. rep(1,10)).
  #' @param TrueMin true minimum of S.
  #' @param ThresHold accuracy of optimum.
  #' 
  #' @references Duan, Q.; Kroese, D.P. (2016). Splitting for optimization.
  #' Elsevier, Computers & Operations Research 73: 119-131.
  #' 
  #' @examples
  #' SCO_NC(DeJong1,30,0.4,0.5,5,rep(-100,30),rep(100,30))
  
  tic()
  t <- 0
  n <- length(XMin)
  Ne <- ceiling(N*rarity)
  MaxIter <- 10000
  
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
  while (b > (TrueMin + ThresHold) && t < MaxIter) {
    
    # sort Chi by evalutaion value
    Sx <- data.frame(index=1:N,eval=evals)
    SmallSx <- Sx[order(Sx$eval),][1:Ne,]
    Eps <- Chi[SmallSx[,1],]
    
    b <- SmallSx[1,2]
    
    # splitting factors
    SumBi <- N %% Ne
    Bi <- sample(c(rep(1,SumBi),rep(0,Ne - SumBi)))
    Split <- floor(N/Ne) + Bi
    
    ChiNext <- NULL
    EvalsNext <- NULL
    
    # splitting part
    for (i in 1:Ne) {
      
      y <- Eps[i,]
      yStar <- y
      Sy <- SmallSx$eval[i]
      
      Sigma <- numeric(n)
      
      # optimization part
      for (j in 1:Split[i]) {
        
        # compute Sigma with R uniformly in {1,...,Ne}\{i}
        R <- sample((1:Ne)[1:Ne!=i],1)
        XR <- Eps[R,]
        Sigma <- w*abs(y - XR)
        
        # update y if evaluation becomes smaller (MaxTry attempts)
        for (Try in 1:MaxTry) {
          
          yStar <- yStar + Sigma*rnorm(n)
          SyStar <- S(yStar)
          FuncEvals <- FuncEvals + 1
          
          # check whether we've found a valid better point
          if (SyStar > Sy | min(yStar < XMin) | min(yStar > XMax)) {
            yStar <- y
            SyStar <- Sy
          } else {
            y <- yStar
            Sy <- SyStar
            break
          }
        }
        
        ChiNext <- rbind(ChiNext,yStar)
        EvalsNext <- rbind(EvalsNext,SyStar)
        yStar <- y
      }
    }
    
    Chi <- ChiNext
    evals <- EvalsNext
    t <- t + 1
  }
  
  time = toc()
  return(list("time" = mean(time$toc - time$tic),"evals" = FuncEvals,"iters" = t,"bestEval" = b))
}
