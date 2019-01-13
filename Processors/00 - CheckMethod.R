library("docstring")

CheckMethod <- function(n, SCOVersion, funcName, funcParam, resultsFile) {
  #' CheckMethod
  #'
  #' @description Runs SCOVersion n times with the given parameters.
  #' 
  #' @examples CheckMethod(1000,SCO,DeJong1,list(DeJong1,30,0.4,0.5,5,
  #' rep(-100,30),rep(100,30)),"SCO_DeJong1_1000")
  #' 
  
  times <- numeric(n); evals <- numeric(n)
  iters <- numeric(n); bests <- numeric(n)
  
  for (i in 1:n) {
    result <- do.call(SCOVersion,funcParam)
    
    evals[i] <- result$evals; times[i] <- result$time
    iters[i] <- result$iters; bests[i] <- result$bestEval
  }
  
  result <- data.frame(time=times,evals=evals,iters=iters,best=bests)
  write.table(result,file=paste(resultsFile,".txt",sep=""))
  result <- result[order(result$time),]
}
