library("docstring")

CompressResults <- function(Method,ResultsFile="results.txt") {
  #' CompressResults
  #'
  #' @description Creates a file ResultsFile consisting of
  #' a summary of all experiments with our 23 objective
  #' functions using method Method.
  #' 
  #' @examples CompressResults("SCO")
  #' 

  Functions <- c("DeJong1","Schwefel222","Schwefel12","Schwefel221","DeJong2",
                 "StepFun","DeJong4","Schwefel226","Rastrigin","Ackley",
                 "Griewank","Penalized1","Penalized2","DeJong5","Kowalik",
                 "SixHumpCamel","BraninHoo","GoldsteinPrice","Hartmann3",
                 "Hartmann6","Shekel5","Shekel7","Shekel10")
  
  write("Function Method Min Mean Max CPU Iters VarIters Evals OutRuns",
        ResultsFile,append=F)
  
  for (func in Functions) {
    print(Method)
    print(toString(func))
    
    FileName <- paste(Method,"_",func,sep="")
    funcName <- func
    Method <- Method
    runs <- 1000
    
    result <- read.table(paste(FileName,".txt",sep=""),sep="",row.names=NULL)
    result <- result[order(result$time),]
    line <- paste(funcName,Method,min(result$best),mean(result$best),
                  max(result$best),mean(result$time),mean(result$iters),
                  var(result$iters),mean(result$evals),runs-length(result$time))
    
    print(line)
    write(line,ResultsFile,append=T)
  }
}
