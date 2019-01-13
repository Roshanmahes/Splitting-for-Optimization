library("docstring")

ExtractFiles <- function(Method,FunctionName,MaxIter) {
  #' ExtractFiles
  #'
  #' @description Extracts all good runs (runs below
  #' MaxIter iterations) of the experiment performed
  #' by method Method on function FunctionName.
  #' 
  #' @examples ExtractFiles(SCO,DeJong1,10000)
  #' 
  
  FileName <- paste(deparse(substitute(Method)),
                    "_",deparse(substitute(FunctionName)),".txt",sep="")
  DataSet <- read.table(FileName,header=T)
  len <- length(DataSet[,1])
  bad <- c()
  for (i in 1:len) {
    print(paste(i,DataSet[i,]$iters))
    if (DataSet[i,]$iters == MaxIter) {
      bad <- c(bad,i)
    }
  }
  if (!is.null(bad)) {
    DataSet <- DataSet[-bad,]
  }
  write.table(DataSet,file=FileName)
}
