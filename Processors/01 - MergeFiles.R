library("docstring")

MergeFiles <- function(FileName, MaxNumber=1000, MinNumber=1) {
  #' MergeFiles
  #'
  #' @description Merges all results of FileName to a single file.
  #' 
  #' @examples MergeFiles(SCO_DeJong1)
  #' 

  MergeSet <- read.table(paste(deparse(substitute(FileName)),
                               "_",MinNumber,".txt",sep=""),header=T)
  for (i in (MinNumber+1):MaxNumber) {
    if (file.exists(paste(deparse(substitute(FileName)),"_",i,".txt",sep=""))) {
      TempSet <- read.table(paste(deparse(substitute(FileName)),
                               "_",i,".txt",sep=""),header=T)
      MergeSet <- merge.data.frame(MergeSet,TempSet,all=T)
    }
  }
  
  write.table(MergeSet,file=paste(deparse(substitute(FileName)),".txt",sep=""))
}
