#' @title Which metric to choose
#' @description An interactive decision tree to choose the most appropriate ANTs analytical protocol and related function according to the research question.
#' @author Sebastian Sosa
#' @return a decision tree.
#' @keywords export
which.protocol <- function(){
  tempdir <- paste(system.file(package = "ANTs"),"/","www", sep = "")
  tmpFile <- file.path(tempdir,  'ANTsProtocol.txt')
  file.copy(tmpFile, paste0(tempdir,"/ANTs which protocol.html"), overwrite = TRUE)
  browseURL(paste(system.file(package = "ANTs"),
                  "/www/ANTs which metric.html", sep = ''))
}
