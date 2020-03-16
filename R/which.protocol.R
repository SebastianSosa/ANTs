#' @title Which metric to choose
#' @description An interactive decision tree to choose the most appropriate ANTs analytical protocol and related function according to the research question.
#' @author Sebastian Sosa
#' @keywords export
which.protocol <- function(){
  browseURL(paste(system.file(package = "ANTs"),"/www/ANTs which protocol.html", sep = ''))
  # .Tcl("update") for tcltk window function
}
