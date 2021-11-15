#' @title Which metric to choose
#' @description An interactive decision tree to choose the most appropriate network measure according to the research question and network analysis level.
#' @details For more details on each of these metrics, see Sosa et al. 2020.
#' @author Sebastian Sosa
#' @references Sosa, S., Sueur, C., & Puga Gonzalez, I. (2020). Network measures in animal social network analysis: their strenths, limits, interpretations and uses. Methods in Ecology and Evolution.
#' @return a decision tree.
#' @keywords export
which.metric <- function(){
  # Create a temporary directory
  tempdir <- paste(system.file(package = "ANTs"),"/","www", sep = "")
  tmpFile <- file.path(tempdir,  'ANTsMetrics')
  file.copy(tmpFile, paste0(tempdir,"/ANTs which metric.html"), overwrite = TRUE)
  browseURL(paste(system.file(package = "ANTs"),
                  "/www/ANTs which metric.html", sep = ''))
  # .Tcl("update") for tcltk window function
}
