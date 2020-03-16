#' @title Which metric to choose
#' @description An interactive decision tree to choose the most appropriate network measure according to the research question and network analysis level.
#' @details For more details on each of these metrics, see Sosa et al. 2020.
#' @author Sebastian Sosa
#' @references Sosa, S., Sueur, C., & Puga‐Gonzalez, I. (2020). Network measures in animal social network analysis: their strengths, limits, interpretations and uses. Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210x.13366
#' @keywords export
which.metric <- function(){
  browseURL(paste(system.file(package = "ANTs"),"/www/ANTs which metric.html", sep = ''))
  # .Tcl("update") for tcltk window function
}
