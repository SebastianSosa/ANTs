#' @title Which Metric to choose
#' @description An interactive decission tree to choose the most appropriate network measure according to the research question and network analysis level.
#' @details For more details on each of those metrics see Sosa et al. 2020.
#' @references https://doi.org/10.1111/2041-210X.13366
#' 
which.metric <- function(){
  browseURL(normalizePath("inst/www/which.metric.html"))
  # .Tcl("update") for tcltk window function
}
