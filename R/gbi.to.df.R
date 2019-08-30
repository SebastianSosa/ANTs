#' @title Group by individual matrix to linear data frame of association 
#' @description Convert Linear gbi to a inear data frame of association 
#' @param gbi a a group (N) by individual (I) matrix
#' @return a linear data frame of association.
#' @example gbi.to.df(sim.gbi)
#' @keywords internal
gbi.to.df <- function(gbi){
  ID = NULL
  scan = NULL
  for(a in 1:nrow(gbi)){
    ids.associated = names(which(gbi[a, ] != 0))
    ID = c(ID, ids.associated)
    scan = c(scan, rep(row.names(gbi)[a], length(ids.associated)))
  }
  df = data.frame(scan, ID)
  return(df)
}
