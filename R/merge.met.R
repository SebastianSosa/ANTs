#' @title Merge data frame with metric
#' @description merge metric to a data frame
#' @param vec a vector to merge to argument df
#' @param names a string vector of same length as argument vec, with IDs of each element of argument vec
#' @param df a data frame to merge vec
#' @param dfid an integer indicating the column in argument df in which IDS are stored.
#' @param met a string for the new column merged in argument df
#' @keywords internal
merge.met <- function(vec, names, df, dfid, met){
  ori.name = colnames(df)[dfid]
  d2 = data.frame(names, vec)
  colnames(d2)[1] = ori.name
  colnames(d2)[2] = met
  
  result = merge(df, d2, by = ori.name, all = TRUE)
  return(result)
}
