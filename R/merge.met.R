#' @title Merge data frame with metric
#' @description merge metric to a data frame
merge.met <- function(vec, names, df, dfid, met){
  ori.name = colnames(df)[dfid]
  d2 = data.frame(names, vec)
  colnames(d2)[1] = ori.name
  colnames(d2)[2] = met
  
  result = merge(df, d2, by = ori.name, all = TRUE)
  return(result)
}
