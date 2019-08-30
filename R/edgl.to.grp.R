#' @title Dyadic data frame (edge list) to linear group by individual data frame
#' @description Converts a dyadic data frame (edge list) to a linear group by individual data frame 
#' @param df a symetric edgelist of individual associations.
#' @param id1 a numeric or character vector indicating the column of the first individual association.
#' @param id2 a numeric or character vector indicating the column of the second individual association.
#' @param scan a numeric or character vector representing one or more columns used as scan factors.
#' @return a data frame with individual associations according to scan variable(s) declared by the user.
#' @keywords internal
edgl.to.grp <- function(df, id1, id2, scan){
  # Find columns ids corresponding to individuals----------------------
  col.id1 <- ANTs:::df.col.findId(df, id1)
  col.id2 <- ANTs:::df.col.findId(df, id2)
  df[,col.id1] <- as.character(df[,col.id1])
  df[,col.id2] <- as.character(df[,col.id2])
  # Check if argument scan corresponds to one or to multiple columns----------------------
  if (length(scan)>1) {
    # Find id columns corresponding to argument scan----------------------
    df <- df.ctrlFactor(df, scan)
    scan <- ANTs:::df.col.findId(df, scan)
    # Create a new column merging scan columns----------------------
    col.scan <- ANTs:::df.col.findId(df, "control")
  }
  else {
    col.scan <- ANTs:::df.col.findId(df, scan)
  }
  
  # Find all unique individuals----------------------
  result <- df[, c(col.id1, col.scan)]
  colnames(result) = c('id', colnames(df)[col.scan])
  return(result)
}
