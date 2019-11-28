#' @title Linear group by individual data frame to dyadic data frame (edge list) 
#' @description Converts linear group by individual data frame to a dyadic data frame (edge list) 
#' @param df a data frame of individual associations.
#' @param scan a numeric or character vector representing one or more columns used as scan factors.
#' @param id a numeric or character vector indicating the column holding ids of individuals.
#' @return a data frame with individual associations (symmetric data) according to the scan variable(s) declared by the user.
#' @examples grp.to.edgl(sim.grp, scan = c('day', 'location'), id = 'ID')
#' @keywords internal
grp.to.edgl <- function(df, scan, id){
  # Find columns ids corresponding to individuals----------------------
  col.id <- df.col.findId(df, id)
  
  # Check if argument scan corresponds to one or to multiple columns----------------------
  if (length(scan)>1) {
    # Find id columns corresponding to argument scan----------------------
    df <- df.ctrlFactor(df, scan)
    scan <- df.col.findId(df, scan)
    # Create a new column merging scan columns----------------------
    col.scan <- df.col.findId(df, "control")
  }
  else {
    col.scan <- df.col.findId(df, scan)
  }

  # Find all unique individuals----------------------

  result = NULL
  s = unique(df[,col.scan])
  for (a in 1:length(s)) {
    # Individuals asosciated for each scan
    tmp = df[df[,col.scan] %in% s[a],]
    ids = tmp[,col.id]
    if(length(ids) > 1){
      result = rbind(result,data.frame(expand.grid(ids,ids), tmp[1,scan], row.names = NULL))
    }
  }
  colnames(result) = c('id1', 'id2', paste(colnames(df)[col.scan]))
  
  result = result[result$id1 != result$id2,]
  return(result)
}
