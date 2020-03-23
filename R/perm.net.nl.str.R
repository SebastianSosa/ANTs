#' @title Nodes labels permutation keeping network structure
#' @description Permute node metrics while keeping their dependency
#' @param df a data frame whith nodes informations
#' @param labels a numeric or string vector indicating the columns (labels) to permute.
#' @return a list of data frames of length nperm + 1 with the first element of the list beeing the original data frame and the other elements, the permuted ones.
#' @details Permute network links weigths while keep same network structure (density, modularity, binary global clustering coefficient).
#' @author Sebastian Sosa
#' @keywords internal

perm.net.nl.str.single <- function(df, labels){
  # Sample rows keeping the link between each column
  df[,labels] = df[sample(1:nrow(df)),labels]
  return(df)
}

#' @title Nodes labels permutation keeping network structure
#' @description Permute node metrics while keeping their dependency
#' @param df a data frame whith nodes informations
#' @param labels a numeric or string vector indicating the columns (labels) to permute.
#' @param rf an integer (column id) or a string (column name) indicating the column holding the factor grouping multiple networks.
#' @param nperm an integer indicating the number of permutations wanted.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @return a list of data frames of length nperm + 1 with the first element of the list beeing the original data frame and the other elements, the permuted ones.
#' @details Permute network links weigths while keep same network structure (density, modularity, binary global clustering coefficient).
#' @author Sebastian Sosa
#' @examples
#' df = met.strength(sim.m, df = sim.df, dfid = 1)
#' df = met.eigen(sim.m, df = df, dfid = 1)
#' head(df)
#' perm.net.nl.str(df, labels = c('strength', 'eigen'), nperm = 2)
perm.net.nl.str <- function(df, labels, rf=NULL, nperm, progress = TRUE){
  
  labels=df.col.findId(df, labels)
  result = rep(list(df), (nperm+1))
  
  if(!is.null(rf)){
    # find rf(s) id(s)
    rf = df.col.findId(df, rf)
    
    # If multiples rfs
    if(length(rf)>1){
      # Merge them to create a unique rf
      df$control_factor = df.ctrlFactor(df, rf)
    }
    else{
      df$control_factor = df[,rf]
    }
    
    # Split data frame according to each of them
    ldf = split(df, df$control_factor)
    
    if(progress){
      # For each permutations
      for (a in 2:(nperm+1)) {
        cat('Permutation:', a-1, '\r')
        # In each data frames obtain by spliting the original data frame accordign to the rf(s)
        for (b in 1:length(ldf)) {
          # Premute the rows
          ldf[[b]] = perm.net.nl.str.single(df = df, labels = labels)
        }
        cat("\n")
        
        # Merge result
        t = do.call('rbind', ldf)
        # Remove column of rf create to split the data frame
        result[[a]] = t[,-ncol(t)]
      }
    }
    else{
      # For each permutations
      for (a in 2:(nperm+1)) {
        # In each data frames obtain by spliting the original data frame accordign to the rf(s)
        for (b in 1:length(ldf)) {
          # Premute the rows
          ldf[[b]] = perm.net.nl.str.single(df = df, labels = labels)
        }

        # Merge result
        t = do.call('rbind', ldf)
        # Remove column of rf create to split the data frame
        result[[a]] = t[,-ncol(t)]
      }
    }
  }
  else{
    if(progress){
      # For each permutations
      for (a in 2:(nperm+1)) {
        cat('Permutation:', a-1, '\r')
        # Premute the rows
        result[[a]] = perm.net.nl.str.single(df = df, labels = labels)
      }
      cat("\n")
    }
    else{
      # For each permutations
      for (a in 2:(nperm+1)) {
        cat('Permutation:', a-1, '\r')
        # Premute the rows
        result[[a]] = perm.net.nl.str.single(df = df, labels = labels)
      }
    }
  }

  return(result)
}
