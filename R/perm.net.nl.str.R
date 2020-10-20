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
#' t = perm.net.nl.str(df, labels = c('strength', 'eigen'), nperm = 2)
perm.net.nl.str <- function(df, labels, rf=NULL, nperm, progress = TRUE){
  if(!is.null(rf)){
    if (is.data.frame(df) ) {
      stop("Argument df is incorrect. Node label permutations with random factors cannot be run on a single data frame. Argument ldf has to be a list of data frames for this type of permutation approach.")
    }
    if(!is.list(df)){
      stop("Argument df is not a list.")
      
    }
    
    test = unlist(lapply(df, is.data.frame))
    if(all(test) == TRUE){
      labels=df.col.findId(df[[1]], labels)

      if(progress){
        result = list()
        result[[1]] = do.call('rbind', df)
        # For each permutations
        for (a in 2:(nperm+1)) {
          cat('Permutation:', a-1, '\r')
          r = lapply(seq(df), function(i, x, labels){
            r = perm.net.nl.str.single(df = x[[i]], labels = labels)
          }, x = df, labels = labels)
          t = do.call('rbind', r)
          result[[a]] = t
          attr(result[[a]], "permutation") <- a-1
        }
      }else{
        result = list()
        result[[1]] = do.call('rbind', df)
        # For each permutations
        for (a in 2:(nperm+1)) {
          r = lapply(seq(df), function(i, x, labels){
            r = perm.net.nl.str.single(df = x[[i]], labels = labels)
          }, x = df, labels = labels)
          t = do.call('rbind', r)
          result[[a]] = t
        }
      }
    }else{stop("Argument df is not a list of data frames.")}


  }
  else{
    labels=df.col.findId(df, labels)
    result = rep(list(df), (nperm+1))
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
        # Premute the rows
        result[[a]] = perm.net.nl.str.single(df = df, labels = labels)
      }
    }
  }

  if(is.null(rf)){
    attr(result, "ANT") <- "ANT node label permutation without random factors and structure maintained"
    attr(result, "rf") <- rf
  }else{
    attr(result, "ANT") <- "ANT node label permutation with random factors and structure maintained"
    attr(result, "rf") <- NULL
  }
  attr(result, "labels") <- labels

  return(result)
}

#' @title Redo Nodes labels permutation keeping network structure
#' @description Permute node metrics while keeping their dependency. this function help to redo the permutations within stat. functions 
#' @keywords internal
redo.perm.net.nl.str <- function(df, labels, rf){
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
  
   result = perm.net.nl.str(ldf, labels = labels, rf = rf, nperm = 1, progress = FALSE)[[2]]
  return(result)
}