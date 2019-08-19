#' @title Links weigths permutations
#' @description Permute network links weigths while keep same network structure.
#' @param M a sqara numeric matrix
#' @param nperm an integer indicating the number of permutations wanted.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @return a list of matrices of length nperm + 1 with the first matrix beeing the original one and the other elements, the permuted ones.
#' @details Permute network links weigths while keep same network structure (density, modularity, binary global clustering coefficient).
#' @author Sebastian Sosa
#' @examples
#' test = perm.net.lk.w(sim.m, nperm = 2)
#' test[[1]][test[[1]] > 0] == test[[2]][test[[2]] > 0]
perm.net.lk.w <- function(m, nperm, progress = TRUE){
  # Declaring result list object
  result = rep(list(m), nperm+1)
  if(progress){
    # for each permutations
    result = lapply(seq(result), function(i,x){
      cat('Permutation:', i, '\r')
      # Sample non zero matrix cell
      x[[i]][x[[i]]>0]=sample(x[[i]][x[[i]]>0])
      return(x[[i]])
    }, x = result)
  }
  else{
    # for each permutations
    result = lapply(seq(result), function(i,x){
      # Sample non zero matrix cell
      x[[i]][x[[i]]>0]=sample(x[[i]][x[[i]]>0])
      return(x[[i]])
    }, x = result)
  }
  result[[1]] = m
  return(result)
}
