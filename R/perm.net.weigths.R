#' @title Network weigths permutation
#' @description Permute weigths while keep link structure
#' @param M a square matrix with colmun and row names
#' @param sym if \emph{TRUE}, it extracts the lower triangle of the matrix only.
#' @param nperm an integer indicating the number of permutations wanted.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @return list of square matrices of length nperm + 1. 
#' @keywords internal
#' @examples 
#' perm.net.weigths(M = sim.m, sym = FALSE, nperm = 10, progress = TRUE)

perm.net.weigths <- function(M, sym = FALSE, nperm = NULL, progress = TRUE){
  if(is.null(nperm)){stop("Argument nperm have to declare.")}
  require(ANTs)
  edg = mat.to.edgl(M = M, sym = sym, erase.diag = TRUE)
  edg = edg[edg$weight!=0,]# If one node is isolated it will be lost!
  
  # To translate in cpp
  result = perm_net_weigths(edg, sym, nperm, progress)
  cat("\n")
  return(result)
}

