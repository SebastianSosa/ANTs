# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
#
# ANT is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Eigenvector Centrality
#' @description Calculates the node metric met.evcent centrality for all vertices.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param dfid an integer or a string indicating the column with individual ids in argument \emph{df}.
#' @param sym if \emph{TRUE}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param binary a boolean, if \emph{TRUE}, it calculates the binary version of the eigenvector centrality.
#' @param out if \emph{TRUE}, it considers outgoing ties to compute the shortest paths.
#' @return
#' \itemize{
#' \item An integer vector of nodes \emph{eigenvector centrality} if argument \emph{df} is \emph{NULL}.
#' \item A list of integer vectors of nodes \emph{eigenvector centrality} if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{eigenvector centrality} if argument\emph{df} is not \emph{NULL}. The name of the column is adapted according to arguments value \emph{binary}, \emph{sym} and \emph{out}.
#' \item A list of arguments df with a new column for nodes \emph{eigenvector centrality} if 1) argument \emph{df} is not \emph{NULL}, 2) argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and 3) argument \emph{df} is a list of data frames of same length as argument \emph{M}.The name of the column of each element of the list is adapted according to argument value \emph{binary}.
#' }

#' @details Eigenvector centrality is the first non-negative met.evcent value obtained through the linear transformation of an adjacency matrix. This centrality measure quantifies not only a node connectedness, but also the connections of the nodes to whom it is connected. Thus, a node can have a high met.evcent value by having a high met.degree or met.strength, or by being connected to nodes that have high degrees or strengths.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Bonacich, P. (1972). Factoring and weighting approaches to status scores and clique identification. Journal of mathematical sociology, 2(1), 113-120.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.eigen(sim.m)
#' head(sim.df)
#' met.eigen(sim.m,df=sim.df)

met.eigen <- function(M, df = NULL, dfid = NULL, sym = TRUE, binary = FALSE, out = FALSE) {
  # Check if argument M is a square matrix or a list of square matrices----------------------
 test <-  check.mat(M)

  # If argument M is a square Matrix----------------------
  if (test=="M ok") {
    # If argument df is NULL return a simple numeric vector
    if (is.null(df)) {
      # Compute network metric
      result <- met.eigen.single(M, df = df, dfid = dfid, sym = sym, binary = binary, out = out)
      return(result)
    }
     # If argument df is not NULL return a data frame
    else {
      #If argument dfid is NULL simply add the network metric vector into the data frame,
      # else order data frame according matrix column order 
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      # Compute network metric
      result <- met.eigen.single(M, df = df, dfid = dfid, sym = sym, binary = binary, out = out)
      return(result)
    }
  }

  # Check if argument M is an object returned by perm.ds.grp, perm.ds.focal or perm.net.nl----------------------
  # This part was created to handle repermutation in functions stat.lm, stat.glm and stat.glmm
  if (!is.null(attributes(M)$ANT)) {
    # Check if argument M originates from a single network protocol
    test1 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
    test2 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
    test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

    # Check if argument M is issue from a multiples network protocol
    test4 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
    test5 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
    test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"

    # If argumpent M originates from a single network protocol, we work on a list of matrices
    if (any(test1, test2, test3)) {
      # Check if argument df is not NULL
      if (!is.null(df)) {
        # Check if argument df is a data frame
        if (!is.data.frame(df)) {
          stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
        }
      }

      # Check if argument dfid is NULL
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }

      # Compute network metric and keep attribute permutations
      result <- lapply(M, function(x, df, dfid, sym, binary, out) {
        r <- met.eigen.single(x, df = df, dfid = dfid, sym = sym, binary = binary, out = out)
        attr(r, "permutation") <- attributes(x)$permutation
        return(r)
      }, df = df, dfid = dfid, sym = sym, binary = binary, out = out)

      # If argument M is an object returned by  perm.ds.grp, 
      # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
      # In case of future repermutations
      if (test1) {
        attr(result, "scan") <- attributes(M)$scan
        attr(result, "ctrlf") <- attributes(M)$ctrlf
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        cat("\n")
        return(result)
      }

      # If argument M is an object returned by perm.ds.focal, 
      # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
      # In case of future repermutations
      if (test2) {
        attr(result, "focal") <- attributes(M)$focal
        attr(result, "ctrl") <- attributes(M)$ctrl
        attr(result, "alters") <- attributes(M)$alters
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        cat("\n")
        return(result)
      }

      # If argument M is an object returned by perm.net.nl, 
      # Store argument M attributes 'ANT'
      # In case of future repermutations
      if (test3) {
        attr(result, "ANT") <- attributes(M)$ANT
        cat("\n")
        return(result)
      }
    }

    # If argument M originates from a multiple network protocol, we work on a list of lists of matrices. M[i] being a list of permutations of a specific matrix.
    if (any(test4, test5, test6)) {
      # Check if argument df is NULL
      if (is.null(df)) {
        # Compute network metric
        result <- lapply(M, function(x, sym, binary, out) {
            r1 <- lapply(x, function(y, sym = sym, binary = binary, out = out) {
              r2 <- met.eigen.single(y, sym = sym, binary = binary, out = out)
              attr(r2, "permutation") <- attributes(y)$permutation
              return(r2)
            }, sym = sym, binary = binary, out = out)
          }, sym = sym, binary = binary, out = out)
      }
      # Check if argument df is not NULL
      else {
        # Check if argument df is a data frame
        if (is.data.frame(df)) {
          stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
        }
        # Check if argument dfid is NULL
        if (is.null(dfid)) {
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        }
        # Check if each matrix size is equal to corresponding data frame size
        # Which means, we are working on a case of multiple repermutations
        # Thus with a list of lists of matrices and data frames
        if (sum(unlist(lapply(seq_along(M), function(i, a) {
          nrow(a[[i]][[1]])
        }, a = M))) == nrow(df[[1]])) {

          # Compute network metric
          tmp <- lapply(M, function(x, sym, binary, out) {
            r1 <- lapply(x, function(y, sym, binary, out) {
              r2 <- met.eigen.single(y, sym = sym, binary = binary, out = out)
            }, sym = sym, binary = binary, out = out)
          }, sym = sym, binary = binary, out = out)
          tmp <- do.call(Map, c(c, tmp))

            if (binary) {
              if (sym) {
                result <- lapply(seq_along(df), function(i, a, b) {
                  a[[i]]$eigenB <- b[[i]]
                  return(a[[i]])
                }, a = df, b = tmp)
              }
              else {
                if (out) {
                  result <- lapply(seq_along(df), function(i, a, b) {
                    a[[i]]$outeigenB <- b[[i]]
                    return(a[[i]])
                  }, a = df, b = tmp)
                }
                else {
                  result <- lapply(seq_along(df), function(i, a, b) {
                    a[[i]]$ineigenB <- b[[i]]
                    return(a[[i]])
                  }, a = df, b = tmp)
                }
              }
            }
            else {
              if (sym) {
                result <- lapply(seq_along(df), function(i, a, b) {
                  a[[i]]$eigen <- b[[i]]
                  return(a[[i]])
                }, a = df, b = tmp)
              }
              else {
                if (out) {
                  result <- lapply(seq_along(df), function(i, a, b) {
                    a[[i]]$outeigen <- b[[i]]
                    return(a[[i]])
                  }, a = df, b = tmp)
                }
                else {
                  result <- lapply(seq_along(df), function(i, a, b) {
                    a[[i]]$ineigen <- b[[i]]
                    return(a[[i]])
                  }, a = df, b = tmp)
                }
              }
            }
        }

        # Else, we are working on a case of single repermutation with a list of matrices and data frames
        else {
          # Check if argument dfid is not NULL, then order data frame
          if (!is.null(dfid)) {
            dfid <- df.col.findId(df[[1]], dfid)
            df <- lapply(df, function(x) {
              x <- x[order(x[[dfid]]), ]
            })
          }
          # Else warning
          else {
            warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
          }
          # Merge list of data frames
          ldf <- do.call("rbind", df)

          # Compute network metric
          tmp <- lapply(M, function(x, sym, binary, out) {
              r1 <- lapply(x, function(y, sym, binary, out) {
                r2 <- met.eigen.single(y, sym = sym, binary = binary, out = out)
              }, sym = sym, binary = binary, out = out)
            }, sym = sym, binary = binary, out = out)
          tmp <- do.call(Map, c(c, tmp))

          # Name column of data frame according user arguments binary,sym, out declaration
          if (binary) {
              if (sym) {
                result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
                  ldf$eigenB <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
              else {
                if (out) {
                  result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
                    ldf$outeigenB <- tmp[[i]]
                    attr(ldf, "permutation") <- i
                    return(ldf)
                  }, tmp = tmp, ldf = ldf)
                }
                else {
                  result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
                    ldf$ineigenB <- tmp[[i]]
                    attr(ldf, "permutation") <- i
                    return(ldf)
                  }, tmp = tmp, ldf = ldf)
                }
              }
          }
          else {
            if (sym) {
              result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
                ldf$eigen <- tmp[[i]]
                attr(ldf, "permutation") <- i
                return(ldf)
              }, tmp = tmp, ldf = ldf)
            }
            else {
              if (out) {
                result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
                  ldf$outeigen <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
              else {
                result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
                  ldf$ineigen <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
            }
          }
        }
      }

      # If argument M is an object returned by  perm.ds.grp, 
      # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
      # In case of future repermutations
      if (test4) {
        attr(result, "scan") <- attributes(M)$scan
        attr(result, "ctrlf") <- attributes(M)$ctrlf
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }

      # If argument M is an object returned by perm.ds.focal, 
      # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
      # In case of future repermutations
      if (test5) {
        attr(result, "focal") <- attributes(M)$focal
        attr(result, "ctrl") <- attributes(M)$ctrl
        attr(result, "alters") <- attributes(M)$alters
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }

      # If argument M is an object returned by perm.net.nl, 
      # Store argument M attributes 'ANT'
      # In case of future repermutations
      if (test6) {
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
    }  

  # If argument M is a list of square matrices----------------------
  if (test =="M list ok")  {
    # Check if argument df is NULL and not argument dfid
    if (is.null(df) & !is.null(dfid)) {
      stop("Argument 'df' can't be NULL when argument 'dfid' isn't", "\r")
    }

    # Check if argument df and dfid are NULL
    if (is.null(df) & is.null(dfid)) {
      result <- lapply(M, met.eigen.single, df, dfid, sym, binary, out)
      return(result)
    }

    # Check if argument df is not NULL, is not a data frame and is a list
    if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
      # Check if argument dfid is not NULL
      if (!is.null(dfid)) {
        # Compute network metric
        result <- mapply(met.eigen.single, M, df = df, dfid = dfid, sym = sym, binary = binary, out = out, SIMPLIFY = FALSE)
        return(result)
      }
      # Check if argument dfid is NULL and print warning
      else {
        # Compute network metric
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        result <- mapply(met.eigen.single, M, df = df, sym = sym, binary = binary, out = out, SIMPLIFY = FALSE)
        return(result)
      }
    }
  }
  }
  
  # If argument M is a list of square matrices----------------------
 if (test =="M list ok") {
   # Check if argument df is NULL and not argument dfid
   if (is.null(df) & !is.null(dfid)) {
     stop("Argument 'df' can't be NULL when argument 'dfid' isn't", "\r")
   }
   
   # Check if argument df and dfid are NULL
   if (is.null(df) & is.null(dfid)) {
     result <- lapply(M, met.eigen.single, df = df, dfid = dfid, sym = sym, binary = binary, out = out)
     return(result)
   }
   
   # Check if argument df is not NULL, is not a data frame and is a list
   if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
     # Check if argument dfid is not NULL
     if (!is.null(dfid)) {
       # Compute network metric
       result <- mapply(function(M,  df, dfid, sym , binary, out) {
         r <- met.eigen.single(M = M, df = df, dfid = dfid, sym = sym, binary = binary, out = out)
         return(r)
       }, M = M, df = df, dfid = dfid, sym = sym, binary = binary, out = out, SIMPLIFY = FALSE)
       return(result)
     }
     else {
       # Compute network metric
       warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
       # Compute network metric
       result <- mapply(function(M,  df,  sym , binary, out) {
         r <- met.eigen.single(M = M, df = df,  sym = sym, binary = binary, out = out)
         return(r)
       }, M = M, df = df, sym = sym, binary = binary, out = out, SIMPLIFY = FALSE)
       return(result)
     }
   }
 }
}
