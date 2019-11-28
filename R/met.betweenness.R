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

#' @title Betweenness centrality
#' @description Computes node betweenness centrality of all nodes of the network.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param dfid an integer or a string indicating the column with individual ids in argument \emph{df}.
#' @param binary ia boolean, if \emph{TRUE}, it calculates the binary version of the betweenness centrality.
#' @param shortest.weight if \emph{FALSE}, it considers the highest \emph{strength} as the shortest path.
#' @param normalization normalizes the weigths of the links i.e. divides them by the average strength of the network. Argument normalization can't be \emph{TRUE} when argument binary is \emph{FALSE}.
#' @param sym if \emph{TRUE}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{TRUE}, it considers outgoing ties to compute shortest paths.
#' @return
#' \itemize{
#' \item An integer vector of nodes \emph{betweenness} if argument \emph{df} is \emph{NULL}.
#' \item A list of integer vectors of nodes \emph{betweenness} if argument \emph{M} is a list and if argument \emph{df} is \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{betweenness} if argument \emph{df} is not \emph{NULL}. The name of the column is adapted according to arguments \emph{binary}, \emph{shortest.weight}, \emph{normalization}, \emph{sym} and \emph{out}.
#' \item A list of arguments df with a new column for nodes \emph{betweenness} if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @details Betweenness is the number of times a node is included in the shortest paths (geodesic distances) between all the potential combinations of edges of the other nodes. As it directly derives from the geodesic distance, it is important to pay attention to how the investigator intends to calculate geodesic distances (binary or weighted, directed or undirected, and using the lowest or the highest strength as the shortest path). Betweenness provides a specific centrality measure insofar that it informs on the role of a node in the transmission of information as nodes with high betweenness are likely to constitute bridges that connect subgroups.
#' @author Hu Feng He, Sebastian Sosa, Ivan Puga-Gonzalez, Xiaohua Xie.
#' @references Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social networks, 1(3), 215-239.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.betweenness(sim.m)
#' head(sim.df)
#' met.betweenness(sim.m,df=sim.df)
met.betweenness <- function(M, binary = FALSE, shortest.weight = FALSE, normalization = TRUE, sym = FALSE, out = TRUE, df = NULL, dfid = NULL) {
  # Check if argument M is a square matrix or a list of square matrices----------------------
  test <- check.mat(M)

  # If argument M is a square Matrix----------------------
  if (test=="M ok") {
    # If argument df is NULL return a simple numeric vector
    if (is.null(df)) {
      # Compute network metric
      result <- met.betweenness.single(M, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid)
      return(result)
    }

    # If argument df is not NULL return a data frame
    else {
      #If argument dfid is NULL simply add the network metric vector into the data frame,
      # else order data frame according matrix column order 
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }

      # Creat an object to store the type of betweenness user selected
      # According to binary, shortest.weight, normalization, arguments sym, out
      tmp <- "tmp"
      if (binary) {
        if (normalization) {
          if (shortest.weight) {
            if (sym) {
              attr(tmp, "name") <- "norm.short.betweennessB"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "norm.short.outbetweennessB"
              }
              else {
                attr(tmp, "name") <- "norm.short.inbetweennessB"
              }
            }
          }
          else {
            if (sym) {
              attr(tmp, "name") <- "norm.betweennessB"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "norm.outbetweennessB"
              }
              else {
                attr(tmp, "name") <- "norm.inbetweennessB"
              }
            }
          }
        }
        else {
          if (shortest.weight) {
            if (sym) {
              attr(tmp, "name") <- "short.betweennessB"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "short.outbetweennessB"
              }
              else {
                attr(tmp, "name") <- "short.inbetweennessB"
              }
            }
          }
          else {
            if (sym) {
              attr(tmp, "name") <- "betweennessB"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "outbetweennessB"
              }
              else {
                attr(tmp, "name") <- "inbetweennessB"
              }
            }
          }
        }
      }
      else {
        if (normalization) {
          if (shortest.weight) {
            if (sym) {
              attr(tmp, "name") <- "norm.short.betweenness"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "norm.short.outbetweenness"
              }
              else {
                attr(tmp, "name") <- "norm.short.inbetweenness"
              }
            }
          }
          else {
            if (sym) {
              attr(tmp, "name") <- "norm.betweenness"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "norm.outbetweenness"
              }
              else {
                attr(tmp, "name") <- "norm.inbetweenness"
              }
            }
          }
        }
        else {
          if (shortest.weight) {
            if (sym) {
              attr(tmp, "name") <- "short.betweenness"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "short.outbetweenness"
              }
              else {
                attr(tmp, "name") <- "short.inbetweenness"
              }
            }
          }
          else {
            if (sym) {
              attr(tmp, "name") <- "betweenness"
            }
            else {
              if (out) {
                attr(tmp, "name") <- "outbetweenness"
              }
              else {
                attr(tmp, "name") <- "inbetweenness"
              }
            }
          }
        }
      }

      # Compute network metric
      result <- met.betweenness.single(M, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid)
      colnames(result)[ncol(result)] <- attributes(tmp)$name
      return(result)
    }
  }

  else {
    # Creat an object to store the type of betweenness user selected
    # According to binary, shortest.weight, normalization, arguments sym, out-------------------
    tmp <- "tmp"
    if (binary) {
      if (normalization) {
        if (shortest.weight) {
          if (sym) {
            attr(tmp, "name") <- "norm.short.betweennessB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.short.outbetweennessB"
            }
            else {
              attr(tmp, "name") <- "norm.short.inbetweennessB"
            }
          }
        }
        else {
          if (sym) {
            attr(tmp, "name") <- "norm.betweennessB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.outbetweennessB"
            }
            else {
              attr(tmp, "name") <- "norm.inbetweennessB"
            }
          }
        }
      }
      else {
        if (shortest.weight) {
          if (sym) {
            attr(tmp, "name") <- "short.betweennessB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "short.outbetweennessB"
            }
            else {
              attr(tmp, "name") <- "short.inbetweennessB"
            }
          }
        }
        else {
          if (sym) {
            attr(tmp, "name") <- "betweennessB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "outbetweennessB"
            }
            else {
              attr(tmp, "name") <- "inbetweennessB"
            }
          }
        }
      }
    }
    else {
      if (normalization) {
        if (shortest.weight) {
          if (sym) {
            attr(tmp, "name") <- "norm.short.betweenness"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.short.outbetweenness"
            }
            else {
              attr(tmp, "name") <- "norm.short.inbetweenness"
            }
          }
        }
        else {
          if (sym) {
            attr(tmp, "name") <- "norm.betweenness"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.outbetweenness"
            }
            else {
              attr(tmp, "name") <- "norm.inbetweenness"
            }
          }
        }
      }
      else {
        if (shortest.weight) {
          if (sym) {
            attr(tmp, "name") <- "short.betweenness"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "short.outbetweenness"
            }
            else {
              attr(tmp, "name") <- "short.inbetweenness"
            }
          }
        }
        else {
          if (sym) {
            attr(tmp, "name") <- "betweenness"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "outbetweenness"
            }
            else {
              attr(tmp, "name") <- "inbetweenness"
            }
          }
        }
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
        result <- lapply(M, function(x, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid, tmp) {
          r <- met.betweenness.single(x, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid)
          colnames(r)[ncol(r)] <- attributes(tmp)$name
          attr(r, "permutation") <- attributes(x)$permutation
          return(r)
        }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid, tmp = tmp)

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
          result <- lapply(M, function(x, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df) {
            r1 <- lapply(x, function(y, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df) {
              r2 <- met.betweenness.single(y, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df)
              attr(r2, "permutation") <- attributes(y)$permutation
              return(r2)
            }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df)
          }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df)
          return(result)
        }
        # Check if argument df is not NULL
        else {
          # Check if argument df is a data frame
          if (!is.null(df) & is.data.frame(df)) {
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
            tmp2 <- lapply(M, function(x, binary, shortest.weight, normalization, sym, out) {
              r1 <- lapply(x, function(y, binary, shortest.weight, normalization, sym, out) {
                r2 <- met.betweenness.single(y, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out)
              }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out)
            }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out)

            tmp2 <- do.call(Map, c(c, tmp2))

            # Name column of data frame according user arguments binary, shortest.weight, normalization, arguments sym, out declaration          
            result <- lapply(seq_along(df), function(i, a, b, c) {
              a[, (ncol(a) + 1)] <- b[[i]]
              colnames(a)[, ncol(a)] <- attributes(c)$name
              return(a)
            }, a = df, b = tmp2, c = tmp)
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
            tmp2 <- lapply(M, function(x, binary = binary, shortest.weight, normalization, sym, out, tmp) {
              r1 <- lapply(x, function(y, binary, shortest.weight, normalization, sym, out) {
                r2 <- met.betweenness.single(y, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out)
              }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, tmp = tmp)
            }, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, tmp = tmp)

            tmp2 <- do.call(Map, c(c, tmp2))

            # Name column of data frame according user argument binary declaration
            result <- lapply(seq_along(df), function(i, a, b, c) {
              a[, (ncol(a) + 1)] <- b[[i]]
              colnames(a)[, ncol(a)] <- attributes(c)$name
              return(a)
            }, a = df, b = tmp2, c = tmp)

            result <- lapply(seq_along(tmp2), function(i, tmp2, ldf, tmp) {
              ldf <- tmp2[[i]]
              colnames(ldf)[ncol(ldf)] <- attributes(tmp)$name
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp2 = tmp2, ldf = ldf, tmp = tmp)
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
          cat("\n")
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
          cat("\n")
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
    }

    if (test =="M list ok") {
      # Check if argument df is NULL and not argument dfid
      if (is.null(df) & !is.null(dfid)) {
        stop("Argument 'df' can't be NULL when argument 'dfid' isn't", "\r")
      }

      # Check if argument df and dfid are NULL
      if (is.null(df) & is.null(dfid)) {
        result <- lapply(M, met.betweenness.single, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid)
        return(result)
      }

      # Check if argument df is not NULL, is not a data frame and is a list
      if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
        n=rep(list(attributes(tmp)$name),length(M))
        # Check if argument dfid is not NULL
        if (!is.null(dfid)) {
          # Compute network metric
          result <- mapply(function(M, binary, shortest.weight, normalization, sym, out, df, dfid,tmp1) {
            r <- met.betweenness.single(m = M, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid)
            colnames(r)[ncol(r)] <- tmp1
            return(r)
          }, M = M, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, dfid = dfid, tmp1=n, SIMPLIFY = FALSE)
          return(result)
        }
        else {
          # Compute network metric
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
          result <- mapply(function(M, binary, shortest.weight, normalization, sym, out, df,tmp1) {
            r <- met.betweenness.single(m = M, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df)
            colnames(r)[ncol(r)] <- colnames(r)[ncol(r)] <- tmp1
            return(r)
          }, M = M, binary = binary, shortest.weight = shortest.weight, normalization = normalization, sym = sym, out = out, df = df, tmp1=n, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
  }
}
