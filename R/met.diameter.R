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

#' @title Diameter
#' @description Calculates the network diameter .

#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param weighted if \emph{FALSE}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the highest met.strength as the shortest path.
#' @param normalization normalizes the weights of the links i.e. divides them by the average strength of the network. Argument normalization can't be TRUE when argument weighted is FALSE.
#' @param directed if \emph{false}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers outgoing ties.
#' @return
#' \itemize{
#' \item a double representing the diameter of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the diameter of the corresponding matrix of the list.
#' \item A list of arguments \emph{df} with a new column of network diameter if argument\emph{df} is not \emph{NULL} and if argument \emph{M} is a list of matrices. The name of the column is adapted according to arguments values \emph{.weighted}, \emph{shortest.weight}, \emph{normalization}, \emph{directed} and \emph{out}.
#' \item A list of arguments \emph{df} with a new column of network diameter if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @details Diameter is the longer geodesic distance.
#' @author  Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Opsahl, T., Agneessens, F., & Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social networks, 32(3), 245-251.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.diameter(sim.m)

met.diameter <- function(M, df = NULL, weighted = TRUE, shortest.weight = FALSE, normalization = TRUE, directed = TRUE, out = TRUE) {
  # Checking if argument M is a square matrix 
  test <- is.matrix(M)
  if (test) {
    # Compute network metric
    result <- met.geodesicDiameter.single(M, weighted, shortest.weight, normalization, directed, out)[[1]]
    if (is.null(df)) {
      names(result) <- "Diameter"
      return(result)
    }
    else {
      # Adding network metric in argument df
      df$diameter <- result
      return(df)
    }
  }

  else {
    # Selecting the appropriate name of diameter selected by user
    tmp <- "tmp"
    if (weighted == FALSE) {
      if (normalization) {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.short.diameterB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.short.outdiameterB"
            }
            else {
              attr(tmp, "name") <- "norm.short.indiameterB"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.diameterB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.outdiameterB"
            }
            else {
              attr(tmp, "name") <- "norm.indiameterB"
            }
          }
        }
      }
      else {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "short.diameterB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "short.outdiameterB"
            }
            else {
              attr(tmp, "name") <- "short.indiameterB"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "diameterB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "outdiameterB"
            }
            else {
              attr(tmp, "name") <- "indiameterB"
            }
          }
        }
      }
    }
    else {
      if (normalization) {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.short.diameter"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.short.outdiameter"
            }
            else {
              attr(tmp, "name") <- "norm.short.indiameter"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.diameter"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.outdiameter"
            }
            else {
              attr(tmp, "name") <- "norm.indiameter"
            }
          }
        }
      }
      else {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "short.diameter"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "short.outdiameter"
            }
            else {
              attr(tmp, "name") <- "short.indiameter"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "diameter"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "outdiameter"
            }
            else {
              attr(tmp, "name") <- "indiameter"
            }
          }
        }
      }
    }

    # Check if argument M is an object returned by perm.ds.grp, perm.ds.focal or perm.net.nl----------------------
    # This part was created to handle repermutation in functions stat.lm, stat.glm and stat.glmm
    if (!is.null(attributes(M)$ANT)) {
      # Check if argument M originates from a single network protocol
      test1 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
      test2 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
      test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

      # Check if argument M originates from a multiple network protocol
      test4 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
      test5 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
      test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"
      
      # Check if argument M originates from ANTs multiples matrices importations
      test7 <- attributes(M)$ANT == "list of matrices obtained through data frames of interactions"

      # If argument M originates from a single network protocol, we work on a list of matrices
      if (any(test1, test2, test3)) {
        # Check if argument df is not NULL
        if (!is.null(df)) {
          # Check if argument df is a data frame
          if (!is.data.frame(df)) {
            stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
          }
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out, df, tmp) {
            df$ge <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
            names(df)[ncol(df)] = attributes(tmp)$name
            attr(df, "permutation") <- attributes(x)$permutation
            return(df)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out, df = df,  tmp = tmp)
        } 
        else{
          # Compute network metric and keep attribute permutations 
          # and name column of data frame according to user arguments binary,sym, out declaration
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out, df, tmp) {
            r<- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
            names(r) = attributes(tmp)$name
            attr(r, "permutation") <- attributes(x)$permutation
            return(r)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out, df = df,  tmp = tmp)
        }

        

        # If argument M is an object returned by perm.ds.grp, 
        # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
        # In case of future repermutations
        if (test1) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "focal") <- attributes(M)$focal
          attr(result, "ctrl") <- attributes(M)$ctrl
          attr(result, "alters") <- attributes(M)$alters
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        # If argument M is an object returned by perm.ds.focal, 
        # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
        # In case of future repermutations
        if (test2) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        # If argument M is an object returned by perm.net.nl, 
        # Store argument M attributes 'ANT'
        # In case of future repermutations
        if (test3) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }

      # If argument M originates from a multiple network protocol, we work on a list of lists of matrices. M[i] being a list of permutations of a specific matrix.
      if (any(test4, test5, test6)) {
        # Check if argument df is NULL
        if (is.null(df)) {
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r1 <- lapply(x, function(y, weighted, shortest.weight, normalization, directed, out) {
              r2 <- met.geodesicDiameter.single(y, weighted, shortest.weight, normalization, directed, out)[[1]]
              attr(r2, "permutation") <- attributes(y)$permutation
              return(r2)
            }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            return(r1)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        }
        # Check if argument df is not NULL
        else {
          if (!is.null(df) & is.data.frame(df)) {
            stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
          }
          # Check if each matrix size is equal to the corresponding data frame size
          # Which means, we are working on a case of multiple repermutations
          # Thus with a list of lists of matrices and data frames
          if (sum(unlist(lapply(seq_along(M), function(i, a) {
            nrow(a[[i]][[1]])
          }, a = M))) == nrow(df[[1]]))  {
            tmp2 <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
              r1 <- lapply(x, function(y, weighted, shortest.weight, normalization, directed, out) {
                r2 <- met.geodesicDiameter.single(y, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
              }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)

            tmp2 <- do.call(Map, c(c, tmp2))

            # Paste each of these new vectors in a list of data frames
            # and name column of data frame according to user arguments binary,sym, out declaration
            result <- lapply(seq_along(tmp2), function(x, tmp2, df, tmp) {
              df[[x]]$diameter <- tmp[[x]]
              colnames(df[[x]])[ncol(df[[x]])] <- attributes(tmp)$name
              return(df[[x]])
            }, tmp2 = tmp2, df = df, tmp = tmp)
          }
          # Else, we are working on a case of single repermutation with a list of matrices and data frames
          else {
            # data frame manipulation
            ldf <- do.call("rbind", df)

            tmp2 <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
              r1 <- lapply(x, function(y, weighted, shortest.weight, normalization, directed, out) {
                r2 <- met.geodesicDiameter.single(y, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
              }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)

            tmp2 <- do.call(Map, c(c, tmp2))

            # Name column of data frame according to user arguments binary,sym, out declaration
             result <- lapply(seq_along(tmp2), function(tmp2, tmp, ldf, i) {
              ldf$diamter <- tmp[[i]]
              names(ldf)[ncol(ldf)] = attributes(tmp)$name
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp2 = tmp2, tmp = tmp, ldf = ldf)

            # If argument M is an object returned by perm.ds.grp, 
            # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
            # In case of future repermutations
        if (test4) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "focal") <- attributes(M)$focal
          attr(result, "ctrl") <- attributes(M)$ctrl
          attr(result, "alters") <- attributes(M)$alters
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
  
            # If argument M is an object returned by perm.ds.focal, 
            # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
            # In case of future repermutations
        if (test5) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

            # If argument M is an object returned by perm.net.nl, 
            # Store argument M attributes 'ANT'
            # In case of future repermutations
        if (test6) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
          }
        }
      }
      
      # If argument M originates from ANTs multiples matrices importations
      if(test7){
        # Check if argument df is NULL
        if (is.null(df)) {
          # Compute network metric
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
          attr(result, "name") <- attributes(tmp)$name
          return(result)
        }
        
        # Check if argument df is not NULL, is not a data frame and is a list
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          
          result <- mapply(function(x, y, t) {
            y$diameter <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
            colnames(y)[ncol(y)] <- t
            return(y)
          }, x = M, y = df, t = attributes(tmp)$name, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
    # If argument M is a list of square matrices----------------------        
    else {
      if (!test & is.list(M)) {
        # Check if argument df is NULL
        if (is.null(df)) {
          # Compute network metric
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
          attr(result, "name") <- attributes(tmp)$name
          return(result)
        }
        
        # Check if argument df is not NULL, is not a data frame and is a list
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          
          result <- mapply(function(x, y, t) {
            y$diameter <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
            colnames(y)[ncol(y)] <- t
            return(y)
          }, x = M, y = df, t = attributes(tmp)$name, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
    
  }
}
