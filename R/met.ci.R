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

#' @title Centralisation index
#' @description Computes network Centralisation index
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @details Centralisation index of a network is based on eigenvector centrality.
#' @return
#' #' \itemize{
#' \item a double representing the centralisation index of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the centralisation index of the corresponding matrix of the list.
#' \item A list of arguments \emph{df} with a new column of network centralisation index if argument\emph{df} is not \emph{NULL} and if argument \emph{M} is a list of matrices. The name of the column is adapted according to arguments values \emph{binary} and \emph{sym}.
#' \item A list of arguments \emph{df} with a new column of network centralisation index if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Pasquaretta, C., Levé, M., Claidiere, N., Van De Waal, E., Whiten, A., MacIntosh, A. J., ... & Crofoot, M. C. (2014). Social networks in primates: smart and tolerant species have more efficient networks. Scientific reports, 4, 7600.
#' @examples
#' met.ci(sim.m)

met.ci <- function(M, df = NULL) {
  # Checking if argument M is a square matrix 
  test <- is.matrix(M)
  if (test) {
    # Compute network metric
    result <- met.ci.single(M)
    if (is.null(df)) {
      return(result)
    }
    else {
      # Adding network metric in argument df
      df$ci <- result
      return(df)
    }
  }
  else {
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
        if (is.null(df)) {
          # Compute network metric
          result <- lapply(M, function(x) {
            r <- met.ci.single(x)
            attr(r, "permutation") <- attributes(x)$permutation
            return(r)
          })
        }
        else {
          if (!is.data.frame(df)) {
            stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
          }
          # Compute network metric
          result <- lapply(M, function(x, df) {
            df$ci <- met.ci.single(x)
            attr(df, "permutation") <- attributes(x)$permutation
            return(df)
          }, df = df)
        }

        # If argument M is an object returned by perm.ds.grp, 
        # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
        # In case of future repermutations
        if (test1) {
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
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }

      # If argument M originates from a multiple network protocol, we work on a list of lists of matrices. M[i] being a list of permutations of a specific matrix.
      if (any(test4, test5, test6)) {
        # Check if argument df is NULL
        if (is.null(df)) {
          # Compute network metric
          result <- lapply(M, function(x) {
            r1 <- lapply(x, function(y) {
              r2 <- met.ci.single(y)
              attr(r2, "permutation") <- attributes(y)$permutation
              return(r2)
            })
            return(r1)
          })
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        # Check if argument df is not NULL
        else {
          # Check if argument df is a data frame
          if (is.data.frame(df)) {
            stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
          }
          # Check if each matrix size is equal to corresponding data frame size
          # Which means we are working on a case of multiple repermutations
          # Thus with a list of lists of matrices and data frames
          if (sum(unlist(lapply(seq_along(M), function(i, a) {
            nrow(a[[i]][[1]])
          }, a = M))) == nrow(df[[1]])) {
            tmp <- lapply(M, function(x) {
              r1 <- lapply(x, function(y) {
                r2 <- met.ci.single(y)
              })
            })

            # Merge vector
            tmp <- do.call(Map, c(c, tmp))

            # Adding network metric in argument df
            result <- lapply(seq_along(tmp), function(x, tmp, df) {
              df[[x]]$ci <- tmp[[x]]
              return(df[[x]])
            }, tmp = tmp, df = df)
          }
          else {
            # Merge list of data frames in a data frame
            ldf <- do.call("rbind", df)

            # Compute network metric
            tmp <- lapply(M, function(x) {
              r1 <- lapply(x, function(y) {
                r2 <- met.ci.single(y)
              })
            })

            # merge element one of each list together, merge element two of each list together, etc...
            tmp <- do.call(Map, c(c, tmp))

            result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
              ldf$ci <- tmp[[i]]
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp = tmp, ldf = ldf)
          }
        }

        # If argument M is an object returned by perm.ds.grp, 
        # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
        # In case of future repermutations
        if (test4) {
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
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }
      
      # If argument M originates from ANTs multiples matrices importations
      if(test7){
        # Check if argument df is NULL
        if (is.null(df)) {
          result <- lapply(M, met.ci.single)
          return(result)
        }
        
        # Check if argument df is not NULL, is not a data frame and is a list
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          # Compute network metric
          result <- mapply(function(x, y) {
            y$ci <- met.ci.single(x)
            return(y)
          }, x = M, y = df, SIMPLIFY = FALSE)
          return(result)
        }
      }
      
    }

  # If argument M is a list of square matrices----------------------
    else {
      if (!test & is.list(M)) {
        # Check if argument df is NULL
        if (is.null(df)) {
          result <- lapply(M, met.ci.single)
          return(result)
        }

        # Check if argument df is not NULL, is not a data frame and is a list
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          # Compute network metric
          result <- mapply(function(x, y) {
            y$ci <- met.ci.single(x)
            return(y)
          }, x = M, y = df, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
  }
}
