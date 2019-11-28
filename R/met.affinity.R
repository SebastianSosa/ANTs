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

#' @title Affinity
#' @description Calculates the node metric \emph{affinity} for all the nodes of the network
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param dfid an integer or a string indicating the column with individual ids in argument \emph{df}.
#' @param binary a boolean, if \emph{TRUE}, it calculates the binary version of the affinity.
#' @return
#' \itemize{
#' \item An integer vector of nodes \emph{affinity} if argument \emph{df} is \emph{NULL}.
#' \item A list of integer vectors of nodes \emph{affinity} if argument \emph{M} is a list of matrices and  if argument \emph{df} is \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{affinity} if argument\emph{df} is not \emph{NULL}. The name of the column is adapted according to argument value \emph{binary}.
#' \item A list of arguments df with a new column for nodes \emph{affinity} if 1) argument \emph{df} is not \emph{NULL}, 2) argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and 3) argument \emph{df} is a list of data frames of same length as argument \emph{M}.The names of the column of each element of the list is adapted according to argument value \emph{binary}.
#' }
#' @details  Affinity is a second-order metric that evaluates how alters of node i are connected.
#' The binary version is the average degree of alters of node i.
#' The weighted version is the ratio between the metric reach and the strength of node i. A high affinity reveals that nodes tend to be connected to alters with high degrees or strengths.
#' Thus, this metric informs on node assortativity by vertex met.degree, i.e. connections between nodes with similar degrees or strengths.
#'
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.affinity(sim.m)
#' head(sim.df)
#' met.affinity(sim.m,df=sim.df)

met.affinity <- function(M, df = NULL, dfid = NULL, binary = FALSE) {
  # Check if argument M is a square matrix or a list of square matrices----------------------
  test <- check.mat(M)

  # If argument M is a square Matrix----------------------
  if (test=="M ok") {
    # If argument df is NULL return a simple numeric vector
    if (is.null(df)) {
      # Compute network metric
      result <- met.affinity.single(M, df = df, dfid = dfid, binary = binary)
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
      result <- met.affinity.single(M, df = df, dfid = dfid, binary = binary)
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
      result <- lapply(M, function(x, df, dfid, binary) {
        r <- met.affinity.single(x, df = df, dfid = dfid, binary = binary)
        attr(r, "permutation") <- attributes(x)$permutation
        return(r)
      }, df = df, dfid = dfid, binary = binary)

      # If argument M is an object returned by  perm.ds.grp, 
      # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
      # In case of future repermutations
      if (test1) {
        attr(result, "scan") <- attributes(M)$scan
        attr(result, "ctrlf") <- attributes(M)$ctrlf
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
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
        result <- lapply(M, function(x, binary) {
          r1 <- lapply(x, function(y, binary = binary) {
            r2 <- met.affinity.single(y, binary = binary)
            attr(r2, "permutation") <- attributes(y)$permutation
            return(r2)
          }, binary = binary)
        }, binary)
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
          tmp <- lapply(M, function(x, binary) {
            r1 <- lapply(x, function(y, binary = binary) {
              r2 <- met.affinity.single(y, binary = binary)
            }, binary = binary)
          }, binary = binary)
          tmp <- do.call(Map, c(c, tmp))

          # Name column of data frame according user argument binary declaration
          if (binary) {
            result <- lapply(seq_along(df), function(i, a, b) {
              a[[i]]$affinityB <- b[[i]]
              return(a[[i]])
            }, a = df, b = tmp)
          }

          # Name column of data frame according user argument binary declaration
          else {
            result <- lapply(seq_along(df), function(i, a, b) {
              a[[i]]$affinity <- b[[i]]
              return(a[[i]])
            }, a = df, b = tmp)
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
          tmp <- lapply(M, function(x, binary) {
            r1 <- lapply(x, function(y, binary = binary) {
              r2 <- met.affinity.single(y, binary = binary)
            }, binary = binary)
          }, binary = binary)
          tmp <- do.call(Map, c(c, tmp))

          # Name column of data frame according user argument binary declaration
          if (binary) {
            result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
              ldf$affinityB <- tmp[[i]]
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp = tmp, ldf = ldf)
          }

          # Name column of data frame according user argument binary declaration
          else {
            result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
              ldf$affinity <- tmp[[i]]
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp = tmp, ldf = ldf)
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
  }

  # If argument M is a list of square matrices----------------------
  if (test =="M list ok") {
    # Check if argument df is NULL and not argument dfid
    if (is.null(df) & !is.null(dfid)) {
      stop("Argument 'df' can't be NULL when argument 'dfid' isn't", "\r")
    }

    # Check if argument df and dfid are NULL
    if (is.null(df) & is.null(dfid)) {
      result <- lapply(M, met.affinity.single, df = df, dfid = dfid, binary = binary)
      return(result)
    }

    # Check if argument df is not NULL, is not a data frame and is a list
    if (!is.null(df)) {
      # Argument df is a data frame
      if(check.df(df)=="df ok"){stop("Argument df is a data frame. Argument df must a list of data frames when argument M is a list of matrices.")}
      
      # Argument df is a list of data frames
      else{
        # Check if argument dfid is not NULL
        if (!is.null(dfid)){
          # Compute network metric
          result <- mapply(met.affinity.single, M, df = df, dfid = dfid, binary = binary, SIMPLIFY = FALSE)
          return(result)
        }
        # Check if argument dfid is NULL and print warning
        else {
          # Compute network metric
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
          result <- mapply(met.affinity.single, M, df = df, binary = binary, SIMPLIFY = FALSE)
          return(result)
        }       
      }
    }
  }
}
