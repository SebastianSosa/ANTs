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

#' @title Indegree
#' @description Calculates the node metric met.indegree for all vertices.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param dfid an integer or a string indicating the column with individual ids in argument \emph{df}.
#' @return
#' \itemize{
#' \item An integer vector of nodes \emph{indegree} if argument \emph{df} is \emph{NULL}.
#' \item A list of integer vectors of nodes \emph{indegree} if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{indegree} titled 'indegree', if argument \emph{df} is not \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{indegree} if 1) argument \emph{df} is not \emph{NULL}, 2) argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and 3) argument \emph{df} is a list of data frames of same length as argument \emph{M}. The name of the column of each element of the list is adapted according to argument value \emph{binary}.
#' }
#' @details Indegree of a node \emph{i} is the sum of all edges directed towards node \emph{i}.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social networks, 1(3), 215-239.
#' @references Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004). The architecture of complex weighted networks. Proceedings of the National Academy of Sciences of the United States of America, 101(11), 3747-3752.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.indegree(sim.m)
#' head(sim.df)
#' met.indegree(sim.m,df=sim.df)

met.indegree <- function(M, df = NULL, dfid = NULL) {
  # Check if argument M is a square matrix or a list of square matrices----------------------
  test <- check.mat(M)
  # If argument M is a square Matrix----------------------
  if (test=="M ok") {
    # If argument df is NULL return a simple numeric vector
    if (is.null(df)) {
      # Compute network metric
      result <- met.indegree.single(M, df = df, dfid = dfid)
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
      result <- met.indegree.single(M, df = df, dfid = dfid)
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
    if (test1 | test2 | test4 | test5) {
      stop("Argument M is a list of permuted matrices through data stream approach. Such pre-network permutations do not make any degree variation across permuted networks.")
    }

    # If argumpent M is issue single network protocol with node label permutations, we work on a list of list of Matrices
    if (test3) {
      # If argument df is not not null
      if (!is.null(df)) {
        # If argument df is not a data frame
        if (!is.data.frame(df)) {
          stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
        }
      }
      # If argument dfid is null
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      # Compute network metric
      result <- lapply(M, function(x, df = df, dfid = dfid) {
        r <- met.indegree.single(x, df = df, dfid = dfid)
        attr(r, "permutation") <- attributes(x)$permutation
        return(r)
      }, df = df, dfid = dfid)
      attr(result, "ANT") <- attributes(M)$ANT
      return(result)
    }

    # If argumpent M is issue multiples network protocol with node links permutations, we work on a list of list of Matrices
    if (test6) {
      # If argument df is null
      if (is.null(df)) {
        # Compute network metric
        result <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.indegree.single(y)
            attr(r2, "permutation") <- attributes(y)$permutation
            return(r2)
          })
        })
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
      # If argument df is not null
      else {
        # Check if argument df is a data frame
        if (is.data.frame(df)) {
          stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
        }
        # Check if argument is null
        if (is.null(dfid)) {
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        }

        # Check if each matrix size is equal to corresponding data frame size
        # Which means, we are working on a case of multiple repermutations
        # Thus with a list of lists of matrices and data frames
        if (sum(unlist(lapply(seq_along(M), function(i, a) {
          nrow(a[[i]][[1]])
        }, a = M))) == nrow(df[[1]])) {
          tmp <- lapply(M, function(x) {
            r1 <- lapply(x, function(y) {
              r2 <- met.indegree.single(y)
            })
          })

          tmp <- do.call(Map, c(c, tmp))

          result <- lapply(seq_along(df), function(i, a, b) {
            a[[i]]$degree <- b[[i]]
            return(a[[i]])
          }, a = df, b = tmp)
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
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
          tmp <- lapply(M, function(x) {
            r1 <- lapply(x, function(y) {
              r2 <- met.indegree.single(y)
            })
          })
          tmp <- do.call(Map, c(c, tmp))
          result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
            ldf$degree <- tmp[[i]]
            attr(ldf, "permutation") <- i
            return(ldf)
          }, tmp = tmp, ldf = ldf)

          attr(result, "ANT") <- attributes(M)$ANT
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
      result <- lapply(M, met.indegree.single)
      return(result)
    }
    # Check if argument df is not NULL, is not a data frame and is a list
    if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
      # Argument df is a data frame
      if (!is.null(dfid)) {
        # Compute network metric
        result <- mapply(met.indegree.single, M, df = df, dfid = dfid, SIMPLIFY = FALSE)
        return(result)
      }
      else {
        # Compute network metric
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        result <- mapply(met.indegree.single, M, df = df, SIMPLIFY = FALSE)
        return(result)
      }
    }
  }
}