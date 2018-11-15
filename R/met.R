# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title ANT Graphical User Interface (GUI)
#' @description Graphical user interface for the calculation of node metrics.
#' @param M a square adjacency matrix or a list of square adjacency matrices or output from ANT functions of type perm'.
#' @param df  a data frame of same length as in the input matrix or a list of data frames of same length as the correspondoing matrix in argument M.
#' @param dfid an integer or a string indicating the column of individual ids in argument \emph{df}
#' @details  returns the data frame with node network metrics, if weighted =TRUE:
#' \itemize{
#' \item Weighted met.degree (WD)
#' \item Weighted met.indegree (WID)
#' \item Weighted met.outdegree (WOD)
#' \item Weighted met.met.ri (met.met.ri)
#' \item Weighted met.evcent (WEi)
#' \item Weighted clustering coefficient (WCC)
#' \item Weighted closeness (WCL)
#' \item Weighted betweenness (WDB)
#' \item Weighted betweenness (WUB)
#' \item Weighted met.disparity (met.disparity)
#' }
#' if weighted =NULL, it returns binary network metrics:
#' \itemize{
#' \item Binary met.degree (BD)
#' \item Binary met.indegree (BID)
#' \item Binary met.outdegree (BOD)
#' \item Binary met.met.ri (met.met.ri)
#' \item Binary met.evcent (BEi)
#' \item Binary clustering coefficient (BCC)
#' \item Binary closeness (BCL)
#' \item Binary betweenness (DBB)
#' \item Binary betweenness (UBB)
#' \item Binary met.evcent community (e.i. modularity) (BEiM)
#' }
#' @author Sebastian Sosa, Ivan Puga-Gonzalez

met <- function(M, df = NULL, dfid = NULL) {
  if (is.null(df) & !is.null(dfid)) {
    stop("Argument 'df' can't be NULL when argument 'dfid' isn't")
  }

  op <- suppressWarnings(nodeskMetrics())

  if (is.matrix(M)) {
    result <- nodeskMetrics2(M, option = op, df = df, dfid = dfid)
    return(result)
  }

  if (!is.null(attributes(M)$ANT)) {
    test1 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
    test2 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
    test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

    test4 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
    test5 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
    test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"


    if (any(test1, test2, test3)) {
      if (!is.null(df)) {
        warning("Argument M is a list of permuted matrices through data stream approach. Such pre-network permutations do not make any degree variation across permuted networks.", "\r")
        if (!is.data.frame(df)) {
          stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
        }
        result <- lapply(M, nodeskMetrics2, option = op, df = df, dfid = dfid)
      }
      else {
        warning("Argument M is a list of permuted matrices through data stream approach. Such pre-network permutations do not make any degree variation across permuted networks.", "\r")
        result <- lapply(M, nodeskMetrics2, option = op)
      }

      if (test1) {
        attr(result, "scan") <- attributes(M)$scan
        attr(result, "ctrlf") <- attributes(M)$ctrlf
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }

      if (test2) {
        attr(result, "focal") <- attributes(M)$focal
        attr(result, "ctrl") <- attributes(M)$ctrl
        attr(result, "alters") <- attributes(M)$alters
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }

      if (test3) {
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
    }
    if (any(test4, test5, test6)) {
      if (is.null(df)) {
        result <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- nodeskMetrics2(y, option = op)
            attr(r2, "permutation") <- attributes(y)$permutation
            return(r2)
          })
        })
      }
      if (!is.null(df) & is.data.frame(df)) {
        stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
      }
      else {
        if (!is.null(dfid)) {
          # data fame manipulation
          dfid <- df.col.findId(df[[1]], dfid)
          df <- lapply(df, function(x) {
            x <- x[order(x[[dfid]]), ]
          })
        }

        ldf <- do.call("rbind", df)

        tmp <- lapply(M, function(x, option) {
          r1 <- lapply(x, function(y, option) {
            r2 <- nodeskMetrics2(y, option = op)
          }, option = op)
        }, option = op)

        tmp <- do.call(Map, c(f = rbind, tmp))

        result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
          ldf <- cbind(ldf, tmp[[i]])
          attr(ldf, "permutation") <- i
          return(ldf)
        }, tmp = tmp, ldf = ldf)
      }

      if (test4) {
        attr(result, "scan") <- attributes(M)$scan
        attr(result, "ctrlf") <- attributes(M)$ctrlf
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }

      if (test5) {
        attr(result, "focal") <- attributes(M)$focal
        attr(result, "ctrl") <- attributes(M)$ctrl
        attr(result, "alters") <- attributes(M)$alters
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }

      if (test6) {
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
    }
  }
  else {
    if (!is.matrix(M) & is.list(M)) {
      if (is.null(df) & is.null(dfid)) {
        result <- lapply(M, function(x, op) {
          r <- nodeskMetrics2(x, option = op)
          return(r)
        }, op)
        return(result)
      }
      if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
        result <- mapply(function(x, df, dfid, op) {
          r <- nodeskMetrics2(x, option = op)
          col.id <- df.col.findId(df, dfid)
          df <- df[match(colnames(x), df[, col.id]), ]
          r <- data.frame(df, r)
          return(r)
        }, M, df = df, dfid = dfid, op = op, SIMPLIFY = FALSE)
        return(result)
      }
    }
  }

  stop("Argument M is not a matrix or a list of matrices, or a list of permuted matrices obtained with perm.ds.grp, perm.ds.focal or perm.n.nl ANT functions")
}
