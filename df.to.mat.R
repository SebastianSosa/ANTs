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

#' @title Data frame to a matrix
#' @description Converts a data frame into a matrix.
#' @param df a data frame of interactions or associations.
#' @param actor an integer or a string indicating the column of the individuals performing the behaviour.
#' @param receiver an integer or a string indicating the column of the individuals receiving the behaviour.
#' @param weighted an integer or a string indicating the column of the weights of interactions. If NULL, interactions are given a weight of 1.
#' @param tobs an numeric vector or a list of numeric vectors of each individual time of observation. It has to be alphabetically ordered following unique id names.
#' @param sym  a boolean if \emph{true}, interactions or associations are considered symmetric.
#' @param num.ids a boolean if \emph{true}, it returns ordered matrix according to the numeric ids.
#' @return An adjacency matrix (undirected if argument \emph{sym} is true, directed otherwise).
#' @details Converts a data frame of interactions or associations into an adjacency matrix of interactions or associations.
#' @examples
#' head(sim.focal.directed)
#' df.to.mat(df=sim.focal.directed,actor='actor', receiver='receiver')

df.to.mat <- function(df, actor, receiver, weighted = NULL, tobs = NULL, sym = F, num.ids = F) {
  if (!is.data.frame(df) & is.list(df)) {
    if (!is.null(tobs)) {
      if (!is.list(tobs)) {
        stop("Argument tobs must a list of vectors when argument df is a list of data frames")
      }
      m <- mapply(function(x, y, actor, receiver, weighted, sym, num.ids) {
        if (!is.null(weighted)) {
          col.actor <- df.col.findId(x, actor)
          col.receiver <- df.col.findId(x, receiver)
          col.weight <- df.col.findId(x, weighted)
          edgl <- x[, c(col.actor, col.receiver, col.weight)]
        }
        else {
          col.actor <- df.col.findId(x, actor)
          col.receiver <- df.col.findId(x, receiver)
          edgl <- x[, c(col.actor, col.receiver)]
          edgl[, 3] <- rep(1, nrow(x))
        }
        colnames(edgl) <- c("from", "to", "weight")
        m <- edgl_to_matrix(edgl, sym = sym)
        if (num.ids == T) {
          m <- m[order(as.numeric(as.character(rownames(m)))), order(as.numeric(as.character(colnames(m))))]
        }
        else {
          m <- m[order(rownames(m)), order(colnames(m))]
        }
        if (!is.null(y)) {
          if (length(y) == ncol(m)) {
            m <- m / time.heterogeneity(y)
            diag(m) <- 0
          }
          else {
            stop("Argument tobs is not of same length as the number of columns in the output matrix.")
          }
        }
        return(m)
      }, x = df, y = tobs, actor = actor, receiver = receiver, weighted = weighted, sym = sym, num.ids = num.ids)
    }
    else {
      m <- lapply(df, function(x, actor, receiver, weighted, sym, tobs, num.ids) {
        if (!is.null(weighted)) {
          col.actor <- df.col.findId(x, actor)
          col.receiver <- df.col.findId(x, receiver)
          col.weight <- df.col.findId(x, weighted)
          edgl <- x[, c(col.actor, col.receiver, col.weight)]
        }
        else {
          col.actor <- df.col.findId(x, actor)
          col.receiver <- df.col.findId(x, receiver)
          edgl <- x[, c(col.actor, col.receiver)]
          edgl[, 3] <- rep(1, nrow(x))
        }
        colnames(edgl) <- c("from", "to", "weight")
        m <- edgl_to_matrix(edgl, sym = sym)
        if (num.ids == T) {
          m <- m[order(as.numeric(as.character(rownames(m)))), order(as.numeric(as.character(colnames(m))))]
        }
        else {
          m <- m[order(rownames(m)), order(colnames(m))]
        }
        return(m)
      }, actor = actor, receiver = receiver, weighted = weighted, sym = sym, tobs = tobs, num.ids = num.ids)
    }
    names(m) <- names(df)
    attr(m, "ANT") <- "list of matrices obtained through data frames of interactions"
  }
  if (is.data.frame(df)) {
    if (!is.null(weighted)) {
      col.actor <- df.col.findId(df, actor)
      col.receiver <- df.col.findId(df, receiver)
      col.weight <- df.col.findId(df, weighted)
      edgl <- df[, c(col.actor, col.receiver, col.weight)]
    }
    else {
      col.actor <- df.col.findId(df, actor)
      col.receiver <- df.col.findId(df, receiver)
      edgl <- df[, c(col.actor, col.receiver)]
      edgl[, 3] <- rep(1, nrow(df))
    }
    colnames(edgl) <- c("from", "to", "weight")
    m <- edgl_to_matrix(edgl, sym = sym)
    if (num.ids == T) {
      m <- m[order(as.numeric(as.character(rownames(m)))), order(as.numeric(as.character(colnames(m))))]
    }
    else {
      m <- m[order(rownames(m)), order(colnames(m))]
    }
    if (!is.null(tobs)) {
      if (length(tobs) == ncol(m)) {
        m <- m / time.heterogeneity(tobs)
        diag(m) <- 0
      }
      else {
        stop("Argument tobs is not of same length as the number of columns in the output matrix.")
      }
    }
    attr(m, "ANT") <- "Matrix obtained through a data frame of interactions"
  }
  return(m)
}
