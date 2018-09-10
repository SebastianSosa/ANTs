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

#' @title Node label permutations with or without random factor(s)
#' @description Performs node label permutations.
#' @param ldf a list of data frames, with each data frame representing a subset of the random factors (e.g. groups and/or periods)
#' @param labels a numeric or character vector representing the labels to permute.
#' @param rf an integer or a string indicating the column holding the factor stating multiple networks.
#' @param nperm an integer indicating the number of permutations wanted.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @return A list of data frames. Each data frame is the merger of the input lists of data frames. The first element of the list is the original input data, the other elements are the different permutations.
#' @details Node label permutations for GLMM models need to permute labels within a control factor.
#' @references Croft, D. P., James, R., & Krause, J. (2008). Exploring animal social networks. Princeton University Press.
#' @references Croft, D. P., Madden, J. R., Franks, D. W., & James, R. (2011). Hypothesis testing in animal social networks. Trends in Ecology & Evolution, 26(10), 502-507.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples
#' df=met.strength(sim.m,df=sim.df)
#' t=perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)

perm.net.nl <- function(ldf, labels, rf = NULL, nperm, progress = T) {
  if (is.data.frame(ldf) && is.null(rf)) {
    if (is.null(rf)) {
      if (is.data.frame(ldf) == FALSE) {
        stop("Argument df is not a data frame")
      }
      col.id <- df.col.findId(ldf, labels)
      result <- perm_nodeLabels(ldf, label = col.id, nperm, progress = progress)
      attr(result, "ANT") <- "ANT node label permutation"
      attr(result, "labels") <- paste(labels)
      return(result)
    }
  }
  if (is.data.frame(ldf) && !is.null(rf)) {
    stop("Argument ldf is incorrect. Node label permutations with random factors cannot be run on a single data frame. Argument ldf has to be a list of data frames for this type of permutation approach.")
  }
  if (!is.data.frame(ldf) && !is.null(rf)) { # need to check if all elements of the list is a data frame
    col.id <- df.col.findId(ldf[[1]], labels)
    result <- perm_nl_rf(ldf = ldf, lables = col.id, nperm = nperm, progress = progress)
    attr(result, "ANT") <- "ANT node label permutation with random factors"
    attr(result, "rf") <- rf
    attr(result, "labels") <- labels
  }
  return(result)
}
