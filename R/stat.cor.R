# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
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

#' @title Correlation test on permuted data.
#' @details Performs correlations tests in a data frame and extracts the t statistic.
#'
#' @param ant an ant data object originating from permutations
#' @param var1 an integer or string indicating the column in the data frames that corresponds to the first variable to correlate. This variable must be numerical.
#' @param var2 an integer or string indicating the column in the data frames that corresponds to the second variable to correlate.This variable must be numerical.
#' @param method a string vector indicating which type of correlation to perform:
#' \itemize{
#' \item 'pearson' for pearson correlation
#' \item 'kendall' for kendall correlation
#' \item 'spearman' for spearman correlation
#' }
#' @param progress a boolean indicating the visualization of the permutation process.
#' @return a vector of 3 elements: the t statistic, the degree of freedom, and the estimate.

#' @description This function is the first step in the process to create a correlation test in permuted data. For more details about correlation tests, see R documentation.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#'
#' @references D. J. Best & D. E. Roberts (1975), Algorithm AS 89: The Upper Tail Probabilities of Spearman's rho. Applied Statistics, 24, 377-379.
#' @references Myles Hollander & Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 185-194 (Kendall and Spearman tests).
#' @examples
#' t=met.strength(sim.m,sim.df,1) # Computing network metric
#' t=perm.net.nl(t,labels='age',rf=NULL,nperm=10,progress=FALSE) # Node label permutations
#' r.c=stat.cor(t,'age','strength',progress=FALSE) # Permuted correlation test

stat.cor <- function(ant, var1, var2, method = "pearson", progress = TRUE) {
  # Extract columns id
  id1 <- df.col.findId(ant[[1]], var1)
  id2 <- df.col.findId(ant[[1]], var2)
  
  # Compute correlation coefficient in the list of data frames returned by ANTs data object originating from permutations
 if (progress == TRUE) {
    # Compute correlation coefficient in the list of data frames returned by ANTs data object originating from permutations
    result <- lapply(ant, function(d, id1, id2, method) {
      cat("  Processing file: ", attr(d, "permutation"), "\r")
      r <- cor(x = d[, id1], y = d[, id2], method = method)
      return(r)
    }, id1, id2, method)
  }
  else {
    result <- lapply(ant, function(d, id1, id2, method) {
      r <- cor(x = d[, id1], y = d[, id2], method = method)
      return(r)
    }, id1, id2, method)
  }

  # Merge results and adapt results according to method type
  result <- do.call("rbind", result)
  attr(result, "class") <- "ant cor"
  if (method == "pearson") {
    attr(result, "comment") <- paste("Pearson coefficient")
  }
  if (method == "kendall") {
    attr(result, "comment") <- paste("Kendall coefficient")
  }
  if (method == "spearman") {
    attr(result, "comment") <- paste("Spearman coefficient")
  }
  cat("\n")
  return(result)
}
