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

#' @title T-test on data frame
#' @description Performs correlations T-test through observed and permuted data to calculate the p-value on the posterior distribution
#'
#' @param ant an output of ANT function \code{perm.net.nl} without any random factor declared, or output of ANT 'met' categories functions in which output of ANT functions \code{perm.ds.focal}, \code{perm.ds.grp} or \code{perm.net.lk} where single matrix have been used.
#' @param formula a formula of the form lhs ~ rhs where lhs is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param na.action a function which indicates what should happen when the data contain NAs. Defaults to getOption("na.action").
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @return a data frame with 2 columns: the t statistic, the met.degree of freedom, the confidence interval for var1 and 2, and the estimates for var1 and var2

#' @details t-test on permuted data allows to extract the posterior distribution of the value of interest. The posterior distribution allows to calculate the p-value. For more details about t-tests, see R documentation.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples
#' t=met.strength(sim.m,sim.df,1) # Computing network metric
#' t=perm.net.nl(t,labels='age',rf=NULL,nperm=10,progress=FALSE) # Node label permutations
#' r.t=stat.t(t,formula = strength ~ sex,progress=FALSE) # Permuted t-test
#' @seealso \code{\link{t.test}}

stat.t <- function(ant, formula, alternative = "two.sided", na.action = na.omit, mu = 0, paired = FALSE, var.equal = FALSE, progress = TRUE) {
  # Extract observed data
  obs <- ant[[1]]
  # T-test on observed data
  obs <- t.test(formula, data = obs, na.action = , mu = mu, paired = paired, var.equal = var.equal)
  ant <- ant[-1]

  if (progress == TRUE) {
    # T-test on permuted data
    results <- lapply(ant, function(d, formula = formula, na.action = na.action, mu = mu, paired = paired, var.equal = var.equal) {
      cat("  Processing file: ", attr(d, "permutation"), "\r")
      r <- t.test(formula, data = d, na.action = na.action, mu = mu, paired = paired, var.equal = var.equal)
      r <- list(r$statistic)
      return(r)
    }, formula, na.action = na.action, mu = mu, paired = paired, var.equal = var.equal)
    cat("\n")
  }
  # If argument progress is FALSE, same as previoulsy but without printing statistical test progress
  else {
    results <- lapply(ant, function(d, formula = formula, na.action = na.action, mu = mu, paired = paired, var.equal = var.equal) {
      r <- t.test(formula, data = d, na.action = na.action, mu = mu, paired = paired, var.equal = var.equal)
      r <- list(r$statistic)
      return(r)
    }, formula, na.action = na.action, mu = mu, paired = paired, var.equal = var.equal)
  }

  # Merge list of T values
  r <- do.call("rbind", results)
  result <- list()
  result$observe <- obs
  result$permutation <- r

  # Extract T-test information
  attr(result, "class") <- "ant t-test"
  if (paired == FALSE) {
    attr(result, "comment") <- "unpaired"
  }
  else {
    attr(result, "comment") <- "paired"
  }
  if (alternative == "two.sided") {
    attr(result, "alternative") <- "two sided"
  }
  if (alternative == "greater") {
    attr(result, "alternative") <- "greater"
  }
  if (alternative == "less") {
    attr(result, "alternative") <- "less"
  }
  return(result)
}
