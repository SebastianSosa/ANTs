# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
#
# ANT is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Confidence interval
#' @description Calculate 25, 50, 95 and 99 Confidence interval of a data set
#' @param x a numeric vector.
#' @param quantile numeric vector of probabilities with values in [0,1]. (Values up to 2e-14 outside that range are accepted and moved to the nearby endpoint.)
#' @return a numeric vector of length 2 with the lower and upper confidence interval.
#' @details Confidence interval allow to asses a level of certitude that the average of the 'population' is between a specific range.
#' @author Sebastian Sosa,Ivan Puga-Gonzalez
#' @keywords internal

stat.ci <- function(x, probs = c(0.05, 0.95)) {
  if(length(probs) > 2){stop("Only two bornes are allowed for quantiles")}
  ci = quantile(x, probs) 
  names(ci) <- c("lower ci", "upper ci")
  return(ci)
}
