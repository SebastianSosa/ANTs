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

#' @title  Data stream permutation for focal sampling data .
#' @details  Pre-network permutation for focal sampling data, and for symmetric behaviour only.
#' @param df a data frame.
#' @param focal an integer indicating the column of the focal.
#' @param alters an integer indicating the column of focal's alters.
#' @param ctrl a numeric vector indicating the control factors.
#' @param nperm an integer indicating the number of permutations.
#' @param progress a boolean indicating if the permutation process must be visible.
#' @param method a string indicating the association index to compute:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @description Warning, the original function (Farine 2017) uses a control factor, the number of focals and the ids of the focals.
#' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in ecology and evolution, 8(10), 1309-1320.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' head(sim.focal.undirected)
#' t=perm.ds.focal(sim.focal.undirected,focal=3,ctrl=1,alters=4,nperm=10,progress=TRUE,method='sri')

perm.ds.focal <- function(df, focal, ctrl = NULL, alters, nperm, progress = T, method = "sri") {
  if (is.null(ctrl)) {
    stop("Argument ctrl cannot be empty")
  }
  test <- check.df(df)
  if (is.null(test)) {
    "Argument df is not a data frame or a list of data frames."
  }
  if (test == "df ok") {
    result <- perm.dataStream.focal(df = df, focal = focal, scan = ctrl, alters = alters, nperm = nperm, progress = progress, method = method)
    attr(result, "ANT") <- "ANT data stream focal sampling single matrix"
    attr(result, "focal") <- focal
    attr(result, "ctrl") <- ctrl
    attr(result, "alters") <- alters
    attr(result, "method") <- method
    return(result)
  }
  if (test == "df list ok") {
    result <- lapply(df, perm.dataStream.focal, focal = focal, scan = ctrl, alters = alters, nperm = nperm, progress = progress, method = method)
    attr(result, "ANT") <- "ANT data stream focal sampling multiple matrices"
    attr(result, "focal") <- focal
    attr(result, "ctrl") <- ctrl
    attr(result, "alters") <- alters
    attr(result, "method") <- method
    return(result)
  }
}
