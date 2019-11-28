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

#' @title Continuous assortativity
#' @description Calculates the binary or weighted version of vertices Newman's assortativity for continuous attributes.
#' @param Matrix a square adjacency matrix.
#' @param values The dataframe column that contains the vertices' attributes.
#' @param weighted if \emph{true}, it calculates the weigthed version.
#' @param se if \emph{true}, it calculates the standard error.
#' @return An integer representing the continuous assortativity index of the network.
#' @details Assortativity (like the E-I index or the Moran 'I' statistic) allows the study of homophily (preferential interaction between nodes with similar attributes) and heterophily (the preferential interaction between nodes with different attributes). Attributes can be individual characteristics such as sex or age, or individual node metrics such as the met.degree, in which case it is referred to as assortativity by vertex met.degree.
#' @author  Ivan Puga-Gonzalez, Sebastian Sosa
#' @references Newman, M. E. (2003). Mixing patterns in networks. Physical Review E, 67(2), 026126.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.assortatvityContinuous <- function(Matrix, se = FALSE, values, weighted = TRUE, df = NULL) {
  if (!is.matrix(Matrix)) {
    stop("Argument Matrix is not a matrix")
  }
  if (ncol(Matrix) != length(values)) {
    stop("number of elements in 'values' is not equal to number of nodes")
  }
  # Handling NA
  test = which(is.na(values) == TRUE)
  if(length(test) != 0){
    values = values[-test]
    Matrix = Matrix[-test, -test]
  }
  
  if (weighted) {
    MATjk <- Matrix
  } else {
    MATjk <- Matrix
    MATjk[MATjk > 0] <- 1
    MATjk[MATjk < 1] <- 0
  }

  VertexValuesJi <- matrix(values, length(values), length(values), byrow = FALSE)
  diag(VertexValuesJi) <- 0
  VertexValuesKi <- t(VertexValuesJi)
  InverseMATjk <- 1 / sum(MATjk)
  Num1 <- sum(MATjk * VertexValuesJi * VertexValuesKi)
  Num2 <- sum(MATjk * VertexValuesJi)
  Num3 <- sum(MATjk * VertexValuesKi)
  Den1 <- sum(MATjk * VertexValuesJi^2)
  Den2 <- sum(MATjk * VertexValuesJi)^2
  Den3 <- sum(MATjk * VertexValuesKi^2)
  Den4 <- sum(MATjk * VertexValuesKi)^2
  NUM <- Num1 - InverseMATjk * Num2 * Num3
  DEN <- sqrt((Den1 - InverseMATjk * Den2) * (Den3 - InverseMATjk * Den4))
  r <- NUM / DEN

  if (se) {
    #### calculates se JackKnife method
    Indexes <- which(MATjk > 0)
    se <- 0
    for (a in 1:length(Indexes)) {
      MATjk2 <- MATjk
      MATjk2[Indexes[a]] <- 0
      InverseMATjk2 <- 1 / sum(MATjk2)
      Num1 <- sum(MATjk2 * VertexValuesJi * VertexValuesKi)
      Num2 <- sum(MATjk2 * VertexValuesJi)
      Num3 <- sum(MATjk2 * VertexValuesKi)
      Den1 <- sum(MATjk2 * VertexValuesJi^2)
      Den2 <- sum(MATjk2 * VertexValuesJi)^2
      Den3 <- sum(MATjk2 * VertexValuesKi^2)
      Den4 <- sum(MATjk2 * VertexValuesKi)^2
      NUM <- Num1 - InverseMATjk2 * Num2 * Num3
      DEN <- sqrt((Den1 - InverseMATjk2 * Den2) * (Den3 - InverseMATjk2 * Den4))
      met.ri <- NUM / DEN
      se <- se + (met.ri - r)^2
    }
    n <- length(Indexes)
    se <- sqrt(((n - 1) / n) * se)
    if (is.null(df)) {

    }
    if (weighted) {
      if (is.null(df)) {
        return(c("assor.conti.w" = r, "assor.se" = se))
      }
      else {
        df$assor.conti.w <- r
        df$assor.se <- se
      }
    }
    else {
      if (is.null(df)) {
        return(c("assor.conti.b" = r, "assor.se" = se))
      }
      else {
        df$assor.conti.b <- r
        df$assor.se <- se
      }
    }
  }
  else {
    if (weighted) {
      if (is.null(df)) {
        attr(r, "names") <- "assor.conti.w"
        return(r)
      }
      else {
        df$assor.conti.w <- r
        return(df)
      }
    }
    else {
      if (is.null(df)) {
        attr(r, "names") <- "assor.conti.b"
        return(r)
      }
      else {
        df$assor.conti.b <- r
        return(df)
      }
    }
  }
}
