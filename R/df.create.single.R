# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Creates an empty data frame.
#' @description Creates an empty data frame with as many rows or columns as in the corresponding matrix.
#' @param M a matrix.
#' @param names if \emph{true} then a column in the data frame is added with the names of the matrix columns.
#' @return A data frame of N rows based on the number of rows in argument M.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal


df.create.single<-function(M,names=T){
    df=data.frame(matrix(ncol=0,nrow =ncol(M)))

    if(names){df$id=colnames(M)}

  return(df)
}

# work that still remain in this function:
# - Add a error check if col=T but in reality is not
