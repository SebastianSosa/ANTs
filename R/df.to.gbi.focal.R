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

#' @title Converts a data frame of individual associations into a group by individual matrix.
#' @description Converts a data frame of individual associations into a group by individual matrix.
#' @param df a data frame in which to include a control column.
#' @param scan a numeric or character vector representing one or more columns used as scan factors.
#' @param id a numeric or character vector indicating the column holding ids of individuals.
#' @return The same data frame input with an extra column named 'control', representing the control factors.
#' @details Control factors are used in permutation approaches to constrain their permutations.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples 
#'df.to.gbi.focal(df=sim.focal.undirected,focal='focal', alters='alter',ctrl='nfocal')
#' @keywords internal
df.to.gbi.focal<-function(df,focal,alters,ctrl){
  col.alters=df.col.findId(df,alters)
  col.focal=df.col.findId(df,focal)
  col.ctrl=df.col.findId(df,ctrl)
  
  ctrl=c(col.ctrl,col.focal)
  df=df.ctrlFactor(df,control = ctrl)
  df$control=as.factor(df$control)
  
  Vecids=unique(c(as.character(df[,col.focal]),as.character(df[,col.alters])))
  group_scan=unique(df$control)
  GBI=df_to_gbi(df,col.ctrl,col.alters,Vecids,group_scan)
  GBI
}