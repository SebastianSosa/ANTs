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

#' @title Data frame ANT ID check
#' @description Check that the ID column of a data frame or a list is set as factorof data frames
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @keywords internal
check.id <- function(df) {
  
  #### if it is dataframe
  if (is.data.frame(df)){
    ### check if IDs are already a factor
    if(!is.factor(df$ID)){
      df$ID<-as.factor(df$ID)
      return(df)
    }else{return(df)}
  }
  
  ### if it is list of dataframes
  if (!is.data.frame(df) & is.list(df)) 
  {
    ### check if all IDs in each df list are factors
    if(!all(unlist(lapply(df,function(x){is.factor(x$ID)}))))
    {
      df<-lapply(df, function(x){ 
        x$ID<-as.factor(x$ID)
        return(x)})
      return(df)
    }else{return(df)}
  }
}
