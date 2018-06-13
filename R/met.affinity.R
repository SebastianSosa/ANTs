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

#' @title Affinity
#' @description Calculates the node metric \emph{affinity} for all the nodes of the network 
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param dfid an integer or a string indicating the column with individual ids in argument \emph{df}.
#' @param binary a boolean, if \emph{TRUE}, it calculates the binary version of the affinity.
#' @return
#' \itemize{
#' \item An integer vector of nodes \emph{affinity} if argument \emph{df} is \emph{NULL}.
#' \item A list of integer vectors of nodes \emph{affinity} if argument \emph{M} is a list of matrices and  if argument \emph{df} is \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{affinity} if argument\emph{df} is not \emph{NULL}. The name of the column is adapted according to argument value \emph{binary}.
#' \item A list of arguments df with a new column for nodes \emph{affinity} if 1) argument \emph{df} is not \emph{NULL}, 2) argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and 3) argument \emph{df} is a list of data frames of same length as argument \emph{M}.The names of the column of each element of the list is adapted according to argument value \emph{binary}.
#' }
#' @details  Affinity is a second-order metric that evaluates how alters of node i are connected. 
#' The binary version is the average degree of alters of node i. 
#' The weighted version is the ratio between the metric reach and the strength of node i. A high affinity reveals that nodes tend to be connected to alters with high degrees or strengths.
#' Thus, this metric informs on node assortativity by vertex met.degree, i.e. connections between nodes with similar degrees or strengths.
#' 
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.affinity(sim.m)
#'head(sim.df)
#' met.affinity(sim.m,df=sim.df)

met.affinity<-function(M,df=NULL,dfid=NULL,binary=F){
  test=is.matrix(M)
  if(is.null(dfid)){
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      } 
  if(test){
    result=met.affinity.single(M,df=df,dfid=dfid,binary=binary)
    cat("\n")
    return(result)
  }
  
  if(!is.null(attributes(M)$ANT)){
    test1=attributes(M)$ANT=='ANT data stream group sampling single matrix'
    test2=attributes(M)$ANT=="ANT data stream focal sampling single matrix"
    test3=attributes(M)$ANT=="ANT link permutations single matrix"
    
    test4=attributes(M)$ANT=='ANT data stream group sampling multiple matrices'
    test5=attributes(M)$ANT=="ANT data stream focal sampling multiple matrices"
    test6=attributes(M)$ANT=="ANT link permutations multiple matrices"
      
    if(any(test1,test2,test3)){
      if(!is.null(df)){
        if(!is.data.frame(df)){stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function","\r")}
      }
      if(is.null(dfid)){
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      } 
      result=lapply(M,function(x,df,dfid,binary){
        r=met.affinity.single(x,df=df,dfid=dfid,binary=binary)
        attr(r,"permutation")=attributes(x)$permutation
        return(r)
      },df=df,dfid=dfid,binary=binary) 
      
      if(test1){
        attr(result,'scan')=attributes(M)$scan
        attr(result,'ctrlf')=attributes(M)$ctrlf
        attr(result,'method')=attributes(M)$method
        attr(result,'ANT')=attributes(M)$ANT
        return(result)
      } 
      
      if(test2){
        attr(result,'focal')=attributes(M)$focal
        attr(result,'ctrl')=attributes(M)$ctrl
        attr(result,'alters')=attributes(M)$alters
        attr(result,'method')=attributes(M)$method
        attr(result,'ANT')=attributes(M)$ANT
        return(result)
      }
      
      if(test3){
        attr(result,'ANT')=attributes(M)$ANT
        return(result)
      }
      
    }
    if(any(test4,test5,test6)){
      if(is.null(df)){
        result=lapply(M, function(x,binary){
          r1=lapply(x, function(y,binary=binary){
            r2=met.affinity.single(y,binary=binary)
            attr(r2,'permutation')=attributes(y)$permutation
            return(r2)
          },binary=binary)
        },binary)
      }
      else{
        if(!is.null(df) & is.data.frame(df)){stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.","\r")}
        if(is.null(dfid)){
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        }
        if(sum(unlist(lapply(seq_along(M), function(i,a){nrow(a[[i]][[1]])},a=M)))==nrow(df[[1]])){
          tmp=lapply(M, function(x,binary){
            r1=lapply(x, function(y,binary=binary){
              r2=met.affinity.single(y,binary=binary)
            },binary=binary)
          },binary=binary)
          tmp=do.call(Map,c(c,tmp))
          
          if(binary){
            result=lapply(seq_along(df), function(i,a,b){
              a[[i]]$affinityB=b[[i]]
              return(a[[i]])
            },a=df, b=tmp)
          }
          else{
            result=lapply(seq_along(df), function(i,a,b){
              a[[i]]$affinity=b[[i]]
              return(a[[i]])
            },a=df, b=tmp)
          }
        }
        else{
          #data fame manipulation
          if(!is.null(dfid)){
            dfid=df.col.findId(df[[1]],dfid)
            df=lapply(df,function(x){x=x[order(x[[dfid]]),]})
          }
          else{
            warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
          }
          ldf=do.call('rbind',df)
          
          tmp=lapply(M, function(x,binary){
            r1=lapply(x, function(y,binary=binary){
              r2=met.affinity.single(y,binary=binary)
            },binary=binary)
          },binary=binary)
          tmp=do.call(Map,c(c,tmp))
          
          if(binary){
            result=lapply(seq_along(tmp), function(tmp,ldf,i){
              ldf$affinityB=tmp[[i]]
              attr(ldf,'permutation')=i
              return(ldf)
            },tmp=tmp,ldf=ldf)
          }
          else{
            result=lapply(seq_along(tmp), function(tmp,ldf,i){
              ldf$affinity=tmp[[i]]
              attr(ldf,'permutation')=i
              return(ldf)
            },tmp=tmp,ldf=ldf)
          }
        }
      }
      
      if(test4){
        attr(result,'scan')=attributes(M)$scan
        attr(result,'ctrlf')=attributes(M)$ctrlf
        attr(result,'method')=attributes(M)$method
        attr(result,'ANT')=attributes(M)$ANT
        return(result)
      }
      
      if(test5){
        attr(result,'focal')=attributes(M)$focal
        attr(result,'ctrl')=attributes(M)$ctrl
        attr(result,'alters')=attributes(M)$alters
        attr(result,'method')=attributes(M)$method
        attr(result,'ANT')=attributes(M)$ANT
        return(result)
      }
      
      if(test6){
        attr(result,'ANT')=attributes(M)$ANT
        return(result)
      }
    }
  }
  
  if(!test & is.list(M)){
    if(is.null(df) & !is.null(dfid)){stop("Argument 'df' can't be NULL when argument 'dfid' isn't","\r")}
    
    if(is.null(df) & is.null(dfid)){
      result=lapply(M,met.affinity.single,binary)
      cat("\n")
      return(result)
    }
    
    if(!is.null(df) & !is.data.frame(df) & is.list(df)){
      if(!is.null(dfid)){
        result=mapply(met.affinity.single,M,df=df,dfid=dfid,binary=binary,SIMPLIFY = F)
        cat("\n")
        return(result)
      }
      else{
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        result=mapply(met.affinity.single,M,df=df,binary=binary,SIMPLIFY = F)
        cat("\n")
        return(result) 
      }
      
    }
  }
  
  
}