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

#' @title Diameter
#' @description Calculates the network diameter .

#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param weighted if \emph{true}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the highest met.strength as the shortest path.
#' @param normalization normalizes the weights of the links i.e. divides them by the average strength of the network. Argument normalization can't be TRUE when argument weighted is FALSE.
#' @param directed if \emph{false}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers outgoing ties.
#' @return
#' \itemize{
#' \item a double representing the diameter of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the diameter of the corresponding matrix of the list.
#' \item A list of arguments \emph{df} with a new column of network diameter if argument\emph{df} is not \emph{NULL} and if argument \emph{M} is a list of matrices. The name of the column is adapted according to arguments values \emph{.weighted}, \emph{shortest.weight}, \emph{normalization}, \emph{directed} and \emph{out}.
#' \item A list of arguments \emph{df} with a new column of network diameter if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @details Binary network met.density is the ratio of existing links of a network in relation to all potential links.
#' @author  Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Opsahl, T., Agneessens, F., & Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social networks, 32(3), 245-251.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.diameter(sim.m)

met.diameter<-function(M,df=NULL,weighted=T,shortest.weight=F,normalization=T,directed=T,out=T){
  test=is.matrix(M)
  if(test){
    result=met.geodesicDiameter.single(M,weighted,shortest.weight,normalization,directed,out)[[1]]
    if(is.null(df)){
      return(result)
    }
    else{
      df$diameter=result
      return(df)
    }
    
  }
  
  else{
    tmp='tmp'
    if(weighted==F){
      if(normalization){
        if(shortest.weight){
          if(directed==F){
            attr(tmp,'name')='norm.short.diameterB'
          }
          else{
            if(out){
              attr(tmp,'name')='norm.short.outdiameterB'
            }
            else{
              attr(tmp,'name')='norm.short.indiameterB'
            }
          }
        }
        else{
          if(directed==F){
            attr(tmp,'name')='norm.diameterB'
          }
          else{
            if(out){
              attr(tmp,'name')='norm.outdiameterB'
            }
            else{
              attr(tmp,'name')='norm.indiameterB'
            }
          }
        }
      }
      else{
        if(shortest.weight){
          if(directed==F){
            attr(tmp,'name')='short.diameterB'
          }
          else{
            if(out){
              attr(tmp,'name')='short.outdiameterB'
            }
            else{
              attr(tmp,'name')='short.indiameterB'
            }
          }
        }
        else{
          if(directed==F){
            attr(tmp,'name')='diameterB'
          }
          else{
            if(out){
              attr(tmp,'name')='outdiameterB'
            }
            else{
              attr(tmp,'name')='indiameterB'
            }
          }
        }
      }
    }
    else{
      if(normalization){
        if(shortest.weight){
          if(directed==F){
            attr(tmp,'name')='norm.short.diameter'
          }
          else{
            if(out){
              attr(tmp,'name')='norm.short.outdiameter'
            }
            else{
              attr(tmp,'name')='norm.short.indiameter'
            }
          }
        }
        else{
          if(directed==F){
            attr(tmp,'name')='norm.diameter'
          }
          else{
            if(out){
              attr(tmp,'name')='norm.outdiameter'
            }
            else{
              attr(tmp,'name')='norm.indiameter'
            }
          }
        }
      }
      else{
        if(shortest.weight){
          if(directed==F){
            attr(tmp,'name')='short.diameter'
          }
          else{
            if(out){
              attr(tmp,'name')='short.outdiameter'
            }
            else{
              attr(tmp,'name')='short.indiameter'
            }
          }
        }
        else{
          if(directed==F){
            attr(tmp,'name')='diameter'
          }
          else{
            if(out){
              attr(tmp,'name')='outdiameter'
            }
            else{
              attr(tmp,'name')='indiameter'
            }
          }
        }
      }
    }
    
    if(!is.null(attributes(M)$ANT)){
      test1=attributes(M)$ANT=="ANT data stream focal sampling single matrix"
      test2=attributes(M)$ANT=='ANT data stream group sampling single matrix'
      test3=attributes(M)$ANT=='ANT link permutations single matrix'
      
      test4=attributes(M)$ANT=="ANT data stream focal sampling multiple matrices"
      test5=attributes(M)$ANT=='ANT data stream group sampling multiple matrices'
      test6=attributes(M)$ANT=='ANT link permutations multiple matrices'
      
      if(any(test1,test2,test3)){
        if(is.null(df)){
          result=lapply(M,function(x,weighted,shortest.weight,normalization,directed,out){
            r=met.ge.single(x,weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)[[1]]
            attr(r,"permutation")=attributes(x)$permutation
            return(r)
          },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
        }
        else{
          if(!is.data.frame(df)){stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function","\r")}
          result=lapply(M,function(x,weighted,shortest.weight,normalization,directed,out,df){
            df$diameter=met.ge.single(x,weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)[[1]]
            colnames(df)[ncol(df)]= attributes(tmp)$name
            attr(df,"permutation")=attributes(x)$permutation
            return(df)
          },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out,df=df)
        }
        
        if(test1){
          attr(result,'name')=attributes(tmp)$name
          attr(result,'focal')=attributes(M)$focal
          attr(result,'ctrl')=attributes(M)$ctrl
          attr(result,'alters')=attributes(M)$alters
          attr(result,'method')=attributes(M)$method
          attr(result,'ANT')=attributes(M)$ANT
          return(result)
        }
        
        if(test2){
          attr(result,'name')=attributes(tmp)$name
          attr(result,'scan')=attributes(M)$scan
          attr(result,'ctrlf')=attributes(M)$ctrlf
          attr(result,'method')=attributes(M)$method
          attr(result,'ANT')=attributes(M)$ANT
          return(result)
        }
        
        if(test3){
          attr(result,'name')=attributes(tmp)$name
          attr(result,'ANT')=attributes(M)$ANT
          return(result)
        }
      }
      
      if(any(test4,test5,test6)){
        if(is.null(df)){
          result=lapply(M, function(x,weighted,shortest.weight,normalization,directed,out){
            r1=lapply(x, function(y,weighted,shortest.weight,normalization,directed,out){
              r2=met.ge.single(y,weighted,shortest.weight,normalization,directed,out)[[1]]
              attr(r2,'permutation')=attributes(y)$permutation
              return(r2)
            },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
            return(r1)
          },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
        }
        else{
          if(!is.null(df) & is.data.frame(df)){stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.","\r")}
          if(length(M)==nrow(df)){
            tmp2=lapply(M, function(x,weighted,shortest.weight,normalization,directed,out){
              r1=lapply(x, function(y,weighted,shortest.weight,normalization,directed,out){
                r2=met.ge.single(y,weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)[[1]]
              },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
            },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
            
            tmp2=do.call(Map,c(c,tmp2))
            
            result=lapply(seq_along(tmp2), function(x,tmp2,df,tmp){
              df[[x]]$diameter=tmp[[x]]
              colnames(df[[x]])[ncol(df[[x]])]= attributes(tmp)$name
              return(df[[x]])
            },tmp2=tmp2,df=df,tmp=tmp)
          }
          else{
            #data fame manipulation
            ldf=do.call('rbind',df)
            
            tmp=lapply(M, function(x,weighted,shortest.weight,normalization,directed,out){
              r1=lapply(x, function(y,weighted,shortest.weight,normalization,directed,out){
                r2=met.ge.single(y,weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)[[1]]
              },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
            },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
            
            tmp=do.call(Map,c(c,tmp))
            result=lapply(seq_along(tmp), function(tmp,ldf,i){
              ldf$diameter=tmp[[i]]
              attr(ldf,'permutation')=i
              return(ldf)
            },tmp=tmp,ldf=ldf)
          }
        }
        
        if(test4){
          attr(result,'name')=attributes(tmp)$name
          attr(result,'focal')=attributes(M)$focal
          attr(result,'ctrl')=attributes(M)$ctrl
          attr(result,'alters')=attributes(M)$alters
          attr(result,'method')=attributes(M)$method
          attr(result,'ANT')=attributes(M)$ANT
          return(result)
        }
        
        if(test5){
          attr(result,'name')=attributes(tmp)$name
          attr(result,'scan')=attributes(M)$scan
          attr(result,'ctrlf')=attributes(M)$ctrlf
          attr(result,'method')=attributes(M)$method
          attr(result,'ANT')=attributes(M)$ANT
          return(result)
        }

        if(test6){
          attr(result,'name')=attributes(tmp)$name
          attr(result,'ANT')=attributes(M)$ANT
          return(result)
        }
      }
    }
    else{
      if(!test & is.list(M)){
        if(is.null(df)){
          result=lapply(M,function(x,weighted,shortest.weight,normalization,directed,out){
            r=met.geodesicDiameter.single(x,weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)[[1]]
          },weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)
          attr(result,'name')=attributes(tmp)$name
          return(result)
        }
        
        if(!is.null(df) & !is.data.frame(df) & is.list(df)){
          result=mapply(function(x,y,t){
            y$diameter=met.geodesicDiameter.single(x,weighted=weighted,shortest.weight=shortest.weight,normalization=normalization,directed=directed,out=out)[[1]]
            colnames(y)[ncol(y)]= t
            return(y)
          },x=M,y=df,t=attributes(tmp)$name,SIMPLIFY = F)
          return(result)
        }
      }
    }
  }
}
