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

#' @title Histogram of posterior distribution
#' @description plot muliples histograms of posterior distribution
#' @keywords internal
post.dist=function(v_perm,ncols=NULL,nrows=NULL,Obs=NULL){
  par(bg = 'gray63')
  if(ncol(v_perm)==1 & ncol(v_perm)!=0){
    if(is.null(obs)){
      obs=v_perm[1,1]
      v_perm=v_perm[-1,2]
    }
    if(obs>m){
      h=hist(v_perm,breaks=length(v_perm),xaxt="n")
      cuts=cut(h$breaks,c(obs,Inf))
      cuts=ifelse(is.na(cuts),'gray10','gray25')
      plot(h,col=cuts,border=cuts,main=paste(attributes(v_perm)$comment),xaxt="n")
      axis(1) 
      mtext(1, text = round(obs,digits=2), at = obs, col = "white")
      abline(v=obs,col='white')
      legend("topright",legend = "observed value",text.col = 'white',box.lty=0)
    }
    else{
      h=hist(v_perm,breaks=length(v_perm),xaxt="n")
      cuts=cut(h$breaks,c(obs,Inf))
      cuts=ifelse(is.na(cuts),'gray25','gray10')
      plot(h,col=cuts,border=cuts,xlab=paste(attributes(v_perm)$comment),xaxt="n")
      axis(1) 
      mtext(1, text = round(obs,digits=2), at = obs, col = "white")
      abline(v=obs,col='white')
      legend("topright",legend = "observed value",text.col = 'white',box.lty=0)
    }  
  }
  else{
    if(is.null(ncols) | is.null(nrows)){
      if(ncol(v_perm)<=4)(
        par(mfrow=c(1,ncol(v_perm)))
      )
      if(ncol(v_perm)>4 & ncol(v_perm)<7){
        par(mfrow=c(2,3))
      }
      if(ncol(v_perm)>6 & ncol(v_perm)<9){
        par(mfrow=c(2,4))
      }
    }
    else{par(mfrow=c(nrows,ncols))}
    for (a in 1:ncol(v_perm)) {
      if(is.null(obs)){
        obs=v_perm[1,a]
        v_perm=v_perm[-1,a]
      }
      else{
        obs=Obs[a]
      }

      m=mean(v_perm[,a])
      if(obs>m){
        h=hist(v_perm[,a],breaks=length(v_perm[,a]),plot = FALSE)
        cuts=cut(h$breaks,c(obs,Inf))
        cuts=ifelse(is.na(cuts),'gray10','gray25')
        if(obs<min(v_perm[,a])){
          xlim=c(floor(obs/0.1)*0.1,ceiling(max(v_perm[,a])/0.1)*0.1)
          names(xlim)=NULL
          plot(h,col=cuts,border=cuts,xlim=xlim,main=paste(colnames(v_perm)[a]),xlab=NULL,xaxt="n")
        }
        else{
          plot(h,col=cuts,border=cuts,main=paste(colnames(v_perm)[a]),xlab=NULL,xaxt="n")
        }
        axis(1,pos=-2) 
        mtext(1, text = round(obs,digits=3), at = obs, col = "white")
        abline(v=obs,col='white')
        legend("topright",legend = "observed value",text.col = 'white',box.lty=0)
      }
      else{
        h=hist(v_perm[,a],breaks=length(v_perm[,a]),plot = FALSE)
        cuts=cut(h$breaks,c(obs,Inf))
        cuts=ifelse(is.na(cuts),'gray25','gray10')
        if(obs>max(v_perm[,a])){plot(h,col=cuts,border=cuts,main=paste(colnames(v_perm)[a]),xlab=NULL,xlim=c(floor(v_perm[,a]/0.1)*0.1,ceiling(max(obs)/0.1)*0.1),xaxt="n")}
        else{plot(h,col=cuts,border=cuts,main=paste(colnames(v_perm)[a]),xlab=NULL,xaxt="n")}
        axis(1,pos=-2) 
        mtext(1, text = round(obs,digits=3), at = obs, col = "white")
        abline(v=obs,col='white')
        legend("topright",legend = "observed value",text.col = 'white',box.lty=0)
      }  
    }
  }
  p=recordPlot()
  dev.off()
  return(p)
}
