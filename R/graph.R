graph<-function(v_perm,Obs,title){
    if(is.null(Obs)){
      obs=v_perm[1]
      v_perm=v_perm[-1]
    }
    else{
      obs=Obs
    }
    
    m=mean(v_perm)
    if(obs>m){
      h=hist(v_perm,breaks=length(v_perm),plot = FALSE)
      cuts=cut(h$breaks,c(obs,Inf))
      cuts=ifelse(is.na(cuts),'gray10','gray25')
      if(obs<min(v_perm)){
        xlim=c(floor(obs/0.1)*0.1,ceiling(max(v_perm)/0.1)*0.1)
        names(xlim)=NULL
        plot(h,col=cuts,border=cuts,xlim=xlim,main=paste(title),xlab=NULL,xaxt="n")
      }
      else{
        plot(h,col=cuts,border=cuts,main=paste(title),xlab=NULL,xaxt="n")
      }
      axis(1,pos=0) 
      #mtext(1, text = round(obs,digits=3), at = obs, col = "white")
      abline(v=obs,col='white')
      legend("topright",legend = paste("observed value","\n",round(obs,digits=3)),text.col = 'white',box.lty=0)
    }
    else{
      h=hist(v_perm,breaks=length(v_perm),plot = F)
      cuts=cut(h$breaks,c(obs,Inf))
      cuts=ifelse(is.na(cuts),'gray25','gray10')
      if(obs>max(v_perm)){plot(h,col=cuts,border=cuts,main=paste(title),xlab=NULL,xlim=c(floor(v_perm/0.1)*0.1,ceiling(max(obs)/0.1)*0.1),xaxt="n")}
      else{plot(h,col=cuts,border=cuts,main=paste(title),xlab=NULL,xaxt="n")}
      axis(1,pos=0) 
      #mtext(1, text = round(obs,digits=3), at = obs, col = "white")
      abline(v=obs,col='white')
      legend("topleft",legend = paste("observed value","\n",round(obs,digits=3)),text.col = 'white',box.lty=0)
    }  
}
