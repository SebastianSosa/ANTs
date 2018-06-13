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

#' @title Interpret ANT GUI output
#' @description Interpret tcltk \emph{nodeskMetrics} function output
#' @param M a matrix
#' @param df a data frame of same row number as the input matrix
#' @param option a numeric vector issue from nodeskMetrics function. 
#' @keywords internal
nodeskMetrics2<-function(M,option,df=NULL,dfid=NULL) {
  #attr(option,'names')=c('affBi','affW',
  #                      'betBi','betUn','betOut','betIn','betW','betN','betS',
  #                      'deg','degOut','degIn',
  #                      'dispUn','dispOut','dispIn',
  #                      'eigBi','eigUn','eigOut','eigIn','eigW',
  #                      'lpBi','lp',
  #                      'reach', 'reachB',
  #                      'ri',
  #                      ''str','strOut','strIn')

  if(!is.null(df) & !is.null(dfid)){
    col.id=df.col.findId(df,dfid)
    df=df[match(colnames(M), df[,col.id]),]
  }
  
  if(is.null(df)){
    df=df.create(M)
    rownames(df)=colnames(M)
  }

# Affinity ------------------------------------------------------------------------------------
  if(option[1]==1){
    affinityB=met.affinity(M,binary=T)
    df=data.frame(df,affinityB)
  }
  if(option[2]==1){
    affinity=met.affinity(M,binary=F)
    df=data.frame(df,affinity)
    }

# Betweenness ---------------------------------------------------------------------------------
  if(any(option[3:9]==1)){
    option3=option[3]==1 #binary
    option4=option[4]==1 #undirected
    option5=option[5]==1 #Out
    option6=option[6]==1 #in
    option7=option[7]==1 #weigthed
    option8=option[8]==1 #normalized
    option9=option[9]==1 #strongest links
    
    if(!option3 | !option7){stop("At least one of the options Binary or Weighted must be selected to compute a version of betweeness")}
    
    if(option3 & !option7 & option9){stop('Binary betweenness through strongest paths cannot be computed because all the links are equal to 1.')}
    if(option3){
      # Binary not noarmalized  betweenness
      if(option4 & !option8){
        betweennessB=met.betweenness(M,binary=T,shortest.weight=F,normalization=F,sym=T)
        df=data.frame(df,betweennessB)
      }
      if(option5 & !option8){
        outbetweennessB=met.betweenness(M,binary=T,shortest.weight=F,normalization=F,sym=F,out=T)
        df=data.frame(df,outbetweennessB)
      }
      if(option6 & !option8){
        inbetweennessB=met.betweenness(M,binary=T,shortest.weight=F,normalization=F,sym=F,out=F)
        df=data.frame(df,inbetweennessB)
        }
      # Binary noarmalized  betweenness
      if(option4 & option8){
        norm.betweennessB=met.betweenness(M,binary=T,shortest.weight=F,normalization=T,sym=T)
        df=data.frame(df,norm.betweennessB)
      }
      if(option5 & option8){
        norm.outbetweennessB=met.betweenness(M,binary=T,shortest.weight=F,normalization=T,sym=F,out=T)
        df=data.frame(df,norm.outbetweennessB)
      }
      if(option6 & option8){
        norm.inbetweennessB=met.betweenness(M,binary=T,shortest.weight=F,normalization=T,sym=F,out=F)
        df=data.frame(df,norm.inbetweennessB)
      }
    }
    if(option7){
      # weighted non noarmalized and through strongest links betweenness
      if(option4 & !option8 & !option9){
        betweenness=met.betweenness(M,binary=F,shortest.weight=F,normalization=F,sym=T)
        df=data.frame(df,betweenness)
      }
      if(option5 & !option8 & !option9){
        outbetweenness=met.betweenness(M,binary=F,shortest.weight=F,normalization=F,sym=F,out=T)
        df=data.frame(df,outbetweenness)
      }
      if(option6 & !option8 & !option9){
        inbetweenness=met.betweenness(M,binary=F,shortest.weight=F,normalization=F,sym=F,out=F)
        df=data.frame(df,inbetweenness)
      }
      # weighted noarmalized and through weakest links betweenness
      if(option4 & !option8 & option9){
        norm.short.betweenness=met.betweenness(M,binary=F,shortest.weight=T,normalization=T,sym=T)
        df=data.frame(df,norm.short.betweenness)
      }
      if(option5 & !option8 & option9){
        norm.short.outbetweenness=met.betweenness(M,binary=F,shortest.weight=T,normalization=T,sym=F,out=T)
        df=data.frame(df,norm.short.outbetweenness)
      }
      if(option6 & !option8 & option9){
        norm.short.inbetweenness=met.betweenness(M,binary=F,shortest.weight=T,normalization=T,sym=F,out=F)
        df=data.frame(df,norm.short.inbetweenness)
      } 
      
      # weighted noarmalized and through strongest links betweenness
      if(option4 & option8 & !option9){
        norm.betweenness=met.betweenness(M,binary=F,shortest.weight=F,normalization=T,sym=T)
        df=data.frame(df,norm.betweenness)
      }
      if(option5 & option8 & !option9){
        norm.outbetweenness=met.betweenness(M,binary=F,shortest.weight=F,normalization=T,sym=F,out=T)
        df=data.frame(df,norm.outbetweenness)
      }
      if(option6 & option8 & !option9){
        norm.inbetweenness=met.betweenness(M,binary=F,shortest.weight=F,normalization=T,sym=F,out=F)
        df=data.frame(df,norm.inbetweenness)
      }
      # weighted noarmalized and through weakest links betweenness
      if(option4 & option8 & option9){
        norm.short.betweenness=met.betweenness(M,binary=F,shortest.weight=T,normalization=T,sym=T)
        df=data.frame(df,norm.short.betweenness)
      }
      if(option5 & option8 & option9){
        norm.short.outbetweenness=met.betweenness(M,binary=F,shortest.weight=T,normalization=T,sym=F,out=T)
        df=data.frame(df,norm.short.outbetweenness)
      }
      if(option6 & option8 & option9){
        norm.short.inbetweenness=met.betweenness(M,binary=F,shortest.weight=T,normalization=T,sym=F,out=F)
        df=data.frame(df,norm.short.inbetweenness)
        }  
    }
  }

# Degree --------------------------------------------------------------------------------------
  if(option[10]==1){
    degree=met.degree(M)
    df=data.frame(df,degree)
  }
  if(option[11]==1){
    outdegree=met.outdegree(M)
    df=data.frame(df,outdegree)
  }
  if(option[12]==1){
    indegree=met.indegree(M)
    df=data.frame(df,indegree)
  }

# Disparity -----------------------------------------------------------------------------------
  if(option[13]==1){
    disparity=met.disparity(M)
    df=data.frame(df,disparity)
  }
  #if(option[14]==1 | option[15]==1 ){
  #disDir=met.disparity(M,directed=T)
  #if(option[13]==1){df=data.frame(df,disDir)}
  #else{
  #disDir=disDir[,-1]
  #df=data.frame(df,disDir)
  #}
  #}

# Eigenvector ---------------------------------------------------------------------------------
  if(any(option[16:20])){
    option16=option[16]==1 # Binary
    option17=option[17]==1 # Undirected
    option18=option[18]==1 # Out
    option19=option[19]==1 # In
    option20=option[20]==1 # Weigehted
    
    if(option16){
      if(option17){
        eigenB=met.eigen(M,sym=T,binary=T,out=F)
        df=data.frame(df,eigenB)
      }
      if(option18){
        outeigenB=met.eigen(M,sym=F,binary=T,out=T)
        df=data.frame(df,outeigenB)
      }
      if(option19){
        ineigenB=met.eigen(M,sym=F,binary=T,out=F)
        df=data.frame(df,ineigenB)
      }
    }
    if(option20){
      if(option17){
        eigen=met.eigen(M,sym=T,binary=T,out=F)
        df=data.frame(df,eigen)
      }
      if(option18){
        outeigen=met.eigen(M,sym=T,binary=T,out=T)
        df=data.frame(df,outeigen)
      }
      if(option19){
        ineigen=met.eigen(M,sym=T,binary=T,out=F)
        df=data.frame(df,ineigen)
      }
    }
  }

# Laplacian centrality ------------------------------------------------------------------------
  if(option[21]==1){
    lpB=met.lp(M,binary=T)
    df=data.frame(df,lpB)
  }
  if(option[22]==1){
    lp=met.lp(M,binary=F)
    df=data.frame(df,lp)
  }

# Reach ---------------------------------------------------------------------------------------
  if(option[23]==1){
    reach=met.reach(M)
    df=data.frame(df,reach)
  }
  #if(option[24]==1){warning("Binary reach version is not available ")}

# Ri index ------------------------------------------------------------------------------------
  if(option[25]==1){
    ri=met.ri(M)
    df=data.frame(df,ri)
  }

# Strength ------------------------------------------------------------------------------------
  if(option[26]==1){
    strength=met.strength(M)
    df=data.frame(df,strength)
  }
  if(option[27]==1){
    outstrength=met.outstrength(M)
    df=data.frame(df,outstrength)
  }
  if(option[28]==1){
    instrength=met.instrength(M)
    df=data.frame(df,instrength)
  }

# End of computation --------------------------------------------------------------------------


  return(df)
  
}