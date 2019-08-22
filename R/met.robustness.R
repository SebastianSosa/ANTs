#' @title Metric robustness
#' @description Perform a matrix boostrapping approach to estimate the confidence intervals surrounding each pairwise association.

#' @param df a data frame of individual associations
#' @param subsampling a vector of integers indicating the percentage of data to remove
#' @param metric the network metric to compute
#' @param assoc.indices a bollean indicating if association indices have to be use
#' @param actor If argument assoc.indices is FALSE fill this argument, an integer or a string indicating the column of the individuals performing the behaviour.
#' @param receiver If argument assoc.indices is FALSE fill this argument, an integer or a string indicating the column of the individuals receiving the behaviour.
#' @param scan If argument assoc.indices is TRUE fill this argument, ana numeric or character vector representing one or more columns used as scan factors.
#' @param id If argument assoc.indices is TRUE fill this argument, ana numeric or character vector indicating the column holding ids of individuals.
#' @return 3 elements:
#' \itemize{
#' \item A matrix in which each columns represent a node metric variation throough bootstraping, with the first row representing the original metric.
#' \item A summary of bootstrap distribution for each nodes.
#' \item A plot of the 
#' }

#' @details This process evaluate networks metrics robustness by performing a boostrapp process to remove a certain percent of the data collected and recomputing the network metric of intereste. This have been usde in Balasubramaniam et al. 2018 to asses sensitivty of sampling effor of global network metrics in primates networks.
#' @author Sebastian Sosa
#' @references Balasubramaniam, K. N., Beisner, B. A., Berman, C. M., De Marco, A., Duboscq, J., Koirala, S., ... & Ogawa, H. (2018). The influence of phylogeny, social style, and sociodemographic factors on macaque social network structure. American journal of primatology, 80(1), e22727.
#' @examples
#'df = sim.focal.directed
#' test = met.robustness(df, actor = 'actor', receiver = 'receiver', metric = 'met.strength')
#' # objects return by the function
#' test$metrics
#' test$summary
#' test$plot
#' # Examples with metric extra arguments
#' met.robustness(df, actor = 'actor', receiver = 'receiver', metric = 'met.affinity')
#' met.robustness(df, actor = 'actor', receiver = 'receiver', metric = 'met.affinity', sym = FALSE)
#' # Examples with association data
#' test2 = met.robustness(df = sim.grp, assoc.indices = TRUE, scan = c('time', 'location'), id = 'ID', metric  = 'met.strength')
#' # Example of how to test global network metric robustness by removing 10% of the observations simulated 100 times
#' test = met.robustness(df, subsampling = rep(10,100), actor = 'actor', receiver = 'receiver', metric = 'met.diameter')
met.robustness <- function(df, subsampling = c(5,10,20,30,40,50), metric = 'met.strength', assoc.indices = FALSE, actor, receiver, scan, id, index = 'sri', ...){
  # Computing percentages 
  percent = (subsampling*nrow(df))/100
  
  if(!assoc.indices){
    # Computing original metric
    col.actor = ANTs:::df.col.findId(df, actor)
    col.receiver = ANTs:::df.col.findId(df, receiver)
    M = df.to.mat(df, actor = col.actor, receiver = col.receiver)
    met = do.call(metric, list(M = M, ...))
    result = rep(list(met),length(subsampling)+1)
    
    # Bootstraping for each values declare by user
    for (a in 1:length(subsampling)) {
      cat('Processing bootstrap : ', a, '\r')
      tmp = df[-sample(1:nrow(df), percent[a], replace = FALSE),]
      M = df.to.mat(tmp, actor = col.actor, receiver = col.receiver)
      #print(do.call(metric, list(M = M, ...)))
      result[[a]] = do.call(metric, list(M = M, ...))
    }
    
    # Merge result
    result = do.call(rbind, lapply(lapply(result, unlist), "[",
                          unique(unlist(c(sapply(result,names))))))
    
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1,])
    stripchart(result[1,] ~ c(1:ncol(result)),  vertical = T,
               method = "jitter", pch = 21,
               col = "white", bg = "white", add= TRUE)  
    p = recordPlot()
    
    # Giving ANTs attribute for futures developement of analytical protocol
    attr(result,'ANT') = 'Bootsraping deletions'
    # Returning a list with 1) the different values of nodes metrics, 2) the summary of posterior distribution for each individuals node metric, 3) a boxplot with posterior distribution for each individuals node metric
    return = list('metrics' = result, 'summary' = summary(result[-1,]), 'plot' = p)
  }
  else{
    # Computing original metric
    col.scan = ANTs:::df.col.findId(df, scan)
    col.id = ANTs:::df.col.findId(df, id)
    gbi = df.to.gbi(df, scan = col.scan, id = col.id)
    M = assoc.indices(gbi, index)
    met = do.call(metric, list(M = M, ...))
    result = rep(list(met),nperm+1)
    
    # Bootstraping for each values declare by user
    for (a in 1:length(subsampling)) {
      tmp = df[-sample(1:nrow(df), percent[a], replace = FALSE),]
      gbi = df.to.gbi(tmp, scan = col.scan, id = col.id)
      M = assoc.indices(gbi, index)
      result[[a]] = do.call(metric, list(M = M))
    }
    
    # Merge result
    result = do.call(rbind, lapply(lapply(result, unlist), "[",
                                   unique(unlist(c(sapply(result,names))))))
    
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1,])
    stripchart(result[1,] ~ c(1:ncol(result)),  vertical = T,
               method = "jitter", pch = 21,
               col = "white", bg = "white", add= TRUE)  
    p = recordPlot()
    
    
    # Giving ANTs attribute for futures developement of analytical protocol
    attr(result,'ANT') = 'Bootsraping deletions'
    # Returning a list with 1) the different values of nodes metrics, 2) the summary of posterior distribution for each individuals node metric, 3) a boxplot with posterior distribution for each individuals node metric
    return = list('metrics' = result, 'summary' = summary(result[-1,]), 'plot' = p)
  }
 
}


