#' @title Metric uncertainty
#' @description Perform a matrix boostrapping approach to estimate the confidence intervals surrounding each pairwise association.

#' @param M a square adjacency matrix
#' @param df a data frame of individual inetaractions or associations
#' @param subsampling a vector of integers indicating the percentage of data to remove
#' @param metric the network metric to compute
#' 
#' @return 3 elements:
#' \itemize{
#' \item A matrix in which each columns represent a node metric variation throough bootstraping, with the first row representing the original metric.
#' \item A summary of bootstrap distribution for each nodes.
#' \item A plot of the 
#' }

#' @details This process evaluate networks metrics uncertainty by performing a boostrapp process with replacement on the matrix of association and recomputing the network metric of intereste.
#' @author Sebastian Sosa
#' @references Lusseau, D., Whitehead, H., & Gero, S. (2009). Incorporating uncertainty into the study of animal social networks. arXiv preprint arXiv:0903.1519.
#' @examples
#' test = sampling.met(sim.m, 100, metric = 'met.strength')
#' # objects return by the function
#' test$metrics
#' test$summary
#' test$plot
#' # Example with metric extra arguments
#' sampling.met(sim.m, 100, metric = 'met.affinity', binary = FALSE)
#' sampling.met(sim.m, 100, metric = 'met.affinity', binary = TRUE)
#' # Example with individuals asosciations
#' sampling.met(sim.grp, scan = c('location','time'), id='ID')
sampling.met <- function(df, nperm, metric = 'met.strength', assoc.indices = FALSE, actor = NULL, receiver = NULL, scan = NULL, id = NULL, index = 'sri', ...){
  if(assoc.indices){
    if(is.null(scan) | is.null(id)){stop("Arguments 'scan' and 'id' cannot be NULL.")}
    # Computing metric in the original network
    gbi =  df.to.gbi(df, scan, id)
    M = assoc.indices(gbi,index =index)
    met = do.call(metric, list(M = M, ...))
    
    df = grp.to.edgl(df, scan, id)
    
    # Preparing result
    result = rep(list(met),nperm+1)
    
    # Extracting individuals for result object
    names = colnames(M)
    
    # For each permutations. PB here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (a in 2:(nperm+1)) {
      # Sample data frames rows with resampling
      idx = sample(x = 1:nrow(df), size = nrow(df), replace = TRUE)
      
      # Create a new data frame with those samples
      tmp = df[idx,]
      
      # Convertion into linear mode
      tmp = edgl.to.grp(tmp, id1 = 1, id2 = 2, scan = 3:ncol(tmp))
      
      # Compute the network metric declare by user
      gbi =  df.to.gbi(tmp, id = 1, scan = 2:ncol(tmp))
      M = assoc.indices(gbi, index = index)
      cat(do.call(metric, list(M = M)), '\n')
      result[[a]] = do.call(metric, list(M = M, ...))
    }
    
    # Merge result
    result = do.call(rbind, lapply(lapply(result, unlist), "[",names))
  }
  else{
    if(is.null(actor) | is.null(receiver)){stop("Arguments 'actor' and 'receiver' cannot be NULL.")}
    # Computing metric in the original network
    M = df.to.mat(df, actor = actor, receiver = receiver)
    met = do.call(metric, list(M = M, ...))
    result = rep(list(met),nperm+1)
    
    # Extracting individuals for result object
    names = colnames(M)
    
    # For each permutations
    for (a in 2:(nperm+1)) {
      # Sample data frames rows with resampling
      idx = sample(x = 1:nrow(df), size = nrow(df), replace = TRUE)
      
      # Create a new data frame with those samples
      tmp = df[idx,]
      
      # Compute the network metric declare by user
      M = df.to.mat(tmp, actor = actor, receiver = receiver)
      result[[a]] = do.call(metric, list(M = M, ...))
    }
    
    # Merge result
    result = do.call(rbind, lapply(lapply(result, unlist), "[",names))
    
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1,])
    stripchart(result[1,] ~ c(1:ncol(result)),  vertical = T,
               method = "jitter", pch = 21,
               col = "white", bg = "white", add= TRUE)  
    p = recordPlot()
    
    # Giving ANTs attribute for futures developement of analytical protocol
    attr(result,'ANT') = 'Bootsraping interactions'
  }

  # Returning a list with 1) the different values of nodes metrics, 2) the summary of posterior distribution for each individuals node metric, 3) a boxplot with posterior distribution for each individuals node metric
  return = list('metrics' = result, 'summary' = summary(result[-1,]), 'plot' = p)
}
