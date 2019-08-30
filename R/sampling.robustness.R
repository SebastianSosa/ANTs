#' @title Metric robustness
#' @description Performs a matrix boostrapping approach to estimate the confidence intervals surrounding each pairwise association.
#' @param df a data frame of individual associations
#' @param subsampling a vector of integers indicating the percentage of data to remove
#' @param metric the network metric to compute
#' @param assoc.indices a bolean indicating if association indices must be used
#' @param actor If argument assoc.indices is FALSE, fill this argument, an integer or a string indicating the column of the individuals performing the behaviour.
#' @param receiver If argument assoc.indices is FALSE, fill this argument, an integer or a string indicating the column of the individuals receiving the behaviour.
#' @param scan If argument assoc.indices is TRUE, fill this argument, a numeric or character vector representing one or more columns used as scan factors.
#' @param id If argument assoc.indices is TRUE, fill this argument, a numeric or character vector indicating the column holding ids of individuals.
#' #' @param index a string indicating the association index to compute:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @return 3 elements:
#' \itemize{
#' \item A matrix in which each columns represent a node metric variation throough bootstraping, with the first row representing the original metric.
#' \item A summary of bootstrap distribution for each nodes.
#' \item A plot of metric variations through deletions
#' }
#' @details This process evaluates network metrics robustness by performing a boostrap process to remove a certain percent of the data collected and recomputing the network metric of interest. This has been used in Balasubramaniam et al. 2018 to assess sensitivty of sampling effort of global network metrics in primate networks.
#' @author Sebastian Sosa
#' @references Balasubramaniam, K. N., Beisner, B. A., Berman, C. M., De Marco, A., Duboscq, J., Koirala, S., ... & Ogawa, H. (2018). The influence of phylogeny, social style, and sociodemographic factors on macaque social network structure. American journal of primatology, 80(1), e22727.
#' @examples
#' Example for node measures
#' test <- sampling.robustness(sim.focal.directed, actor = "actor", receiver = "receiver", metric = "met.strength")
#' 
#' # objects returned by the function
#' test$metrics
#' test$summary
#' test$plot
#' 
#' # Examples with metric extra arguments
#' sampling.robustness(sim.focal.directed, actor = "actor", receiver = "receiver", metric = "met.affinity")
#' sampling.robustness(sim.focal.directed, actor = "actor", receiver = "receiver", metric = "met.affinity", binary = FALSE)
#' 
#' # Examples with association data
#' test2 <- sampling.robustness(df = sim.grp, assoc.indices = TRUE, scan = c("time", "location"), id = "ID", metric = "met.strength")
#' 
#' # Example of how to test global network metric robustness by removing 10% of the observations simulated 100 times
#' test <- sampling.robustness(sim.focal.directed, subsampling = rep(10, 100), actor = "actor", receiver = "receiver", metric = "met.diameter")
sampling.robustness <- function(df, subsampling = c(5, 10, 20, 30, 40, 50), metric = "met.strength",
                                assoc.indices = FALSE, actor, receiver, scan, id, index = "sri", ...) {
  # Compute percentages
  percent <- (subsampling * nrow(df)) / 100

  if (!assoc.indices) {
    if(is.null(actor) | is.null(receiver)){stop("Arguments 'actor' and 'receiver' cannot be NULL.")}
    # Compute original metric
    col.actor <- ANTs:::df.col.findId(df, actor)
    col.receiver <- ANTs:::df.col.findId(df, receiver)
    M <- df.to.mat(df, actor = col.actor, receiver = col.receiver)
    met <- do.call(metric, list(M = M, ...))
    names <- colnames(M)
    result <- rep(list(met), length(subsampling) + 1)

    # Bootstrapping for each value declared by the user
    for (a in 1:length(subsampling)) {
      cat("Processing bootstrap : ", a, "\r")
      tmp <- df[-sample(1:nrow(df), percent[a], replace = FALSE), ]
      M <- df.to.mat(tmp, actor = col.actor, receiver = col.receiver)
      result[[a + 1]] <- do.call(metric, list(M = M))
    }
  }
  else {
    if(is.null(scan) | is.null(id)){stop("Arguments 'scan' and 'id' cannot be NULL.")}
    # Compute original metric
    col.scan <- ANTs:::df.col.findId(df, scan)
    col.id <- ANTs:::df.col.findId(df, id)
    gbi <- df.to.gbi(df, scan = col.scan, id = col.id)
    M <- assoc.indices(gbi, index)
    met <- do.call(metric, list(M = M, ...))
    names <- colnames(M)
    result <- rep(list(met), length(subsampling) + 1)

    # Bootstrapping for each value declared by the user
    for (a in 1:length(subsampling)) {
      cat("Processing bootstrap : ", a, "\r")
      tmp <- df[-sample(1:nrow(df), percent[a], replace = FALSE), ]
      gbi <- df.to.gbi(tmp, scan = col.scan, id = col.id)
      M <- assoc.indices(gbi, index)
      result[[a + 1]] <- do.call(metric, list(M = M, ...))
    }
  }
  
  # Merging and returning result
  # Node network measures
  if (length(met) > 1){
    result <- do.call(rbind, lapply(result, "[", names))
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1, ], ylim=c(min(result, na.rm = TRUE), max(result, na.rm = TRUE)))
    stripchart(result[1, ]~c(1:ncol(result)),
               vertical = T,
               method = "jitter", pch = 21,
               col = "white", bg = "white", add = TRUE
    )
    p <- recordPlot()
    # Give ANTs attribute for future developement of analytical protocol
    attr(result, "ANT") <- "Bootsraping deletions"
    # Return a list with 1) the different values of node metrics, 2) the summary of posterior distribution for each individual node metric, 3) a boxplot with posterior distribution for each individual node metric
    return <- list("metrics" = result, "summary" = summary(result[-1, ]), "plot" = p)
  }
  # Network global measures
  else{
    result <- unlist(result)
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1], ylim=c(min(result, na.rm = TRUE), max(result, na.rm = TRUE)))
    stripchart(result[1],
               vertical = T,
               method = "jitter", pch = 21,
               col = "white", bg = "white", add = TRUE
    )
    p <- recordPlot()
    # Give ANTs attribute for future developement of analytical protocol
    attr(result, "ANT") <- "Bootsraping deletions"
    # Return a list with 1) the different values of node metrics, 2) the summary of posterior distribution for each individual node metric, 3) a boxplot with posterior distribution for each individual node metric
    return <- list("metrics" = result, "summary" = summary(result[-1]), "plot" = p)
  }
}
