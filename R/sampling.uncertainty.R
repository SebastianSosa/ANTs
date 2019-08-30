#' @title Metric uncertainty
#' @description Perform a matrix boostrapping approach to estimate the confidence intervals surrounding each pairwise association.

#' @param df a data frame of individual interactions or associations
#' @param subsampling a vector of integers indicating the percentage of data to remove
#' @param metric the network metric to compute
#' @param assoc.indices a bolean indicating if association indices must be used
#' @param actor If argument assoc.indices is FALSE, fill this argument, an integer or a string indicating the column of the individuals performing the behaviour.
#' @param receiver If argument assoc.indices is FALSE, fill this argument, an integer or a string indicating the column of the individuals receiving the behaviour.
#' @param scan If argument assoc.indices is TRUE, fill this argument, a numeric or character vector representing one or more columns used as scan factors.
#' @param id If argument assoc.indices is TRUE, fill this argument, a numeric or character vector indicating the column holding ids of individuals.
#' @param index a string indicating the association index to compute:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @return 3 elements:
#' \itemize{
#' \item A matrix in which each column represents a node metric variation through bootstrapping, with the first row representing the original metric.
#' \item A summary of bootstrap distribution for each node.
#' \item A plot of metric variations through bootstrap
#' }

#' @details This process evaluates network metrics uncertainty by performing a boostrap with replacement on the data frame of associations and recomputing the network metric of interest.
#' @author Sebastian Sosa
#' @references Lusseau, D., Whitehead, H., & Gero, S. (2009). Incorporating uncertainty into the study of animal social networks. arXiv preprint arXiv:0903.1519.
#' @examples
#' test <- sampling.uncertainty(df = sim.focal.directed, nboot = 100, actor = "actor", receiver = "receiver", metric = "met.strength")
#'
#' # objects returned by the function
#' test$metrics
#' test$summary
#' test$plot
#'
#' # Example with metric extra arguments
#' sampling.uncertainty(df = sim.focal.directed, nboot = 100, actor = "actor", receiver = "receiver", metric = "met.affinity", binary = FALSE)
#' sampling.uncertainty(df = sim.focal.directed, nboot = 100, actor = "actor", receiver = "receiver", metric = "met.affinity", binary = TRUE)
#'
#' # Example with individual associations
#' sampling.uncertainty(df = sim.grp, nboot = 100, assoc.indices = TRUE, scan = c("day", "location", "time"), id = "ID")
sampling.uncertainty <- function(df, nboot, metric = "met.strength", assoc.indices = FALSE, actor = NULL, receiver = NULL, scan = NULL, id = NULL, index = "sri", ...) {
  if (assoc.indices) {
    if (is.null(scan) | is.null(id)) {
      stop("Arguments 'scan' and 'id' cannot be NULL.")
    }
    # Compute metric in the original network
    gbi <- df.to.gbi(df, scan, id)
    M <- assoc.indices(gbi, index = index)
    met <- do.call(metric, list(M = M, ...))

    # Extract individuals for result object
    names <- colnames(M)

    df <- grp.to.edgl(df, scan, id)

    # Preparing result
    result <- rep(list(met), nboot + 1)

    # Perform bootstrapp
    for (a in 1:(nboot + 1)) {
      cat("Processing bootstrap : ", a, "\r")
      # Sample data frame rows with resampling
      idx <- sample(x = 1:nrow(df), size = nrow(df), replace = TRUE)

      # Create a new data frame with those samples
      tmp <- df[idx, ]

      # Convertion into linear mode
      tmp <- edgl.to.grp(tmp, id1 = 1, id2 = 2, scan = 3:ncol(tmp))

      # Compute the network metric declared by the user
      gbi <- df.to.gbi(tmp, id = 1, scan = 2:ncol(tmp))
      M <- assoc.indices(gbi, index = index)
      result[[a + 1]] <- do.call(metric, list(M = M, ...))
    }
  }
  else {
    if (is.null(actor) | is.null(receiver)) {
      stop("Arguments 'actor' and 'receiver' cannot be NULL.")
    }
    # Compute metric in the original network
    M <- df.to.mat(df, actor = actor, receiver = receiver)
    met <- do.call(metric, list(M = M, ...))
    result <- rep(list(met), nboot + 1)

    # Extract individuals for result object
    names <- colnames(M)

    # Perform bootstrapp
    for (a in 1:(nboot + 1)) {
      cat("Processing bootstrap : ", a, "\r")
      # Sample data frame rows with resampling
      idx <- sample(x = 1:nrow(df), size = nrow(df), replace = TRUE)

      # Create a new data frame with those samples
      tmp <- df[idx, ]

      # Compute the network metric declared by the user
      M <- df.to.mat(tmp, actor = actor, receiver = receiver)
      result[[a + 1]] <- do.call(metric, list(M = M, ...))
    }
  }

  # Merging and returning result
  # Node network measures
  if (length(met) > 1) {
    result <- do.call(rbind, lapply(result, "[", names))
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1, ], ylim = c(min(result, na.rm = TRUE), max(result, na.rm = TRUE)))
    stripchart(result[1, ] ~ c(1:ncol(result)),
      vertical = T,
      method = "jitter", pch = 21,
      col = "white", bg = "white", add = TRUE
    )
    p <- recordPlot()
    # Give ANTs attribute for future development of analytical protocol
    attr(result, "ANT") <- "Bootsraping deletions"
    # Return a list with 1) the different values of node metrics, 2) the summary of posterior distribution for each individual node metric, 3) a boxplot with posterior distribution for each individual node metric
    return <- list("metrics" = result, "summary" = summary(result[-1, ]), "plot" = p)
  }
  # Network global measures
  else {
    result <- unlist(result)
    # Plot results
    par(bg = "gray63")
    boxplot(result[-1], ylim = c(min(result, na.rm = TRUE), max(result, na.rm = TRUE)))
    stripchart(result[1],
      vertical = T,
      method = "jitter", pch = 21,
      col = "white", bg = "white", add = TRUE
    )
    p <- recordPlot()
    # Give ANTs attribute for future development of analytical protocol
    attr(result, "ANT") <- "Bootsraping deletions"
    # Return a list with 1) the different values of node metrics, 2) the summary of posterior distribution for each individual node metric, 3) a boxplot with posterior distribution for each individual node metric
    return <- list("metrics" = result, "summary" = summary(result[-1]), "plot" = p)
  }
}
