#' @title  Sampling effort
#' @description Visualize metric variation through period of observations
#' @param df a data frame of interactions or associations.
#' @param col.time an integer or string indicating the column with the time/period information
#' @param cumulative a bolean, if TRUE, it computes the argument metric declared for each step of periods keeping previous periods
#' @param metric a string to call an ANTs function of class 'met.XXX'.
#' @param assoc.indices a bolean, if TRUE, it creates matrices of associations according to argument 'index' and argument 'df' must be a data frame of associations, see df.to.gbi.
#' Otherwise, it creates  a matrix of interactions and argument 'df' must be a data frame of interactions type (see df.to.mat).
#' @param actor an integer or a string indicating the column of the individuals performing the behaviour. This argument must be declared if argument 'assoc.indices' is equal to FALSE.
#' @param receiver an integer or a string indicating the column of the individuals receiving the behaviour. This argument must be declared if argument 'assoc.indices' is equal to FALSE.
#' @param sym a boolean if true, interactions or associations are considered symmetric. This argument must be declared if argument 'assoc.indices' is equal to FALSE.
#' @param scan a numeric or character vector representing one or more columns used as scan factors. This argument must be declared if argument 'assoc.indices' is equal to FALSE.
#' @param id a numeric or character vector indicating the column holding ids of individuals.
#' @param index a string indicating the association index to compute:
#' @param ... additional argument related to the computation of the metric declared.

#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @return A list of two elemnts:
#' \itemize{
#' \item 'df', a data frame with metric evolution through time
#' \item plot a plot of the metric evolution through time
#' }
#' @details This function allows to visualize metric (nodal and global) variation through periods of observation. Studies have highlighted the need to assess their stability. 
#' Metric stability can be assessed by a sigmoide curve reaching a plateau. While the function doesn't give you any statistical test, it allows to visualize if the plateau is reached or not.
#' For this approach, argument cumulative must be set to TRUE.
#' @author Sebastian Sosa
#' @references 
#' Farine, D. R., & Strandburg-Peshkin, A. (2015). Estimating uncertainty and reliability of social network data using Bayesian inference. Royal Society open science, 2(9), 150367.
#' @examples 
#' df <- sim.focal.directed
#' df$period <- rep(c("a", "b", "c", "d", "e"))

#' # Node measures non cumulative example 
#' sampling.effort(df, col.time = "period", cumulative = FALSE,
#'                 metric = "met.strength", actor = "actor", receiver = "receiver")
#' 
#' # Node measures cumulative example
#' sampling.effort(df, col.time = "period", cumulative = TRUE,
#'                 metric = "met.strength", actor = "actor", receiver = "receiver")
#' 
#' # Node measures with extra arguments example
#' sampling.effort(df, col.time = "period", actor = "actor",
#'                receiver = "receiver", metric = "met.affinity")
#'  
#' sampling.effort(df, col.time = "period", actor = "actor",
#'                 receiver = "receiver", metric = "met.affinity", binary = TRUE)
#' 
#' # Example of how to test global network metric with non cumulative version
#' sampling.effort(df, col.time = "period", cumulative = FALSE,
#'                 metric = "met.density", actor = "actor", receiver = "receiver")
#' 
#' # Example of how to test global network metric with cumulative version
#' sampling.effort(df, col.time = "period", cumulative = TRUE,
#'                 metric = "met.density", actor = "actor", receiver = "receiver")
#' 
#' # Same example with gambit of the group data collection protocol--------
#' # Node measures non cumulative example
#' sampling.effort(sim.grp, col.time = "day", cumulative = TRUE,
#'                  metric = "met.strength", assoc.indices = TRUE,
#'                  scan = c("time", "location"), id = "ID", index = "sri")
#' 
#' # Node measures non cumulative example
#' sampling.effort(sim.grp, col.time = "day", cumulative = FALSE,
#'                 metric = "met.strength", assoc.indices = TRUE, 
#'                 scan = c("time", "location"), id = "ID", index = "sri" )
#' 
#' # Node measures with extra arguments example
#' sampling.effort(sim.grp, col.time = "day", cumulative = FALSE, 
#'                 metric = "met.affinity", assoc.indices = TRUE, 
#'                 scan = c("time", "location"), id = "ID", index = "sri")
#'                 
#' sampling.effort(sim.grp, col.time = "day", cumulative = FALSE, 
#'                 metric = "met.affinity", assoc.indices = TRUE, 
#'                 scan = c("time", "location"), id = "ID", 
#'                 index = "sri", binary = TRUE)
#' 
#' # Example of how to test global network metric with non cumulative version
#' sampling.effort(df = sim.grp, col.time = "day", cumulative = FALSE,
#'                 metric = "met.density",assoc.indices = TRUE, 
#'                 scan = c("time", "location"), id = "ID", index = "sri")
#' 
#' # Example of how to test global network metric with cumulative version
#' sampling.effort(df = sim.grp, col.time = "day", cumulative = TRUE, 
#'                 metric = "met.density", assoc.indices = TRUE,
#'                 scan = c("time", "location"), id = "ID", index = "sri")
#' 
sampling.effort <- function(df, col.time, cumulative = TRUE, metric = "met.strength",
                            assoc.indices = FALSE, actor = NULL, receiver = NULL, sym = FALSE, scan = NULL, id = NULL, index = "sri", ...) {
  # Finding col.time column
  col.time <- df.col.findId(df, col.time)
  
  # Ordering data frame according to time
  df <- df[order(df[, col.time]), ]
  
  # finding unique times
  time.window <- unique(df[, col.time])
  
  if (assoc.indices) {
    if(is.null(scan) | is.null(id)){stop("Arguments 'scan' and 'id' cannot be NULL.")}
    # finding scan column
    col.scan <- df.col.findId(df, scan)
    
    # finding ids column
    col.id <- df.col.findId(df, id)
    
    # Data frame to GBI
    gbi <- df.to.gbi(df, scan = col.scan, id = col.id)
    
    # GBI to Matrix of association according to user  'index' argument delcaration
    M <- assoc.indices(gbi, index)
    
    # Computing metric declare by user
    met <- do.call(metric, list(M = M, ...))
    
    # Extracting names of indiviudals
    names <- data.frame("names" = colnames(M))
    
    # Case of node metric
    if (length(met) > 1) {
      result <- NULL
      
      # For each time window 
      for (a in 1:length(time.window)) {

        # If argument cumulative is TRUE we extract the observations for this time window plus the previous time windows
        if (cumulative){
          tmp <- df[df[, col.time] %in% time.window[1:a], ]
        }
        # Else we extract only the current time window
        else{
          tmp <- df[df[, col.time] %in% time.window[a], ]
        }
        
        # If their is observations, we will compute the metric declare by user
        if (nrow(tmp) > 0) {
          # Data frame to GBI
          gbi <- df.to.gbi(tmp, scan = col.scan, id = col.id)
          # GBI to Matrix of association according to user  'index' argument delcaration
          M <- assoc.indices(gbi, index)
          
          # Computing metric declare by user
          r1 <- do.call(metric, list(M = M, ...))
          
          # storing result in a data frame
          r2 <- merge(data.frame("names" = names), data.frame("names" = names(r1), "met" = r1), by = "names", all = T)
          r2$period <- time.window[a]
          result <- rbind(result, r2)
        }
        
        # If their is no observaitons for this time window we fill result with NA
        else {
          r1 <- data.frame("names" = names(r1), "met" = rep(NA, length(names)))
          r2 <- merge(data.frame("names" = names), r1, by = "names", all = T)
          r2$period <- time.window[a]
          result <- rbind(result, r2)
        }
      }
      
      # Ploting global metrics
      interaction.plot(result$period, result$names, result$met,
                       col = heat.colors(ncol(result)),
                       xlab = colnames(df)[col.time], ylab = metric
      )
      
      # Record plot
      p <- recordPlot()
    }
    # Case of global metric
    else {
      result <- NULL
      # For each time window 
      for (a in 1:length(time.window)) {

        # If argument cumulative is TRUE we extract the observations for this time window plus the previous time windows
        if (cumulative){
          tmp <- df[df[, col.time] %in% time.window[1:a], ]
        }
        
        # Else we extract only the current time window
        else{
          tmp <- df[df[, col.time] %in% time.window[a], ]
        }
        
        # If their is observations, we will compute the metric declare by user
        if (nrow(tmp) > 0) {
          # Data frame to GBI
          gbi <- df.to.gbi(tmp, scan = col.scan, id = col.id)
          # GBI to Matrix of association according to user  'index' argument delcaration
          tmp.M <- assoc.indices(gbi, index)
          
          # Increasing matrix size to have the same dimention of the original matrix
          if(ncol(tmp.M) < ncol(M)){
            diff = ncol(M) - ncol(tmp.M)
            tmp.M = cbind(tmp.M, matrix(0, nrow = ncol(tmp.M), ncol = diff))
            tmp.M = rbind(tmp.M, matrix(0, ncol = ncol(tmp.M), nrow = diff))
          }
          
          # Computing metric declare by user
          r2 <- data.frame(do.call(metric, list(M = tmp.M,...)), time.window[a])
          
          # storing result in a data frame
          colnames(r2) <- c("metric", "period")
          result <- rbind(result, r2)
        }
        else {
          r2 <- merge("metric" = NA, "period" = time.window[a])
          result <- rbind(result, r2)
        }
      }
      
      # Ploting global metrics
      plot(result$met, type = "l", xaxt='n',
           xlab = colnames(df)[col.time], ylab = metric, main = paste(metric, "evolution through period of time")
      )
      axis(1, at = 1:nrow(result), labels = result$period)
      # Record plot
      p <- recordPlot()
    }
  }
  else {
    if(is.null(actor) | is.null(receiver)){stop("Arguments 'actor' and 'receiver' cannot be NULL.")}
    # finding actor column
    col.actor <- df.col.findId(df, actor)
    
    # finding receiver column
    col.receiver <- df.col.findId(df, receiver)
    
    # Data frame to matrix
    M = df.to.mat(df = df, actor = actor, receiver = receiver, sym = sym)
    
    # Computing metric declare by user
    met <- do.call(metric, list(M = M, ...))
    
    # Extracting names of indiviudals
    names <- data.frame("names" = colnames(M))
    
    # Case of node metric
    if (length(met) > 1) {
      result <- NULL
      
      # For each time window 
      for (a in 1:length(time.window)) {
        
        # If argument cumulative is TRUE we extract the observations for this time window plus the previous time windows
        if (cumulative){
          tmp <- df[df[, col.time] %in% time.window[1:a], ]
        }
        # Else we extract only the current time window
        else{
          tmp <- df[df[, col.time] %in% time.window[a], ]
        }
        # If their is observations, we will compute the metric declare by user
        if (nrow(tmp) > 0) {
          # Computing metric declare by user
          r1 <- do.call(metric, list(M = df.to.mat(df = tmp, actor = actor, receiver = receiver, sym = sym), ...))
          
          # storing result in a data frame
          r2 <- merge(data.frame("names" = names), data.frame("names" = names(r1), "met" = r1), by = "names", all = T)
          r2$period <- time.window[a]
          result <- rbind(result, r2)
        }
        
        # If their is no observaitons for this time window we fill result with NA
        else {
          r1 <- data.frame("names" = names(r1), "met" = rep(NA, length(names)))
          r2 <- merge(data.frame("names" = names), r1, by = "names", all = T)
          r2$period <- time.window[a]
          result <- rbind(result, r2)
        }
      }
      
      # Ploting individuals metrics for each time window
      interaction.plot(result$period, result$names, result$met,
                       col = heat.colors(10),
                       xlab = colnames(df)[col.time], ylab = metric
      )
      
      # Record plot
      p <- recordPlot()
    }
    # Case of global metric
    else {
      result <- NULL
      
      # For each time window 
      for (a in 1:length(time.window)) {

        # If argument cumulative is TRUE we extract the observations for this time window plus the previous time windows
        if (cumulative){
          tmp <- df[df[, col.time] %in% time.window[1:a], ]
        }
        # Else we extract only the current time window
        else{
          tmp <- df[df[, col.time] %in% time.window[a], ]
        }
        
        # If their is observations, we will compute the metric declare by user
        if (nrow(tmp) > 0) {
          # data frame to matrix
          tmp.M = df.to.mat(df = tmp, actor = actor, receiver = receiver, sym = sym)
          
          # Increasing matrix size to have the same dimention of the original matrix
          if(ncol(tmp.M) < ncol(M)){
            diff = ncol(M) - ncol(tmp.M)
            tmp.M = cbind(tmp.M, matrix(0, nrow = ncol(tmp.M), ncol = diff))
            tmp.M = rbind(tmp.M, matrix(0, ncol = ncol(tmp.M), nrow = diff))
          }
          
          # Computing metric
          r2 <- data.frame(do.call(metric, list(M = tmp.M, ...)), time.window[a])
          
          # storing result in a data frame
          colnames(r2) <- c("metric", "period")
          result <- rbind(result, r2)
        }
        
        # If their is no observaitons for this time window we fill result with NA
        else {
          r2 <- merge("metric" = NA, "period" = time.window[a])
          result <- rbind(result, r2)
        }
      }
      
      # Ploting individuals metrics for each time window
      plot(result$met,type = "l", xaxt='n',
           xlab = colnames(df)[col.time], ylab = metric, main = paste(metric, "evolution through period of time")
      )
      axis(1, at = 1:nrow(result), labels = result$period)
      
      # Record plot
      p <- recordPlot()
    }
  }
  return(list("df" = result, "plot" = p))
}
