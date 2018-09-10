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

#' @title ANT diagnostic for permuted statistics
#' @param x an ANT object originating from the statistical functions of class 'stat.'
#' @description ANT method to make a diagnostic of all ANT permutation tests. This method adapts the diagnostic results according to the data input. The
#' output is adapted to the type of test run. However, some outputs are common to all tests.
#' @return
#' A list of two elements fot stat.cor, stat.t functions:
#' \itemize{
#' \item A data frame with the permuted p-values (left side and right side), the confidence interval (25, 50 or 95) and the mean of the posterior distribution of the statistics of interest according to the statistical test (coefficient of correlation, t of students, or estimate(s))
#' \item An histogram of the posterior distribution of the statistics of interest according to the statistical test.
#' }
#' #' A list of four elements fot stat.lm, stat.glm, stat.glmm functions:
#' \itemize{
#' \item A data frame with the orginal stats of the model, and permuted p-values (left side and right side), the confidence interval (25, 50 or 95) and the mean of the posterior distribution of the statistics of interest according to the statistical test (coefficient of correlation, t of students, or estimate(s))
#' \item Diagnostic plot of the original model
#' \item An histogram of the posterior distribution of the statistics of interest according to the statistical test.
#' \item a vector of the permutations that generates errors and for which new permutations were performed.
#' }
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
setGeneric(name = "ant", ant <- function(x) {
  if (!is.null(attr(x, "class"))) {
    if (attr(x, "class") == "ant cor") {
      obs <- x[, 1][1]
      v_perm <- x[, 1][-1]
      p_valuevalue_left_side <- sum(v_perm < obs) / length(v_perm)
      p_valuevalue_right_side <- sum(v_perm > obs) / length(v_perm)
      diag <- list()
      p <- c(p_valuevalue_left_side, p_valuevalue_right_side)
      stat.ci <- stat.ci(v_perm)
      m <- mean(v_perm)
      df <- as.data.frame(cbind(obs, p[1], p[2], stat.ci[1], stat.ci[2], m))
      colnames(df) <- (c("Observed correlation", "p left", "p right", "95ci lower", "95ci upper", "mean"))
      rownames(df) <- c("statistics")
      diag$statistics <- df
      par(bg = "gray63")
      if (obs > m) {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = F))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray10", "gray25")
        plot(h, col = cuts, border = cuts, xlab = paste(attributes(x)$comment), main = paste(attributes(x)$comment, "posterior distribution"), xaxt = "n")
        axis(1, pos = -30)
        mtext(1, text = round(obs, digit = 3), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      else {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = F))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray25", "gray10")
        plot(h, col = cuts, border = cuts, xlab = paste(attributes(x)$comment), main = paste(attributes(x)$comment, "posterior distribution"), xaxt = "n")
        axis(1, pos = -10)
        mtext(1, text = round(obs, digit = 3), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      diag$post.dist <- p
      dev.off()
      dev.new()
      cat("Correlation test for", length(v_perm), "perm :", "\n")
      cat("Observed correlation: ", obs, "\n")
      cat("P-values and effect sizes: \n")
      print(df)
      invisible(return(diag))
    }
    if (attr(x, "class") == "ant t-test") {
      v <- do.call("rbind", x[[2]][, 1])
      obs <- x[[1]]$statistic
      v_perm <- v[, 1]
      p_valuevalue_left_side <- sum(v_perm < obs) / length(v_perm)
      p_valuevalue_right_side <- sum(v_perm > obs) / length(v_perm)
      diag <- list()
      p <- c(p_valuevalue_left_side, p_valuevalue_right_side)
      stat.ci <- stat.ci(v_perm)
      m <- mean(v_perm)
      df <- as.data.frame(cbind(obs, p[1], p[2], stat.ci[1], stat.ci[2], m))
      colnames(df) <- (c("t observed", "p left", "p right", "95ci lower", "95ci upper", "mean"))
      rownames(df) <- c("statistics")
      diag$statistics <- df
      par(bg = "gray63")
      if (obs > m) {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = FALSE))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray10", "gray25")
        plot(h, xlab = "t coefficient", main = paste("Student t posterior distribution for", paste(attr(x, "alternative")), paste(attr(x, "comment")), "permuted test"), xaxt = "n", col = cuts, border = cuts)
        axis(1, pos = -10)
        mtext(1, text = round(obs, digit = 2), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      else {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = FALSE))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray25", "gray10")
        plot(h, xlab = "t coefficient", main = paste("Student t posterior distribution for", paste(attr(x, "alternative")), paste(attr(x, "comment")), "permuted test"), xaxt = "n", col = cuts, border = cuts)
        axis(1, pos = -10)
        mtext(1, text = round(obs, digit = 2), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      diag$post.dist <- p
      dev.off()
      dev.new()
      cat("t test for", length(v_perm), "perm :", "\n")
      cat("t observed: ", obs, "\n")
      cat("P-values and effect sizes: \n")
      print(df)
      dev.off()
      invisible(return(diag))
    }
    if (attr(x, "class") == "ant lm" | attr(x, "class") == "ant glm" | attr(x, "class") == "ant glmm") {
      # P values and model summary
      s <- x$Original.model
      obs <- x$Original.model$coefficients[, 1]
      v_perms <- x$permutations
      stat <- NULL
      for (a in 1:ncol(v_perms)) {
        r <- stat.ci(v_perms[, a])
        stat[[a]] <- data.frame(
          sum(v_perms[, a] < obs[a]) / length(v_perms[, a]),
          sum(v_perms[, a] > obs[a]) / length(v_perms[, a]),
          r[1], r[2],
          mean(v_perms[, a])
        )
      }
      stat <- do.call("rbind", stat)
      colnames(stat) <- c("p.left", "p.rigth", "lower.ci", "uper.ci", "mean")
      rownames(stat) <- colnames(v_perms)
      s$coefficients <- cbind(s$coefficients, stat)
      # Posterior distribution histogrames
      post.dist <- post.dist(v_perms, Obs = obs)
      # Original model diagnostic
      attr(x$Original.model, "family") <- attributes(x)$family
      attr(x$Original.model, "formula") <- attributes(x)$formula
      diagnostic <- stat.model.diag(x$Original.model)
      diag <- list()
      diag[[1]] <- s
      diag[[2]] <- diagnostic
      diag[[3]] <- post.dist
      names(diag) <- c("model", "model.diagnostic", "post.dist")
      invisible(return(diag))
    }
    if (attr(x, "class") == "ant assortativity single matrix") {
      obs <- x[, 1][1]
      v_perm <- x[, 1][-1]
      p_valuevalue_left_side <- sum(v_perm < obs) / length(v_perm)
      p_valuevalue_right_side <- sum(v_perm > obs) / length(v_perm)
      diag <- list()
      p <- c(p_valuevalue_left_side, p_valuevalue_right_side)
      stat.ci <- stat.ci(v_perm)
      m <- mean(v_perm)
      df <- as.data.frame(cbind(obs, p[1], p[2], stat.ci[1], stat.ci[2], m))
      colnames(df) <- (c("Observed correlation", "p left", "p right", "95ci lower", "95ci upper", "mean"))
      rownames(df) <- c("statistics")
      diag$statistics <- df
      par(bg = "gray63")
      if (obs > m) {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = F))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray10", "gray25")
        plot(h, col = cuts, border = cuts, xlab = paste(attributes(x)$comment), main = paste(attributes(x)$comment, "posterior distribution"), xaxt = "n")
        axis(1, pos = -30)
        mtext(1, text = round(obs, digit = 4), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      else {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = F))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray25", "gray10")
        plot(h, col = cuts, border = cuts, xlab = paste(attributes(x)$comment), main = paste(attributes(x)$comment, "posterior distribution"), xaxt = "n")
        axis(1, pos = -10)
        mtext(1, text = round(obs, digit = 4), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      diag$post.dist <- p
      dev.off()
      dev.new()
      cat("assortativity permuted test", length(v_perm), "perm :", "\n")
      cat("Observed assortativity: ", obs, "\n")
      cat("P-values and effect sizes: \n")
      print(df)
      dev.off()
      invisible(return(diag))
    }
    if (is.data.frame(x)) {
      # P values and model summary
      obs <- x[1, ]
      v_perms <- x[-1, ]
      stat <- NULL
      for (a in 1:ncol(v_perms)) {
        r <- stat.ci(v_perms[, a])
        stat[[a]] <- data.frame(
          sum(v_perms[, a] < obs[a]) / length(v_perms[, a]),
          sum(v_perms[, a] > obs[a]) / length(v_perms[, a]),
          r[1], r[2],
          mean(v_perms[, a])
        )
      }
      stat <- do.call("rbind", stat)
      colnames(stat) <- c("p.left", "p.rigth", "lower.ci", "uper.ci", "mean")
      rownames(stat) <- colnames(v_perms)

      # Posterior distribution histogrames
      post.dist <- post.dist(v_perms, obs)

      diag <- list()
      diag[[1]] <- stat
      diag[[2]] <- post.dist
      names(diag) <- c("diagnostics", "post.dist")
      invisible(return(diag))
    }
    stop("Argument x is not an object of class 'ant cor', 'ant t-test', 'ant lm', 'ant glm', 'ant glmm' or a data frame.")
  }
  else {
    if (is.vector(x)) {
      obs <- x[1]
      v_perm <- x[-1]
      p_valuevalue_left_side <- sum(v_perm < obs) / length(v_perm)
      p_valuevalue_right_side <- sum(v_perm > obs) / length(v_perm)
      diag <- list()
      p <- c(p_valuevalue_left_side, p_valuevalue_right_side)
      stat.ci <- stat.ci(v_perm)
      m <- mean(v_perm)
      df <- as.data.frame(cbind(obs, p[1], p[2], stat.ci[1], stat.ci[2], m))
      colnames(df) <- (c("Observed value", "p left", "p right", "95ci lower", "95ci upper", "mean"))
      rownames(df) <- c("statistics")
      diag$statistics <- df
      par(bg = "gray63")
      if (obs > m) {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = F))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray10", "gray25")
        plot(h, col = cuts, border = cuts, xlab = "statistic value", main = "Statistic value posterior distribution", xaxt = "n")
        axis(1, pos = -30)
        mtext(1, text = round(obs, digit = 3), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      else {
        h <- suppressWarnings(hist(v_perm, breaks = length(v_perm), xaxt = "n", plot = F))
        cuts <- cut(h$breaks, c(obs, Inf))
        cuts <- ifelse(is.na(cuts), "gray25", "gray10")
        plot(h, col = cuts, border = cuts, xlab = "statistic value", main = "Statistic value posterior distribution", xaxt = "n")
        axis(1, pos = -10)
        mtext(1, text = round(obs, digit = 3), at = obs, col = "white", line = -0.2)
        abline(v = obs, col = "white")
        legend("topright", legend = "observed value", text.col = "white", box.lty = 0)
        p <- recordPlot()
      }
      diag$post.dist <- p
      dev.off()
      dev.new()
      cat("Permuted test for", length(v_perm), "permutations :", "\n")
      cat("Observed statistic value: ", obs, "\n")
      cat("P-values and effect sizes: \n")
      print(df)
      invisible(return(diag))
    }
    stop("Argument x is not an object of class 'ant cor', 'ant t-test', 'ant lm', 'ant glm', 'ant glmm', a vector or a data frame.")
  }
})
