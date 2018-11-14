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

#' @title Permuted Generalized Linear Model
#' @description Performs Generalized Linear Models tests and computes permuted p-values
#'
#' @param ant an output of ANT function \code{perm.net.nl} without any random factor declared, or output of ANT 'met' category functions in which output of ANT functions \code{perm.ds.focal}, \code{perm.ds.grp} or \code{perm.net.lk} where single matrices were used.
#' @param oda the original data frame of associations when argument ant is obtained with \code{perm.ds.focal} or \code{perm.ds.grp} ANT functions.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param family a description of the error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported, see \code{\link{glm}} and \code{\link{family}}.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @param start starting values for the parameters in the linear predictor.
#' @param control	 a list of parameters for controlling the fitting process.
#' @param model a logical value indicating whether model frame should be included as a component of the returned value.
#' @param method the method to be used in fitting the model. The default method "glm.fit" uses iteratively reweighted least squares (IWLS): the alternative "model.frame" returns the model frame and does no fitting.
#' @param x,y For glm: logical values indicating whether the response vector and model matrix used in the fitting process should be returned as components of the returned value.
#' @param contrasts an optional list. See the contrasts.arg of model.matrix.default.
#' @param ... Extra arguments for \code{\link{glm}} function only.
#' @return Returns a list of 3 elements :
#' \itemize{
#' \item An object of class inheriting from "glm" which inherits from the class "lm".
#' \item A data frame if the estimates of the permuted models.
#' \item A vector of integers indicating the permutations that returned model errors or warnings (e.g. model convergence issues) and for which new permutations were done.
#' }
#' @details This function is the first step in the process to create a t-test in permuted data. For more details on t-tests, see R documentation.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Dobson, A. J. (1990) An Introduction to Generalized Linear Models. London: Chapman and Hall.
#' @references Hastie, T. J. and Pregibon, D. (1992) Generalized linear models. Chapter 6 of Statistical Models in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
#' @references McCullagh P. and Nelder, J. A. (1989) Generalized Linear Models. London: Chapman and Hall.
#' @references Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. New York: Springer.
#' @seealso \code{glm}



stat.glm <- function(ant, oda, formula, family = "gaussian", progress = TRUE, start = NULL, control = list(...),
                     model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL, ...) {
  # Test on observe data ------------------------------------------------------------------------
  odf <- ant[[1]]
  # Model on original data
  tmp <- tryCatch(glm(
    formula = formula, data = odf, family = family, start = start, control = control,
    model = model, method = method, x = x, y = y, contrasts = contrasts, ...
  ), error = identity)


  if (is(tmp, "error")) {
    print("The model on your original data contains the following errors.")
    stop(tmp$message)
  }
  if (is(tmp, "warning")) {
    warning("The model on your original data contains the following warnings.")
    cat(tmp$message)
    answer <- readline(prompt = "Do you want to continue (y/n)? ")

    while (answer != "y" & answer != "n") {
      # play.sound(FALSE)
      readline("Model on your orignal data contain warnings.")
      answer <- readline(prompt = "Do you want to continue (y/n)? ")
    }
    if (answer == "n") {
      suppressMessages(stop(print(tmp)))
    }
  }
  else {
    obs <- summary(tmp)
    obs$resid <- residuals.glm(tmp)
    obs$fit <- fitted(tmp)
    obs$call <- paste(format(formula), ", family =", family)
    obs$coefficients <- obs$coefficients[, -4]
    cat("Original model : ", "\n", "\n")
    print(obs)
  }

  at <- attributes(ant)
  ant <- ant[-1]
  attributes(ant) <- at

  # GLM along permutations ------------------------------------------
  if (is.null(attributes(ant)$ANT)) {
    stop("Argument ant must be an object returned by perm.ds.grp, per.ds.focal or per.ds.nl functions")
  }
  if (attributes(ant)$ANT == "ANT node link permutation") {
    stop("Currently not available")
  }

  test1 <- attributes(ant)$ANT == "ANT node label permutation"
  test2 <- attributes(ant)$ANT == "ANT data stream group sampling single matrix"
  test3 <- attributes(ant)$ANT == "ANT data stream focal sampling single matrix"

  if (any(test1, test2, test3)) {
    if (test1) {
      tmp.env <- new.env()
      tmp.env$error <- NULL
      ctrl <- attributes(ant)$rf
      labels <- attributes(ant)$labels

      if (progress) {
        results <- lapply(seq_along(ant), function(i, ant, formula, progress = T, ctrl = ctrl, odf, labels, family, start, control, model, method, x, y, contrasts, ...) {
          cat("  Processing permutation : ", attributes(ant[[i]])$permutation, "\r")

          r <- tryCatch(glm(
            formula = formula, data = ant[[i]], family = family, start = start, control = control,
            model = model, method = method, x = x, y = y, contrasts = contrasts, ...
          ), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            tmp.env$error <- c(tmp.env$error, attributes(ant[[i]])$permutation)
            while (is(r, "error") | is(r, "warning")) {
              newdf <- perm.net.nl(odf, labels, rf = NULL, nperm = 1, progress = F)[[2]]
              r <- tryCatch(glm(
                formula = formula, data = newdf, family = family, start = start, control = control,
                model = model, method = method, x = x, y = y, contrasts = contrasts, ...
              ), error = identity)
            }
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, ant = ant, formula = formula, progress = T, odf = odf, labels = labels, family = family, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, ...)
        cat("\n")
      }

      else {
        results <- lapply(ant, function(d, formula, progress = T, ctrl = ctrl, odf, labels, family, start, control, model, method, x, y, contrasts, ...) {
          tmp <- tryCatch(glm(
            formula = formula, data = d, family = family, start = start, control = control,
            model = model, method = method, x = x, y = y, contrasts = contrasts, ...
          ), error = identity)

          if (is(tmp, "error") | is(tmp, "warning")) {
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
            while (is(tmp, "error") | is(tmp, "warning")) {
              newdf <- perm.net.nl(odf, labels, rf = NULL, nperm = 1, progress = F)[[2]]
              tmp <- tryCatch(glm(
                formula = formula, data = newdf, family = family, start = start, control = control,
                model = model, method = method, x = x, y = y, contrasts = contrasts, ...
              ), error = identity)
            }
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula, family, progress = T, odf, labels = labels, family = family, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, ...)
        cat("\n")
      }
    }

    # finding nodes metrics to permute in case of warnign or error along LM on permuted data ------------------------------------------
    arguments <- all.vars(formula)
    metrics <- c(
      "degree", "outdegree", "indegree", "strength", "outstrength", "instrength", "affinityB", "affinity", "affinityW", "disparity", "indisparity", "outdisparity",
      "eigenB", "eigenU", "outeigen", "ineigen", "eigenW", "eigen", "lpB", "lpW", "reach", "riB", "riW", "ri"
    )

    target.metrics <- metrics[metrics %in% arguments]

    # Removing nodes metrics from original data frame
    odf <- odf[, -c(df.col.findId(odf, target.metrics))]

    # ANT data stream group and focal sampling  ------------------------------------------
    if (test2) {
      cat("in", "\n")
      # Finding scan and control factor do redo data stream permutation
      Scan <- attributes(ant)$scan
      ctrlf <- attributes(ant)$ctrlf
      index <- attributes(ant)$method

      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$new.oda <- NULL
      tmp.env$error <- NULL

      if (progress) {
        results <- lapply(ant, function(d, index, formula, family, start, control, model, method, x, y, contrasts, odf, oda, target.metrics, Scan, ctrlf, ..) {
          cat("  Processing file: ", attributes(d)$permutation, "\r")
          attr(oda, "permutation") <- 0
          r <- tryCatch(glm(
            formula = formula, data = d, family = family, start = start, control = control,
            model = model, method = method, x = x, y = y, contrasts = contrasts, ...
          ), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            attr(odf, "permutation") <- attributes(d)$permutation
            r <- redo.ds.grp.glm(
              new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, Scan = Scan, ctrlf = ctrlf, index = index, formula = formula, data = d, family = family, start = start, control = control,
              model = model, method = method, x = x, y = y, contrasts = contrasts, ...
            )
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            return(r[[3]])
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, index, formula = formula, family = family, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, odf = odf, oda = oda, target.metrics = target.metrics, Scan = Scan, ctrlf = ctrlf, ...)
        cat("\n")
      }

      else {
        cat("in 2", "\n")
        results <- lapply(ant, function(d, index, formula, family, start, control, model, method, x, y, contrasts, odf, oda, target.metrics, Scan, ctrlf, ..) {
          attr(oda, "permutation") <- 0
          r <- tryCatch(glm(
            formula = formula, data = d, family = family, start = start, control = control,
            model = model, method = method, x = x, y = y, contrasts = contrasts, ...
          ), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            attr(odf, "permutation") <- attributes(d)$permutation
            r <- redo.ds.grp.glm(
              new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, Scan = Scan, ctrlf = ctrlf, index = index, formula = formula, data = d, family = family, start = start, control = control,
              model = model, method = method, x = x, y = y, contrasts = contrasts, ...
            )
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            return(r[[3]])
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, index, formula = formula, family = family, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, odf = odf, oda = oda, target.metrics = target.metrics, Scan = Scan, ctrlf = ctrlf, ...)
      }
    }

    if (test3) {
      focal <- attributes(ant)$focal
      ctrl <- attributes(ant)$ctrl
      alters <- attributes(ant)$alters
      index <- attributes(ant)$method

      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$gbi <- NULL
      tmp.env$gbi2 <- NULL
      tmp.env$error <- NULL

      if (progress) {
        results <- lapply(ant, function(d, formula, family, start, control, model, method, x, y, contrasts, odf, oda, target.metrics, focal, ctrl, alters, index, ...) {
          cat("  Processing file: ", attr(d, "permutation"), "\r")
          attr(oda, "permutation") <- 0
          r <- tryCatch(glm(formula = formula, family = family, data = d, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, ...), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            # redo a permutations on raw data
            # Giving to the original data frame of individuals characteristics (odf) the permutation number where error or warning have been found
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo.ds.grp.first return 3 ellements: 1) permutation index, 2) permuted data frame of associations 3) LM estimates
            r <- redo.ds.focal.glm(family = family, formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            tmp.env$gbi2 <- r[[3]]
            result <- r[[4]]
            return(result)
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, family = family, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, ...)
        cat("\n")
      }

      else {
        results <- lapply(ant, function(d, formula, family, start, control, model, method, x, y, contrasts, odf, oda, target.metrics, focal, ctrl, alters, index, ...) {
          attr(oda, "permutation") <- 0
          r <- tryCatch(glm(formula = formula, family = family, data = d, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, ...), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            # redo a permutations on raw data
            # Giving to the original data frame of individuals characteristics (odf) the permutation number where error or warning have been found
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo.ds.grp.first return 3 ellements: 1) permutation index, 2) permuted data frame of associations 3) LM estimates
            r <- redo.ds.focal.glm(family = family, formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            tmp.env$gbi2 <- r[[3]]
            result <- r[[4]]
            return(result)
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, family = family, start = start, control = control, model = model, method = method, x = x, y = y, contrasts = contrasts, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, ...)
        cat("\n")
      }
    }
  }
  else {
    stop("Argument ant must be an object returned by perm.ds.grp, per.ds.focal or per.ds.nl functions of type 'single matrix'.")
  }

  results <- do.call("rbind", results)
  result <- list("Original.model" = obs, "permutations" = results, "errors" = tmp.env$error)
  attr(result, "class") <- "ant glm"
  attr(result, "formula") <- paste(format(formula))
  return(result)
}