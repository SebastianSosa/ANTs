# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
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

#' @title Extracts statistical measures of interest in Linear Model
#' @description Performs correlations Generalized Linear Models tests and extracts estimates of predictor factors in each permuted model.
#' @param ant an output of ANT function \code{perm.net.nl} without any random factor declared, or output of ANT 'met' categories functions in which output of ANT functions \code{perm.ds.focal}, \code{perm.ds.grp} or \code{perm.net.lk} where single matrices were used.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param oda the original data frame of associations when argument ant is obtained with perm.ds.grp or perm.ds.focal ANT functions.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @param method the method to be used; for fitting, currently only method = "qr" is supported; method = "model.frame" returns the model frame (the same as with model = TRUE, see below).
#' @param model logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param x logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param y logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param qr logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param singular.ok logical. If FALSE (the default in S but not in R) a singular fit is an error.
#' @param contrasts an optional list. See the contrasts.arg of model.matrix.default.
#' @param ... Extra arguments for \code{\link{lm}} function only.
#' @return Returns a list of 3 elements :
#' \itemize{
#' \item An object of class "lm" or for multiple responses of class c("mlm", "lm").
#' \item A data frame if the estimates of the permuted models.
#' \item A vector of integers indicating the permutations that returned model errors or warnings (e.g. model convergence issues) and for which new permutations were done.
#' }
#' @details This function is the first step for performing t-tests in permuted data. For more details on t-tests, see R documentation.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Chambers, J. M. (1992) Linear models. Chapter 4 of Statistical Models in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
#' @references Wilkinson, G. N. and Rogers, C. E. (1973) Symbolic descriptions of factorial models for analysis of variance. Applied Statistics, 22, 392–9.
#' @examples
#' t=met.strength(sim.m,sim.df,1) # Computing network metric
#' t=perm.net.nl(t,labels='age',rf=NULL,nperm=10,progress=FALSE) # Node label permutations
#' r.lm=stat.lm(t,formula = strength ~ sex,progress=FALSE) # Permuted LM
#' @seealso \code{\link{lm}}

stat.lm <- function(ant, formula, oda, progress = TRUE, method = "qr", model = TRUE,
                    x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, ...) {
  if (is.null(attributes(ant)$ANT)) {
    stop("Argument ant must be an object returned by perm.ds.grp, per.ds.focal or per.ds.nl functions")
  }
  if (attributes(ant)$ANT == "ANT node link permutation") {
    stop("Currently not available")
  }
  # LM on observed data ------------------------------------------------------------------------
  odf <- ant[[1]]
  # Model on original data
  tmp <- tryCatch(lm(
    formula = formula, data = odf, method = method,
    model = model, x = x, y = y, qr = qr, singular.ok = singular.ok,
    contrasts = contrasts, ...
  ), error = identity)

  # Check for errors and warnings.
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
  # Extract GLM informations
  else {
    obs <- summary(tmp)
    obs$fit <- fitted(tmp)
    obs$call <- format(formula)

    t <- obs
    t$coefficients <- t$coefficients[, -4]
    cat("Original model : ", "\n", "\n")
    print(t)
  }

  at <- attributes(ant)
  ant <- ant[-1]
  attributes(ant) <- at

  # Checking if argument ant is an object returned by ANTs functions perm.ds.grp, per.ds.focal or per.ds.nl--------------------------
  # For each type of permutation, the process is the following:
  # 1. Compute model on a single permutation
  # 2. Check for warnings or errors
  # 3. If error, redo a permutation
  # 4. Perform steps 1, 2, 3 until there is no more error or warning

  test1 <- attributes(ant)$ANT == "ANT node label permutation"
  test2 <- attributes(ant)$ANT == "ANT data stream group sampling single matrix"
  test3 <- attributes(ant)$ANT == "ANT data stream focal sampling single matrix"

  # LM along permutations ------------------------------------------
  if (any(test1, test2, test3)) {
    # If node label permutations
    if (test1) {
      # Create en environment to store the permutations that return model error
      tmp.env <- new.env()
      tmp.env$error <- NULL
      # Store label permutation information (random factors and permuted labels)
      ctrl <- attributes(ant)$rf
      labels <- attributes(ant)$labels

      if (progress) {
        # LM on permuted data
        results <- lapply(ant, function(d, formula, ctrl = ctrl, odf, labels, method, model, x, y, qr, singular.ok, contrasts, ...) {
          cat("  Processing permutation : ", attributes(d)$permutation, "\r")

          # LM
          r <- tryCatch(lm(formula = formula, data = d, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

          # If error or warning
          if (is(r, "error") | is(r, "warning")) {
            # Extract permutation number
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
            # While error or warning
            while (is(r, "error") | is(r, "warning")) {
              # Permuted labels
              newdf <- perm.net.nl(odf, labels, rf = NULL, nperm = 1, progress = FALSE)[[2]]
              # Redo LM
              r <- tryCatch(lm(formula = formula, data = newdf, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)
            }
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, odf = odf, labels = labels, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
        cat("\n")
      }
      # If argument progress is FALSE, same as previoulsy but without printing statisitical test progress
      else {
        results <- lapply(ant, function(d, formula, ctrl = ctrl, odf, labels, method, model, x, y, qr, singular.ok, contrasts, ...) {
          r <- tryCatch(lm(formula = formula, data = d, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
            while (is(r, "error") | is(r, "warning")) {
              newdf <- perm.net.nl(odf, labels, rf = NULL, nperm = 1, progress = FALSE)[[2]]
              r <- tryCatch(lm(formula = formula, data = newdf, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)
            }
          }
          # Extract coefficients
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, odf = odf, labels = labels, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
      }
    }

    # Finding node metrics to recompute in case of warning or error along LM on permuted data
    # Extract metrics inside the formula
    arguments <- all.vars(formula)
    # All metrics available in ANTs
    metrics <- c(
      "degree", "outdegree", "indegree", "strength", "outstrength", "instrength", "affinityB", "affinity", "affinityW", "disparity", "indisparity", "outdisparity",
      "eigenB", "eigenU", "outeigen", "ineigen", "eigenW", "eigen", "lpB", "lpW", "reach", "riB", "riW", "ri"
    )

    # Which metric in formula are present in ANTs list
    target.metrics <- metrics[metrics %in% arguments]

    # Removing node metrics from original data frame
    odf <- odf[, -c(df.col.findId(odf, target.metrics))]

    # ANTs data stream group sampling  ------------------------------------------
    if (test2) {
      # Finding scan and control factor to redo data stream permutation
      Scan <- attributes(ant)$scan
      ctrlf <- attributes(ant)$ctrlf
      index <- attributes(ant)$method

      # New environment to store the new gbi, errors and original data
      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$new.oda <- NULL
      tmp.env$error <- NULL

      if (progress) {
        # LM on permuted data
        results <- lapply(ant, function(d, formula, index, method, model, x, y, qr, singular.ok, contrasts, odf, oda, target.metrics, Scan = Scan, ctrlf) {
          cat("  Processing file: ", attributes(d)$permutation, "\r")
          attr(oda, "permutation") <- 0
          # LM
          r <- tryCatch(lm(formula = formula, data = d, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

          # If error
          if (is(r, "error") | is(r, "warning")) {
            # Extract permutation number
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo data stream permutations with gambit of the group protocol and recompute network metrics
            r <- redo.ds.grp.lm(new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, formula = formula, Scan = Scan, ctrlf = ctrlf, index = index, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
            # Store information for future repermutations
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            return(r[[3]])
          }

          # Extract coefficients
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula, index, method, model, x, y, qr, singular.ok, contrasts, odf, oda, target.metrics, Scan = Scan, ctrlf)
        cat("\n")
      }
      # If argument progress is FALSE, same as previoulsy but without printing statistical test progress
      else {
        results <- lapply(ant, function(d, formula, index, method, model, x, y, qr, singular.ok, contrasts, odf, oda, target.metrics, Scan = Scan, ctrlf) {
          cat("  Processing file: ", attributes(d)$permutation, "\r")
          attr(oda, "permutation") <- 0
          r <- tryCatch(lm(formula = formula, data = d, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            attr(odf, "permutation") <- attributes(d)$permutation
            r <- redo.ds.grp.lm(new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, formula = formula, Scan = Scan, ctrlf = ctrlf, index = index, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            return(r[[3]])
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula, index, method, model, x, y, qr, singular.ok, contrasts, odf, oda, target.metrics, Scan = Scan, ctrlf)
      }
    }

    # ANTs data stream focal sampling  ------------------------------------------
    if (test3) {
      # Finding focals, control factors, alters and method to redo data stream permutation
      focal <- attributes(ant)$focal
      ctrl <- attributes(ant)$ctrl
      alters <- attributes(ant)$alters
      index <- attributes(ant)$method

      # New environment to store the new gbi, errors and original data
      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$gbi <- NULL
      tmp.env$gbi2 <- NULL
      tmp.env$error <- NULL

      if (progress) {
        # LM on permuted data
        results <- lapply(ant, function(d, formula, method, model, x, y, qr, singular.ok, contrasts, odf, oda, target.metrics, focal, ctrl, alters, index, ...) {
          cat("  Processing file: ", attributes(d)$permutation, "\r")
          attr(oda, "permutation") <- 0
          # LM
          r <- tryCatch(lm(formula = formula, data = d, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

          # If error
          if (is(r, "error") | is(r, "warning")) {
            print("redo")
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) permuted data frame of associations 3) LM estimates
            r <- redo.ds.focal.lm(formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            tmp.env$gbi2 <- r[[3]]
            result <- r[[4]]
            return(result)
          }
          # Extract coefficients
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, ...)
        cat("\n")
      }
      # If argument progress is FALSE, same as previoulsy but without printing statistical test progress
      else {
        results <- lapply(ant, function(d, formula, method, model, x, y, qr, singular.ok, contrasts, odf, oda, target.metrics, focal, ctrlf, alters, index, ...) {
          attr(oda, "permutation") <- 0
          r <- tryCatch(lm(formula = formula, data = d, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

          if (is(r, "error") | is(r, "warning")) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) permuted data frame of associations 3) LM estimates
            r <- redo.ds.focal.lm(formula = formula, new.perm = tmp.env$new.perm, new.odf = tmp.env$new.oda, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$new.oda <- r[[2]]
            result <- r[[3]]
            return(result)
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, method = method, model = model, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrlf = ctrlf, alters = alters, index = index, )
      }
    }
  }
  else {
    stop("Argument ant must be an object returned by perm.ds.grp, per.ds.focal or per.ds.nl functions of type 'single matrix'.")
  }

  # Merge list of coefficients
  results <- do.call("rbind", results)
  # Create an object with the original model, the permuted coefficients, the permutation numbers that require repermutations
  result <- list("Original.model" = t, "permutations" = results, "errors" = tmp.env$error)
  attr(result, "class") <- "ant lm"
  attr(result, "formula") <- paste(format(formula))
  cat("\n")
  return(result)
}
