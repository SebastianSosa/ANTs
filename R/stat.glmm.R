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

#' @title Extracts statistical measures of interest in Generalized Linear Mixed Models
#' @description Performs Generalized Linear Mixed Models tests
#' @param ant an output of ANT function \code{perm.net.nl} with random factor stated, or output of ANT 'met' categories functions in which output of ANT functions \code{perm.ds.focal}, \code{perm.ds.grp} or \code{perm.net.lk} where multiple matrices have been used.
#' @param formula two-sided linear formula object describing both the fixed-effects and random-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. Random-effects terms are distinguished by vertical bars (|) separating expressions for design matrices from grouping factors. Two vertical bars (||) can be used to specify multiple uncorrelated random effects for the same grouping variable. (Because of the way it is implemented, the ||-syntax works only for design matrices containing numeric (continuous) predictors; to fit models with independent categorical effects, see dummy or the lmer_alt function from the afex package.).
#' @param family a GLM family, see \code{\link{glm}} and \code{\link{family}}.
#' @param oda the original data frame of associations when argument ant is obtained with perm.ds.grp or perm.ds.focal ANT functions.
#' @param progress a boolean indicating the visualization of the permutation process.
#' @param ... Extra arguments for \code{lmer} or \code{glmer} function only.
#' @details GLMM with permutation data.
#' @return Returns a list of 3 elements :
#' \itemize{
#' \item An object of class \code{\link{merMod}} (more specifically, an object of subclass lmerMod or glmerMod), for which many methods are available (e.g. methods(class="merMod")).
#' \item A data frame if the estimates of the permuted models.
#' \item A vector of integers indicating the permutations that returned model errors or warnings (e.g. model convergence issues) and for which new permutations were done.
#' }
#' @seealso \code{\link{lmer}} or \code{\link{glmer}}
#' @examples
#' # Creating temporal data--------------------------
#' m2=matrix(sample(sim.m),20,20)
#' diag(m2)=0
#' colnames(m2)=colnames(sim.m)
#' row.names(m2)=row.names(sim.m)
#' df2=sim.df
#' df2$age=df2$age+1
#' df1=sim.df
#' df1$period=rep(1,nrow(df1))
#' df2$period=rep(2,nrow(df2))
#' # Data structure for multiple matrices analytical protocol------------------
#' sim.lm=list(sim.m,m2)
#' sim.ldf=list(df1,df2)
#' # Computing network metric---------------------------------------------------
#' t=met.strength(sim.lm,sim.ldf,1)
#' # Node label permutations---------------------------------------------------
#' t=perm.net.nl(t,labels='age',rf="period",nperm=10,progress=FALSE) 
#' # Permuted GLMM-------------------------------------------------------------
#' r.glmm=stat.glmm(ant = t,formula = strength ~ age + (1|id),family = gaussian(), progress=TRUE)
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

stat.glmm <- function(ant, formula, family, oda = NULL, progress = TRUE, ...) {

  if (is.character(family)) {
    fam <- family
  }
  else {
    if (attributes(family)$class == "family") {
      family <- family
      fam <- family$family
    }
    else {
      stop("Argument family is not a character or a family function.")
    }
  }
  

  if (is.null(attributes(ant)$ANT)) {
    warning("Argument ant is not an object returned by perm.ds.grp, per.ds.focal or per.ds.nl functions. Permutations for which the model fails will produce NA in your posterior distribution of the regression coefficient.")
    if(is.list(ant)){
          test = unlist(lapply(ant, is.data.frame))
          if(all(test) == TRUE){
            if (fam == "gaussian") {
              # Test on observed data ------------------------------------------------------------------------
              odf <- ant[[1]]
              
              tmp <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = odf))), error = identity)
              
              if (isS4(tmp)) {
                if (is(tmp, "error")) {
                  print("The model on your original data contains the following errors.")
                  print(tmp)
                  stop()
                }
                else {
                  r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
                  if(max(abs(r2))<0.001){test=TRUE}
                  else{
                    test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
                  }
                }
              }
              if (is(tmp, "error")) {
                print("The model on your original data contains the following errors.")
                print(tmp)
                stop()
              }
              if (is(tmp, "warning")) {
                test <- FALSE
              }
              
              if (all(test) != TRUE) {
                # play.sound(FALSE)
                warning("The model on your original data contains the following warnings.")
                if (isS4(tmp)) {
                  print(tmp@optinfo$conv$lme4$messages)
                }
                else {
                  cat(tmp$message)
                }
                answer <- readline(prompt = "Do you want to continue (y/n)? ")
                
                while (answer != "y" & answer != "n") {
                  # play.sound(FALSE)
                  readline("Model on your orignal data contain warnings.")
                  answer <- readline(prompt = "Do you want to continue (y/n)? ")
                }
                if (answer == "n") {
                  suppressMessages(stop(print(tmp)))
                }
                else {
                  obs <- summary(tmp)
                  obs$fit <- fitted(tmp)
                  obs$family <- paste(family)
                }
              }
              else {
                obs <- summary(tmp)
                obs$fit <- fitted(tmp)
                obs$family <- paste(family)
                obs$coefficients <- obs$coefficients[, -4]
              }
              
              cat("Original model : ", "\n", "\n")
              
              print(obs)
              at <- attributes(ant)
              ant <- ant[-1]
              attributes(ant) <- at
              # GLMM along permutations ------------------------------------------
              tmp.env <- new.env()
              tmp.env$new.perm <- 0
              tmp.env$gbi <- NULL
              tmp.env$error <- NULL
              
              if (progress) {
                permuted <- lapply(seq_along(ant), function(i, ant, formula, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...) {
                  cat("  Processing permutation : ", attributes(ant[[i]])$permutation, "\r")
                  attr(oda, "permutation") <- 0
                  r <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = ant[[i]], ...))), error = identity)
                  
                  if (isS4(r)) {
                    r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                    if(max(abs(r2))<0.001){test=TRUE}
                    else{
                      test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                    }
                  }
                  if (is(r, "error")) {
                    test <- FALSE
                  }
                  if (is(r, "warning")) {
                    test <- FALSE
                  }
                  
                  if (all(test) != TRUE) {
                    r <- NA
                  }
                  r <- summary(r)$coefficients[, 1]
                  return(r)
                }, ant = ant, formula, odf, oda, target.metrics, Scan = Scan, ctrlf = ctrlf, method = method, fam = fam, ...)
                cat("\n")
              }
              else {
                permuted <- lapply(seq_along(ant), function(i, ant, formula, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...) {
                  attr(oda, "permutation") <- 0
                  r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = ant[[i]], ...))), error = identity)
                  
                  if (isS4(r)) {
                    r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                    if(max(abs(r2))<0.001){test=TRUE}
                    else{
                      test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                    }
                  }
                  if (is(r, "error")) {
                    test <- FALSE
                  }
                  if (is(r, "warning")) {
                    test <- FALSE
                  }
                  
                  if (all(test) != TRUE) {
                    r <- NA
                  }
                  r <- summary(r)$coefficients[, 1]
                  return(r)
                }, ant = ant, formula, odf, oda, target.metrics, Scan = Scan, ctrlf = ctrlf, method = method, fam = fam, ...)
              }
            }
            if (fam != "gaussian") {
              # Test on observed data ------------------------------------------------------------------------
              odf <- ant[[1]]
              
              tmp <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = odf, family = family, ...))), error = identity)
              
              if (isS4(tmp)) {
                if (is(tmp, "error")) {
                  print("The model on your original data contains the following errors.")
                  print(tmp)
                  stop()
                }
                else {
                  r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
                  if(max(abs(r2))<0.001){test=TRUE}
                  else{
                    test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
                  }
                }
              }
              if (is(tmp, "error")) {
                print("The model on your original data contains the following errors.")
                print(tmp)
                stop()
              }
              if (is(tmp, "warning")) {
                test <- FALSE
              }
              
              if (all(test) != TRUE) {
                # play.sound(FALSE)
                warning("The model on your original data contains the following warnings.")
                if (isS4(tmp)) {
                  print(tmp@optinfo$conv$lme4$messages)
                }
                else {
                  cat(tmp$message)
                }
                answer <- readline(prompt = "Do you want to continue (y/n)? ")
                
                while (answer != "y" & answer != "n") {
                  # play.sound(FALSE)
                  readline("Model on your orignal data contain warnings.")
                  answer <- readline(prompt = "Do you want to continue (y/n)? ")
                }
                if (answer == "n") {
                  suppressMessages(stop(print(tmp)))
                }
                else {
                  obs <- summary(tmp)
                  obs$fit <- fitted(tmp)
                  obs$family <- paste(family)
                }
              }
              else {
                obs <- summary(tmp)
                obs$fit <- fitted(tmp)
                obs$family <- paste(fam)
                obs$coefficients <- obs$coefficients[, -4]
              }
              
              cat("Original model : ", "\n", "\n")
              print(obs)
              
              at <- attributes(ant)
              ant <- ant[-1]
              attributes(ant) <- at
              
              # GLMM along permutations ------------------------------------------
              tmp.env <- new.env()
              tmp.env$new.perm <- 0
              tmp.env$gbi <- NULL
              tmp.env$error <- NULL
              if (progress) {
                permuted <- lapply(seq_along(ant), function(i, ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam) {
                  cat("  Processing permutation : ", i, "\r")
                  r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = ant[[i]], family = family, ...))), error = identity)
                  
                  if (isS4(r)) {
                    r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                    if(max(abs(r2))<0.001){test=TRUE}
                    else{
                      test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                    }
                  }
                  if (is(r, "error")) {
                    test <- FALSE
                  }
                  if (is(r, "warning")) {
                    test <- FALSE
                  }
                  
                  if (all(test) != TRUE) {
                    r <- NA
                  }
                  result <- summary(r)$coefficients[, 1]
                  return(result)
                }, ant = ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...)
                cat("\n")
              }
              else {
                permuted <- lapply(seq_along(ant), function(i, ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...) {
                  r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = ant[[i]], family = family, ...))), error = identity)
                  
                  if (isS4(r)) {
                    r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                    if(max(abs(r2))<0.001){test=TRUE}
                    else{
                      test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                    }
                  }
                  if (is(r, "error")) {
                    test <- FALSE
                  }
                  if (is(r, "warning")) {
                    test <- FALSE
                  }
                  
                  if (all(test) != TRUE) {
                    r <- NA
                  }
                  result <- summary(r)$coefficients[, 1]
                  return(result)
                }, ant = ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...)
              }
            }
            permuted <- do.call("rbind", permuted)
            result <- list("Original.model" = obs, "permutations" = permuted, "errors" = tmp.env$error)
            attr(result, "class") <- "ant glmm"
            attr(result, "family") <- paste(family)
            attr(result, "formula") <- format(formula)
            cat("\n")
            return(result)
          }else{stop("Argument ant is not a list of data frames.")}
        }else{stop("Argument ant is neither a list nor an object returned by perm.ds.grp, per.ds.focal or per.ds.nl functions.")}
  }

  if (attributes(ant)$ANT == "ANT data stream group sampling multiple matrices") {
    if (is.null(oda)) {
      stop("Argument oda cannot be NULL when argument ant is obtained with perm.ds.grp or perm.ds.focal ANT functions")
    }

    if (fam == "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]

      tmp <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = odf))), error = identity)

      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }

      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")

        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
        obs$coefficients <- obs$coefficients[, -4]
      }

      cat("Original model : ", "\n", "\n")

      print(obs)
      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at
      # Parametrisation in case of error or warning -------------------------------------------------
      # finding node metrics to permute in case of warning or error along GLMM on permuted data
      arguments <- all.vars(formula)
      metrics <- c(
        "degree", "outdegree", "indegree", "strength", "outstrength", "instrength", "affinityB", "affinity", "affinityW",
        "disparity", "indisparity", "outdisparity", "eigenB", "eigenU", "outeigen", "ineigen", "eigenW", "eigen", "lpB", 
        "lpW", "reach", "riB", "riW", "ri"
      )

      target.metrics <- metrics[metrics %in% arguments]

      # Removing node metrics from original data frame
      odf <- odf[, -c(df.col.findId(odf, target.metrics))]

      # Finding scan and control factor to redo data stream permutation
      Scan <- attributes(ant)$scan
      ctrlf <- attributes(ant)$ctrlf
      method <- attributes(ant)$method

      # GLMM along permutations ------------------------------------------
      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$gbi <- NULL
      tmp.env$error <- NULL

      if (progress) {
        permuted <- lapply(seq_along(ant), function(i, ant, formula, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...) {
          cat("  Processing permutation : ", attributes(ant[[i]])$permutation, "\r")
          attr(oda, "permutation") <- 0
          r <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = ant[[i]], ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(ant[[i]])$permutation
            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
            r <- redo.ds.grp(family = "gaussian", new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, formula = formula, Scan = Scan, method = method, ctrlf = ctrlf, fam = fam, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            result <- r[[3]]
            return(result)
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, ant = ant, formula, odf, oda, target.metrics, Scan = Scan, ctrlf = ctrlf, method = method, fam = fam, ...)
        cat("\n")
      }
      else {
        permuted <- lapply(seq_along(ant), function(i, ant, formula, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...) {
          attr(oda, "permutation") <- 0
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = ant[[i]], ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(ant[[i]])$permutation
            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
            r <- redo.ds.grp(family = "gaussian", new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, formula = formula, Scan = Scan, method = method, ctrlf = ctrlf, fam = fam, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            result <- r[[3]]
            return(result)
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, ant = ant, formula, odf, oda, target.metrics, Scan = Scan, ctrlf = ctrlf, method = method, fam = fam, ...)
      }
    }
    if (fam != "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]

      tmp <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = odf, family = family, ...))), error = identity)

      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }

      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")

        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(fam)
        obs$coefficients <- obs$coefficients[, -4]
      }

      cat("Original model : ", "\n", "\n")
      print(obs)

      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at

      # Parametrisation in case of error or warning -------------------------------------------------
      # finding node metrics to permute in case of warning or error along GLMM on permuted data
      arguments <- all.vars(formula)
      metrics <- c(
        "degree", "outdegree", "indegree", "strength", "outstrength", "instrength", "affinityB", "affinity", "affinityW",
        "disparity", "indisparity", "outdisparity", "eigenB", "eigenU", "outeigen", "ineigen", "eigenW", "eigen", "lpB", 
        "lpW", "reach", "riB", "riW", "ri"
      )

      target.metrics <- metrics[metrics %in% arguments]

      # Removing node metrics from original data frame
      odf <- odf[, -c(df.col.findId(odf, target.metrics))]

      # Finding scan and control factor to redo data stream permutation
      Scan <- attributes(ant)$scan
      ctrlf <- attributes(ant)$ctrlf
      method <- attributes(ant)$method


      # GLMM along permutations ------------------------------------------
      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$gbi <- NULL
      tmp.env$error <- NULL
      if (progress) {
        permuted <- lapply(seq_along(ant), function(i, ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam) {
          cat("  Processing permutation : ", i, "\r")
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = ant[[i]], family = family, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(ant[[i]])$permutation

            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
            r <- redo.ds.grp(family = family, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, formula = formula, Scan = Scan, method = method, ctrlf = ctrlf, fam = fam, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            result <- r[[3]]
            return(result)
          }
          result <- summary(r)$coefficients[, 1]
          return(result)
        }, ant = ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...)
        cat("\n")
      }
      else {
        permuted <- lapply(seq_along(ant), function(i, ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...) {
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = ant[[i]], family = family, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(ant[[i]])$permutation

            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
            r <- redo.ds.grp(family = family, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, oda = oda, odf = odf, target.metrics = target.metrics, formula = formula, Scan = Scan, method = method, ctrlf = ctrlf, fam = fam, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            result <- r[[3]]
            return(result)
          }
          result <- summary(r)$coefficients[, 1]
          return(result)
        }, ant = ant, formula, family, odf, oda, target.metrics, Scan, ctrlf, method, fam, ...)
      }
    }
    permuted <- do.call("rbind", permuted)
    result <- list("Original.model" = obs, "permutations" = permuted, "errors" = tmp.env$error)
    attr(result, "class") <- "ant glmm"
    attr(result, "family") <- paste(family)
    attr(result, "formula") <- format(formula)
    cat("\n")
    return(result)
  }

  if (attributes(ant)$ANT == "ANT data stream focal sampling multiple matrices") {
    if (is.null(oda)) {
      stop("Argument oda cannot be NULL when argument ant is obtained with perm.ds.grp or perm.ds.focal ANT functions")
    }

    if (fam == "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]

      tmp <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = odf, ...))), error = identity)

      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }

      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")

        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
        obs$coefficients <- obs$coefficients[, -4]
      }

      cat("Original model : ", "\n", "\n")
      print(obs)

      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at

      # Parametrisation in case of error or warning -------------------------------------------------
      # finding node metrics to permute in case of warning or error along GLMM on permuted data
      arguments <- all.vars(formula)
      metrics <- c(
        "degree", "outdegree", "indegree", "strength", "outstrength", "instrength", "affinityB", "affinity", "affinityW",
        "disparity", "indisparity", "outdisparity", "eigenB", "eigenU", "outeigen", "ineigen", "eigenW", "eigen", "lpB", 
        "lpW", "reach", "riB", "riW", "ri"
      )

      target.metrics <- metrics[metrics %in% arguments]

      # Removing node metrics from original data frame
      odf <- odf[, -c(df.col.findId(odf, target.metrics))]

      # Finding scan and control factor to redo data stream permutation
      focal <- attributes(ant)$focal
      ctrl <- attributes(ant)$ctrl
      alters <- attributes(ant)$alters
      index <- attributes(ant)$method

      # GLMM along permutations ------------------------------------------
      tmp.env <- new.env()
      tmp.env$new.perm <- 0
      tmp.env$gbi <- NULL
      tmp.env$gbi2 <- NULL
      tmp.env$error <- NULL

      if (progress == TRUE) {
        permuted <- lapply(ant, function(d, formula, odf, oda, target.metrics, focal, ctrl, alters, index, fam, ...) {
          cat("  Processing permutation : ", attributes(d)$permutation, "\r")
          attr(odf, "permutation") <- 0
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = d, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) permuted data frame of associations 3) glmm estimates
            r <- redo.ds.focal.glmm(family = family, formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, fam = fam, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            tmp.env$gbi2 <- r[[3]]
            result <- r[[4]]
            return(result)
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, fam = fam, ...)
        cat("\n")
      }
      else {
        permuted <- lapply(ant, function(d, formula, odf, oda, target.metrics, focal, ctrl, alters, index, fam, ...) {
          attr(odf, "permutation") <- 0
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = d, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(d)$permutation
            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) permuted data frame of associations 3) glmm estimates
            r <- redo.ds.focal.glmm(family = family, formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, fam = fam, ...)
            tmp.env$new.perm <- r[[1]]
            tmp.env$error <- c(tmp.env$error, r[[1]])
            tmp.env$gbi <- r[[2]]
            tmp.env$gbi2 <- r[[3]]
            result <- r[[4]]
            return(result)
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, fam = fam, ...)
      }
    }

    if (fam != "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]

      tmp <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = odf, ...))), error = identity)

      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }

      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")

        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
      }

      cat("Original model : ", "\n", "\n")
      print(obs)

      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at

      # Parametrisation in case of error or warning -------------------------------------------------
      # finding node metrics to permute in case of warning or error along GLMM on permuted data
      arguments <- all.vars(formula)
      metrics <- c(
        "degree", "outdegree", "indegree", "strength", "outstrength", "instrength", "affinityB", "affinity", "affinityW",
        "disparity", "indisparity", "outdisparity", "eigenB", "eigenU", "outeigen", "ineigen", "eigenW", "eigen", "lpB", 
        "lpW", "reach", "riB", "riW", "ri"
      )

      target.metrics <- metrics[metrics %in% arguments]

      # Removing node metrics from original data frame
      odf <- odf[, -c(df.col.findId(odf, target.metrics))]

      # Finding scan and control factor to redo data stream permutation
      focal <- attributes(ant)$focal
      ctrl <- attributes(ant)$ctrl
      alters <- attributes(ant)$alters


      # GLMM along permutations ------------------------------------------
      new.perm <- 0
      new.oda <- NULL
      er <- NULL
      if (progress == TRUE) {
        permuted <- lapply(ant, function(d, formula, family, progress = TRUE, odf, oda, target.metrics, focal, ctrl, alters, new.perm, new.oda, fam, ...) {
          cat("  Processing permutation : ", attributes(d)$permutation, "\r")
          attr(odf, "permutation") <- 0
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = d, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            er <- c(er, attributes(d)$permutation)
            cat("  Processing permutation ", attributes(d)$permutation, " resampling", "\r")
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(d)$permutation

            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) permuted data frame of associations 3) glmm estimates
            r <- redo.ds.focal.glmm(family = family, formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, fam = fam, ...)

            new.perm <- r[[1]]
            new.oda <- r[[2]]
            result <- r[[3]]
            return(result)
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula, family, progress = TRUE, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, new.perm = new.perm, new.oda = new.oda, fam = fam, ...)
        cat("\n")
      }
      else {
        permuted <- lapply(ant, function(d, formula, family, progress = TRUE, odf, oda, target.metrics, focal, ctrl, alters, new.perm, new.oda, fam, ...) {
          attr(odf, "permutation") <- 0
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = d, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            # redo a permutation on raw data
            er <- c(er, attributes(d)$permutation)
            cat("  Processing permutation ", attributes(d)$permutation, " resampling", "\r")
            # Giving to the original data frame of individual characteristics (odf) the permutation number where error or warning were found
            attr(odf, "permutation") <- attributes(d)$permutation

            # redo.ds.grp.first return 3 elements: 1) permutation index, 2) permuted data frame of associations 3) glmm estimates
            r <- redo.ds.focal.glmm(family = family, formula = formula, new.perm = tmp.env$new.perm, gbi = tmp.env$gbi, gbi2 = tmp.env$gbi2, oda = oda, odf = odf, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, index = index, fam = fam, ...)

            new.perm <- r[[1]]
            new.oda <- r[[2]]
            result <- r[[3]]
            return(result)
          }
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula, family, progress = TRUE, odf = odf, oda = oda, target.metrics = target.metrics, focal = focal, ctrl = ctrl, alters = alters, new.perm = new.perm, new.oda = new.oda, fam = fam, ...)
      }
    }

    permuted <- do.call("rbind", permuted)
    result <- list("Original.model" = obs, "permutations" = permuted, "errors" = tmp.env$error)
    attr(result, "class") <- "ant glmm"
    attr(result, "family") <- paste(family)
    attr(result, "formula") <- format(formula)
    cat("\n")
    return(result)
  }

  if (attributes(ant)$ANT == "ANT node label permutation with random factors") {
    if (fam == "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]

      tmp <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = odf, ...))), error = identity)

      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }

      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")

        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
        obs$coefficients <- obs$coefficients[, -4]
      }

      cat("Original model : ", "\n", "\n")
      print(obs)

      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at

      # Test along the list of permutation data ------------------------------------------------------------------------
      if (progress) {
        tmp.env <- new.env()
        tmp.env$error <- NULL
        ctrl <- attributes(ant)$rf
        labels <- attributes(ant)$labels

        permuted <- lapply(seq_along(ant), function(i, ant, formula, progress, ctrl, odf, labels, ...) {
          cat("  Processing permutation : ", attributes(ant[[i]])$permutation, "\r")

          r <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = ant[[i]], ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(ant[[i]])$permutation)
            while (all(test) != TRUE) {
              newdf <- perm.redo(df = odf, labels = labels, ctrl = ctrl)
              r <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = newdf, ...))), error = identity)

              if (isS4(r)) {
                r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                if(max(abs(r2))<0.001){test=TRUE}
                else{
                  test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                }
              }
              if (is(r, "error")) {
                test <- FALSE
              }
              if (is(r, "warning")) {
                test <- FALSE
              }
            }
          }

          r <- summary(r)$coefficients[, 1]

          return(r)
        }, ant = ant, formula, progress = TRUE, ctrl = ctrl, odf = odf, labels = labels, ...)
        cat("\n")
      }
      else {
        tmp.env <- new.env()
        tmp.env$error <- NULL
        ctrl <- attributes(ant)$rf
        labels <- attributes(ant)$labels

        permuted <- lapply(seq_along(ant), function(i, ant, formula, progress, ctrl, odf, labels, ...) {
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = ant[[i]], ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(ant[[i]])$permutation)
            while (all(test) != TRUE) {
              newdf <- perm.redo(df = odf, labels = labels, ctrl = ctrl)
              r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = newdf, ...))), error = identity)

              if (isS4(r)) {
                r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                if(max(abs(r2))<0.001){test=TRUE}
                else{
                  test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                }
              }
              if (is(r, "error")) {
                test <- FALSE
              }
              if (is(r, "warning")) {
                test <- FALSE
              }
            }
          }

          r <- summary(r)$coefficients[, 1]

          return(r)
        }, ant = ant, formula, progress = TRUE, ctrl = ctrl, odf = odf, labels = labels, ...)
      }
    }

    if (fam != "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]

      tmp <- suppressWarnings(tryCatch(suppressMessages(lme4::glmer(formula = formula, data = odf, family = family, ...)), error = identity))
      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }

      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")

        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
      }

      cat("Original model : ", "\n", "\n")
      print(summary(obs))

      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at

      ctrl <- attributes(ant)$rf
      labels <- attributes(ant)$labels
      # Test along the list of permutation data ------------------------------------------------------------------------
      if (progress == TRUE) {
        tmp.env <- new.env()
        tmp.env$error <- NULL

        permuted <- lapply(ant, function(d, formula, family, ctrl, labels, odf, ...) {
          cat("  Processing permutation : ", attributes(d)$permutation, "\r")
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = d, family = family, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
          }

          while (all(test) != TRUE) {
            newdf <- perm.redo(df = odf, labels = labels, ctrl = ctrl)
            r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = newdf, family = family, ...))), error = identity)
            
            if (isS4(r)) {
              r2=with(r@optinfo$derivs,solve(Hessian,gradient))
              if(max(abs(r2))<0.001){test=TRUE}
              else{
                test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
              }
            }
            if (is(r, "error")) {
              test <- FALSE
            }
            if (is(r, "warning")) {
              test <- FALSE
            }
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, family = family, ctrl = ctrl, labels, odf, ...)
        cat("\n")
      }
      else {
        tmp.env <- new.env()
        tmp.env$error <- NULL

        permuted <- lapply(ant, function(d, formula, family, ctrl, labels, odf, w, ...) {
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = d, family = family, ...))), error = identity)

          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }

          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
          }

          while (all(test) != TRUE) {
            newdf <- perm.redo(df = odf, labels = labels, ctrl = ctrl)
            r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = newdf, family = family, ...))), error = identity)
            if (isS4(r)) {
              r2=with(r@optinfo$derivs,solve(Hessian,gradient))
              if(max(abs(r2))<0.001){test=TRUE}
              else{
                test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
              }
            }
            if (is(r, "error")) {
              test <- FALSE
            }
            if (is(r, "warning")) {
              test <- FALSE
            }
          }

          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, family = family, ctrl = ctrl, labels, odf, ...)
      }
    }

    permuted <- do.call("rbind", permuted)
    result <- list("Original.model" = obs, "permutations" = permuted, "errors" = tmp.env$error)
    attr(result, "class") <- "ant glmm"
    attr(result, "family") <- paste(family)
    attr(result, "formula") <- format(formula)
    cat("\n")
    return(result)
  }
  
  if(attributes(ant)$ANT == "ANT node label permutation with random factors and structure maintained"){
    if (fam == "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]
      
      tmp <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = odf, ...))), error = identity)
      
      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }
      
      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")
        
        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
        obs$coefficients <- obs$coefficients[, -4]
      }
      
      cat("Original model : ", "\n", "\n")
      print(obs)
      
      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at
      
      # Test along the list of permutation data ------------------------------------------------------------------------
      if (progress) {
        tmp.env <- new.env()
        tmp.env$error <- NULL
        ctrl <- attributes(ant)$rf
        labels <- attributes(ant)$labels
        
        permuted <- lapply(seq_along(ant), function(i, ant, formula, progress, ctrl, odf, labels, ...) {
          cat("  Processing permutation : ", attributes(ant[[i]])$permutation, "\r")
          
          r <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = ant[[i]], ...))), error = identity)
          
          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }
          
          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(ant[[i]])$permutation)
            while (all(test) != TRUE) {
              newdf <- redo.perm.net.nl.str(df = odf, labels = labels, rf = ctrl)
              
              r <- tryCatch(suppressWarnings(suppressMessages(lmer(formula = formula, data = newdf, ...))), error = identity)
              
              if (isS4(r)) {
                r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                if(max(abs(r2))<0.001){test=TRUE}
                else{
                  test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                }
              }
              if (is(r, "error")) {
                test <- FALSE
              }
              if (is(r, "warning")) {
                test <- FALSE
              }
            }
          }
          
          r <- summary(r)$coefficients[, 1]
          
          return(r)
        }, ant = ant, formula, progress = TRUE, ctrl = ctrl, odf = odf, labels = labels, ...)
        cat("\n")
      }
      else {
        tmp.env <- new.env()
        tmp.env$error <- NULL
        ctrl <- attributes(ant)$rf
        labels <- attributes(ant)$labels
        
        permuted <- lapply(seq_along(ant), function(i, ant, formula, progress, ctrl, odf, labels, ...) {
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = ant[[i]], ...))), error = identity)
          
          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }
          
          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(ant[[i]])$permutation)
            while (all(test) != TRUE) {
              newdf <- redo.perm.net.nl.str(df = odf, labels = labels, rf = ctrl)
              r <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(formula = formula, data = newdf, ...))), error = identity)
              
              if (isS4(r)) {
                r2=with(r@optinfo$derivs,solve(Hessian,gradient))
                if(max(abs(r2))<0.001){test=TRUE}
                else{
                  test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
                }
              }
              if (is(r, "error")) {
                test <- FALSE
              }
              if (is(r, "warning")) {
                test <- FALSE
              }
            }
          }
          
          r <- summary(r)$coefficients[, 1]
          
          return(r)
        }, ant = ant, formula, progress = TRUE, ctrl = ctrl, odf = odf, labels = labels, ...)
      }
    }
    
    if (fam != "gaussian") {
      # Test on observed data ------------------------------------------------------------------------
      odf <- ant[[1]]
      
      tmp <- suppressWarnings(tryCatch(suppressMessages(lme4::glmer(formula = formula, data = odf, family = family, ...)), error = identity))
      if (isS4(tmp)) {
        if (is(tmp, "error")) {
          print("The model on your original data contains the following errors.")
          print(tmp)
          stop()
        }
        else {
          r2=with(tmp@optinfo$derivs,solve(Hessian,gradient))
          if(max(abs(r2))<0.001){test=TRUE}
          else{
            test <- c(!is(tmp, "error"), !is(tmp, "warning"),tmp@optinfo$conv$opt == 0, length(tmp@optinfo$conv$lme4$messages) == 0, length(tmp@optinfo$warnings) == 0)
          }
        }
      }
      if (is(tmp, "error")) {
        print("The model on your original data contains the following errors.")
        print(tmp)
        stop()
      }
      if (is(tmp, "warning")) {
        test <- FALSE
      }
      
      if (all(test) != TRUE) {
        # play.sound(FALSE)
        warning("The model on your original data contains the following warnings.")
        if (isS4(tmp)) {
          print(tmp@optinfo$conv$lme4$messages)
        }
        else {
          cat(tmp$message)
        }
        answer <- readline(prompt = "Do you want to continue (y/n)? ")
        
        while (answer != "y" & answer != "n") {
          # play.sound(FALSE)
          readline("Model on your orignal data contain warnings.")
          answer <- readline(prompt = "Do you want to continue (y/n)? ")
        }
        if (answer == "n") {
          suppressMessages(stop(print(tmp)))
        }
        else {
          obs <- summary(tmp)
          obs$fit <- fitted(tmp)
          obs$family <- paste(family)
        }
      }
      else {
        obs <- summary(tmp)
        obs$fit <- fitted(tmp)
        obs$family <- paste(family)
      }
      
      cat("Original model : ", "\n", "\n")
      print(summary(obs))
      
      at <- attributes(ant)
      ant <- ant[-1]
      attributes(ant) <- at
      
      ctrl <- attributes(ant)$rf
      labels <- attributes(ant)$labels
      # Test along the list of permutation data ------------------------------------------------------------------------
      if (progress == TRUE) {
        tmp.env <- new.env()
        tmp.env$error <- NULL
        
        permuted <- lapply(ant, function(d, formula, family, ctrl, labels, odf, ...) {
          cat("  Processing permutation : ", attributes(d)$permutation, "\r")
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = d, family = family, ...))), error = identity)
          
          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }
          
          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
          }
          
          while (all(test) != TRUE) {
            newdf <- redo.perm.net.nl.str(df = odf, labels = labels, rf = ctrl)
            r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = newdf, family = family, ...))), error = identity)
            
            if (isS4(r)) {
              r2=with(r@optinfo$derivs,solve(Hessian,gradient))
              if(max(abs(r2))<0.001){test=TRUE}
              else{
                test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
              }
            }
            if (is(r, "error")) {
              test <- FALSE
            }
            if (is(r, "warning")) {
              test <- FALSE
            }
          }
          
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, family = family, ctrl = ctrl, labels, odf, ...)
        cat("\n")
      }
      else {
        tmp.env <- new.env()
        tmp.env$error <- NULL
        
        permuted <- lapply(ant, function(d, formula, family, ctrl, labels, odf, w, ...) {
          r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = d, family = family, ...))), error = identity)
          
          if (isS4(r)) {
            r2=with(r@optinfo$derivs,solve(Hessian,gradient))
            if(max(abs(r2))<0.001){test=TRUE}
            else{
              test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
            }
          }
          if (is(r, "error")) {
            test <- FALSE
          }
          if (is(r, "warning")) {
            test <- FALSE
          }
          
          if (all(test) != TRUE) {
            tmp.env$error <- c(tmp.env$error, attributes(d)$permutation)
          }
          
          while (all(test) != TRUE) {
            newdf <- redo.perm.net.nl.str(df = odf, labels = labels, rf = ctrl)
            r <- tryCatch(suppressWarnings(suppressMessages(lme4::glmer(formula = formula, data = newdf, family = family, ...))), error = identity)
            if (isS4(r)) {
              r2=with(r@optinfo$derivs,solve(Hessian,gradient))
              if(max(abs(r2))<0.001){test=TRUE}
              else{
                test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
              }
            }
            if (is(r, "error")) {
              test <- FALSE
            }
            if (is(r, "warning")) {
              test <- FALSE
            }
          }
          
          r <- summary(r)$coefficients[, 1]
          return(r)
        }, formula = formula, family = family, ctrl = ctrl, labels, odf, ...)
      }
    }
    
    permuted <- do.call("rbind", permuted)
    result <- list("Original.model" = obs, "permutations" = permuted, "errors" = tmp.env$error)
    attr(result, "class") <- "ant glmm"
    attr(result, "family") <- paste(family)
    attr(result, "formula") <- format(formula)
    cat("\n")
    return(result)
  }
}
