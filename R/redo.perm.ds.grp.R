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

# This file contains the functions (except c++ functions) for repeating permutations in a data stream permutation for group fellow protocol with multiple permutations
# 6 functions can be found, ordered according to their use in the process:
#
#   1. A c++ function redo_perm_dataStream_1. This function is similar to the original perm_dataStream_1.cpp function.
#   The main difference is that this function returns a gbi for future permutations and an association matrix for network metric calculations.
#   Both of them hold the total amount of permutations in the case any error occurs while running the stat.glmm function.
#   This function is used when the first error is found as all the processes have to be repeated (npermutation according to the dpermutation where the error is found,
#   association matrix according to user's previous choice, node metric permutations).
#
#   2. A c++ function on the Redo_perm_dataStream_ControlFactor.cpp. This function is similar to the original perm_dataStream_ControlFactor.cpp
#   but it returns a list of gbi split according to the control factors for future permutations and a matrix of associations to compute node metrics.
#   Both of them hold the total amount of permutations. This function is used when the first error is found as all the processes have to be repeated (npermutation according to the dpermutation where the error is found,
#   assocaition matrix according to user's previous choice, node metric permutations).
#   This function saves the permutations in the case any error occurs while running the stat.glmm function.
#
#   3. A c++ Redo_perm_dataStream_ControlFactor_scd.cpp.
#
#   4. Redo.perm.ds.grp.cum.scd R function is run to enable the reuse of the previous 'redo_perm_dataStream_1' or 'Redo_perm_dataStream_ControlFactor.cpp' permutations,
#   directly using the gbi and the GBIcontrols.
#
#   5. met.all R function computes node network metric(s) according to a vector of strings
#
#   6. redo.ds.grp R function allows to launch all the processes previously explained.
#' @title Function for cumulative permutations on a data frame of associations
#' @description Performs cumulative data stream permutations for group fellow.
#' @param df a data frame of individuals associations
#' @return A list of two elements with all the permutations : 1) gbi or control gbi if control_factor in null or not respectivelly; 2) Association matrix
#' @keywords internal
redo.perm.ds.grp.cum <- function(df, Scan, method, control_factor, nperm) {
  if (is.null(control_factor)) {
    ### Get the column of scan; can also be used col_scan=grep("Scan",colnames(df)) ????
    col_scan <- df.col.findId(df, Scan)
    if (length(col_scan) > 1) {
      df$scan <- apply(df[, col_scan ], 1, paste, collapse = "_")
    } else {
      df$scan <- df[, col_scan]
    }

    ### convert the scan columns to factors, necessary for GBI
    df$scan <- as.factor(df$scan)

    ids <- levels(df$ID) #### IDS NEED TO BE CHARACTER OR FACTOR SO HERE I GET LEVELS; see cpp function
    col_scan <- df.col.findId(df, "scan") ### THIS IS NECESSARY, CPP FUNCTION TAKES ONE VALUE NOT A VECTOR OF VALUES!!!!!!

    ### Get the index column belonging to ID
    col_ID <- grep("^ID$", colnames(df))

    ### Create gbi matrix (GBI) groups = Scan
    GBI <- df.to.gbi(df, col_scan, col_ID)

    result <- redo_perm_dataStream_1(GBI, nperm, method = method) ### I PUT LIST_GBI, IT WAS RETURNING TO NO OBJECT!!!
    return(result)
  }

  if (!is.null(control_factor)) {
    ## FOR DEBUGGING SEND ALL LINES EACH TIME YOU RUN THE CODE!!!!
    ### Get the column of scan; can also be used col_scan=grep("Scan",colnames(df)) ????
    col_scan <- df.col.findId(df, Scan)
    if (length(col_scan) > 1) {
      df$scan <- apply(df[, col_scan ], 1, paste, collapse = "_")
    } else {
      df$scan <- df[, col_scan]
    }

    ### convert the scan columns to factors, necessary for GBI
    df$scan <- as.factor(df$scan)

    ### get ids, and col numbers of IDs and Controls
    ids <- levels(df$ID)
    col_scan <- df.col.findId(df, "scan") ### THIS IS NECESSARY, CPP FUNCTION TAKES ONE VALUE NOT A VECTOR OF VALUES!!!!!!
    col_ID <- grep("^ID$", colnames(df))

    ### CREATE A LIST OF DIFFERENT DFs DEPENDING ON FACTORS TO CONTROL
    col_id <- df.col.findId(df, control_factor)
    if (length(col_id) > 1) {
      df$control <- apply(df[, col_id ], 1, paste, collapse = "_")
    } else {
      df$control <- df[, col_id]
    }
    df$control <- as.factor(df$control)
    dfControls <- split(df, df$control)

    ############################
    GBIcontrols <- list() ## list to hold the list of gbis
    GroupOrder <- c() ### holds the names of the groups in the order put in the different gbi
    ### CREATE A GBI PER DF to CONTROL
    GBIcontrols <- lapply(dfControls, df.to.gbi, col_scan, col_ID, ids)
    ##################################

    GBI <- do.call(rbind, GBIcontrols)
    CumGbiSizes <- c(0, cumsum(sapply(GBIcontrols, nrow))) ### starts on 0 cause of C++ indexes
    result <- redo_perm_dataStream_ControlFactor(GBIcontrols, GBI, nperm, CumGbiSizes, method = method)
    names(result) <- c("GBIcontrols", "Mat")
  }
  return(result)
}

#' @title cumulative permutations on GBI or GBI with control factors
#' @description Performs cumulative data stream permutations for group fellow.
#' @param GBI gbi or control gbi if control_factor in null or not respectivelly
#' @return A list of two elements with all the permutations : 1) gbi or control gbi if control_factor in null or not respectivelly; 2) Association matrix
#' @details not need of data frame of individuals associations, permutation are directly done inside the gbi, and it return a new gbi or control gbi if control_factor in null or not respectivelly with the permutations
#' @keywords internal
redo.perm.ds.grp.cum.scd <- function(GBI, method, nperm, control_factor) {
  if (is.null(control_factor)) {
    result <- redo_perm_dataStream_1(GBI, nperm = nperm, method = method) ### I PUT LIST_GBI, IT WAS RETURNING TO NO OBJECT!!!
    return(result)
  }

  if (!is.null(control_factor)) {
    list.mat <- redo_perm_dataStream_ControlFactor_scd(GBI, nperm = nperm, method = method)
    m <- do.call("rbind", list.mat)
    r2 <- assoc.indices(m, method)
    result[[1]] <- list.mat
    result[[2]] <- r2
    return(result)
  }
  return(result)
}

#' @title All node metrics
#' @description Computes all network metrics in ANT package according to the character vector input
#' @keywords internal
met.all <- function(M, df, vec) {
  metrics <- c(
    "affinity", "affinityB", # 2
    "betweennessB", "inbetweennessB", "outbetweennessB", # 5
    "norm.betweennessB", "norm.inbetweennessB", "norm.outbetweennessB", # 8
    "betweenness", "inbetweenness", "outbetweenness", # 11
    "short.betweenness", "short.inbetweenness", "short.outbetweenness", # 14
    "norm.betweenness", "norm.inbetweenness", "norm.outbetweenness", # 17
    "norm.short.betweenness", "norm.short.inbetweenness", "norm.short.outbetweenness", # 20
    "degree", "outdegree", "indegree", # 23
    "disparity", "indisparity", "outdisparity", # 26
    "eigenB", "outeigenB", "ineigenB", "eigen", "outeigen", "ineigen", # 32
    "lpB", "lp", # 34
    "reach", "reachB", # 36
    "ri", # 37
    "strength", "outstrength", "instrength"
  ) # 40


  option <- metrics %in% vec

  # Affinity ------------------------------------------------------------------------------------
  if (option[1] == T) {
    t <- lapply(M, met.affinity.single, binary = T)
    t <- do.call("c", t)
    df$instrength <- t
  }
  if (option[2] == T) {
    t <- lapply(M, met.affinity.single, binary = T)
    t <- do.call("c", t)
    df$affinityB <- t
  }

  # Betweenness ---------------------------------------------------------------------------------
  # Binary not noarmalized  betweenness
  if (option[3] == T) {
    t <- lapply(M, met.betweenness.single, weighted = F, shortest.weight = F, normalization = F, directed = F, out = T)
    t <- do.call("c", t)
    df$betweennessB <- t
  }
  if (option[4] == T) {
    t <- lapply(M, met.betweenness.single, weighted = F, shortest.weight = F, normalization = F, directed = T, out = F)
    t <- do.call("c", t)
    df$inbetweennessB <- t
  }
  if (option[5] == T) {
    t <- lapply(M, met.betweenness.single, weighted = F, shortest.weight = F, normalization = F, directed = T, out = T)
    t <- do.call("c", t)
    df$outbetweennessB <- t
  }
  # Binary noarmalized  betweenness
  if (option[6] == T) {
    t <- lapply(M, met.betweenness.single, weighted = F, shortest.weight = F, normalization = T, directed = F, out = T)
    t <- do.call("c", t)
    df$norm.betweennessB <- t
  }
  if (option[7] == T) {
    t <- lapply(M, met.betweenness.single, weighted = F, shortest.weight = F, normalization = T, directed = T, out = F)
    t <- do.call("c", t)
    df$norm.inbetweennessB <- t
  }
  if (option[8] == T) {
    t <- lapply(M, met.betweenness.single, weighted = F, shortest.weight = F, normalization = T, directed = T, out = T)
    t <- do.call("c", t)
    df$norm.outbetweennessB <- t
  }

  # weighted non noarmalized and through strongest links betweenness
  if (option[9] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = F, normalization = F, directed = F, out = T)
    t <- do.call("c", t)
    df$betweenness <- t
  }
  if (option[10] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = F, normalization = F, directed = T, out = F)
    t <- do.call("c", t)
    df$inbetweenness <- t
  }
  if (option[11] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = F, normalization = F, directed = T, out = F)
    t <- do.call("c", t)
    df$outbetweenness <- t
  }

  # weighted non noarmalized and through weakest links betweenness
  if (option[12] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = T, normalization = F, directed = F, out = T)
    t <- do.call("c", t)
    df$short.betweenness <- t
  }
  if (option[13] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = T, normalization = F, directed = T, out = F)
    t <- do.call("c", t)
    df$short.inbetweenness <- t
  }
  if (option[14] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = T, normalization = F, directed = T, out = F)
    t <- do.call("c", t)
    df$short.outbetweenness <- t
  }

  # weighted noarmalized and through strongest links betweenness
  if (option[15] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = F, normalization = T, directed = F, out = T)
    t <- do.call("c", t)
    df$norm.betweenness <- t
  }
  if (option[16] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = F, normalization = T, directed = T, out = F)
    t <- do.call("c", t)
    df$norm.inbetweenness <- t
  }
  if (option[17] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = F, normalization = T, directed = T, out = F)
    t <- do.call("c", t)
    df$norm.outbetweenness <- t
  }

  # weighted noarmalized and through weakest links betweenness
  if (option[18] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = T, normalization = T, directed = F, out = T)
    t <- do.call("c", t)
    df$norm.short.betweenness <- t
  }
  if (option[19] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = T, normalization = T, directed = T, out = F)
    t <- do.call("c", t)
    df$norm.short.inbetweenness <- t
  }
  if (option[20] == T) {
    t <- lapply(M, met.betweenness.single, weighted = T, shortest.weight = T, normalization = T, directed = T, out = F)
    t <- do.call("c", t)
    df$norm.short.outbetweenness <- t
  }

  # Degree --------------------------------------------------------------------------------------
  if (option[21] == T) {
    t <- lapply(M, met.degree.single)
    t <- do.call("c", t)
    df$degree <- t
  }
  if (option[22] == T) {
    t <- lapply(M, met.outdegree.single)
    t <- do.call("c", t)
    df$outdegree <- t
  }
  if (option[23] == T) {
    t <- lapply(M, met.indegree.single)
    t <- do.call("c", t)
    df$indegree <- t
  }

  # Disparity -----------------------------------------------------------------------------------
  if (option[24] == T & any(option[c(25, 26)]) == F) {
    t <- lapply(M, met.disparity.single, directed = F)
    t <- do.call("c", t)
    df$disparity <- t
  }
  # else{
  # tmp=lapply(M,met.disparity.single,directed=T)
  # tmp=do.call('rbind',tmp)
  # if(option[24]==T){df$disparity=tmp[,1]}
  # if(option[25]==T){df$indisparity=tmp[,1]}
  # if(option[26]==T){df$outdisparity=tmp[,1]}
  # }

  # Eigenvector ---------------------------------------------------------------------------------
  if (option[27] == T) {
    t <- lapply(M, met.eigen, sym = T, binary = T, out = F)
    t <- do.call("c", t)
    df$eigenB <- t
  }
  if (option[28] == T) {
    t <- lapply(M, met.eigen, binary = T, sym = F, out = T)
    t <- do.call("c", t)
    df$outeigenB <- t
  }
  if (option[29] == T) {
    t <- lapply(M, met.eigen, binary = T, sym = F, out = F)
    t <- do.call("c", t)
    df$ineigenB <- t
  }
  if (option[30] == T) {
    t <- lapply(M, met.eigen, binary = F, sym = T, out = F)
    t <- do.call("c", t)
    df$eigen <- t
  }

  if (option[31] == T) {
    t <- lapply(M, met.eigen, binary = F, sym = F, out = T)
    t <- do.call("c", t)
    df$outeigen <- t
  }
  if (option[32] == T) {
    t <- lapply(M, met.eigen, binary = F, sym = F, out = F)
    t <- do.call("c", t)
    df$ineigen <- t
  }

  # Laplacian centrality ------------------------------------------------------------------------
  if (option[33] == T) {
    t <- lapply(M, met.lpcB)
    t <- do.call("c", t)
    df$lpB <- t
  }
  if (option[34] == T) {
    t <- lapply(M, met.lpcW)
    t <- do.call("c", t)
    df$lp <- t
  }

  # Reach ---------------------------------------------------------------------------------------
  if (option[35] == T) {
    t <- lapply(M, met.reach, binary = F)
    t <- do.call("c", t)
    df$reach <- t
  }
  if (option[36] == T) {
    t <- lapply(M, met.reach, binary = T)
    t <- do.call("c", t)
    df$reachB <- t
  }

  # RI index ------------------------------------------------------------------------------------
  if (option[37] == T) {
    t <- lapply(M, met.ri.single)
    t <- do.call("c", t)
    df$ri <- t
  }

  # strength ------------------------------------------------------------------------------------
  if (option[38] == T) {
    t <- lapply(M, met.strength.single)
    t <- do.call("c", t)
    df$strength <- t
  }
  if (option[39] == T) {
    t <- lapply(M, met.outstrength.single)
    t <- do.call("c", t)
    df$outstrength <- t
  }
  if (option[40] == T) {
    t <- lapply(M, met.instrength.single)
    t <- do.call("c", t)
    df$instrength <- t
  }
  return(df)
}

#' @title Group fellow data stream function for error found in permutations
#' @description Performs cumulative data stream permutations for group fellow until the glmm found no error or warnings
#' @keywords internal
redo.ds.grp <- function(family, new.perm, gbi, oda, odf, target.metrics, formula, Scan, method, ctrlf, fam, ...) {
  if (new.perm == 0) {
    # Cumulative permutations
    tmp1 <- lapply(oda, redo.perm.ds.grp.cum, Scan = Scan, method = method, control_factor = ctrlf, nperm = attributes(odf)$permutation)

    # Reordering permutations results
    GBI <- list()
    ASSOC <- list()
    for (a in 1:length(tmp1)) {
      GBI[[a]] <- tmp1[[a]][[1]]
      ASSOC[[a]] <- tmp1[[a]][[2]]
    }

    # Computing target metrics and creating new data frame for the glmm test
    new.odf <- met.all(ASSOC, odf, target.metrics)
    # Glmm test
    if (fam == "gaussian") {
      model <- tryCatch(lme4::lmer(formula = formula, data = new.odf, ...), error = identity)
    }
    # if(family=='nb'){r=tryCatch(lme4::glmer.nb(formula=formula, data = new.odf,family=family,...), error=identity)}
    if (fam != "gaussian") {
      model <- tryCatch(lme4::glmer(formula = formula, data = new.odf, family = family, ...), error = identity)
    }

    # Checking error or warnings
    if (isS4(model)) {
      test <- c(!is(model, "error"), !is(model, "warning"), model@optinfo$conv$opt == 0, length(model@optinfo$conv$lme4$messages) == 0, length(model@optinfo$warnings) == 0)
    }
    if (is(model, "error")) {
      test <- FALSE
    }
    if (is(model, "warning")) {
      test <- FALSE
    }
    # If error or warnings recale the function
    if (all(test) != TRUE) {
      redo.ds.grp(family, new.perm, gbi, oda, odf, target.metrics, formula, Scan, method, ctrlf, fam, ...)
    }

    # if no error or warnings
    else {
      # new.perm is equal to the permutation where the error or warning have been found
      new.perm <- attributes(odf)$permutation

      # Result of the function is a list of 3 ellements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
      return(list("new.perm" = new.perm, "gbi" = GBI, "model" = summary(model)$coefficients[, 1]))
    }
  }
  else {
    # Permutation to do is equal to the permutation where the error or the warning is found less the permutation already done during previous error or warning.
    nperm <- attributes(odf)$permutation - new.perm

    # Cumulative permutations
    tmp1 <- lapply(gbi, redo.perm.ds.grp.cum.scd, method = method, nperm = nperm, control_factor = ctrlf)

    # Reordering permutations results
    GBI <- list()
    ASSOC <- list()
    for (a in 1:length(tmp1)) {
      GBI[[a]] <- tmp1[[a]][[1]]
      ASSOC[[a]] <- tmp1[[a]][[2]]
    }

    # Computing target metrics and creating new data frame for the glmm test
    new.odf <- met.all(ASSOC, odf, target.metrics)

    # Glmm test
    if (fam == "gaussian") {
      model <- tryCatch(lme4::lmer(formula = formula, data = new.odf, ...), error = identity)
    }
    # if(family=='nb'){r=tryCatch(lme4::glmer.nb(formula=formula, data = new.odf,family=family,...), error=identity)}
    if (fam != "gaussian") {
      model <- tryCatch(lme4::glmer(formula = formula, data = new.odf, family = family, ...), error = identity)
    }

    # Checking error or warnings
    if (isS4(model)) {
      test <- c(!is(model, "error"), !is(model, "warning"), model@optinfo$conv$opt == 0, length(model@optinfo$conv$lme4$messages) == 0, length(model@optinfo$warnings) == 0)
    }
    if (is(model, "error")) {
      test <- FALSE
    }
    if (is(model, "warning")) {
      test <- FALSE
    }

    # If error or warnings recale the function
    if (all(test) != TRUE) {
      redo.ds.grp(family, new.perm, gbi, oda, odf, target.metrics, formula, Scan, method, ctrlf, fam, ...)
    }
    # if no error or warnings
    else {
      # Result of the function is a list of 3 ellements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
      new.perm <- attributes(odf)$permutation
      return(list("new.perm" = new.perm, "gbi" = GBI, "model" = summary(model)$coefficients[, 1]))
    }
  }
}
