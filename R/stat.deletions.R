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

#' @title Network target & random deletion simulations
#' @description Perfoms a knockout analysis according to specific vertex attributes and a specific percentage of nodes to delete
#' @param M a square adjacency M.
#' @param attr  a vector of categorical (factor or character) or numeric (continuous) attributes of the nodes. The vector must have the same length and order as the nodes in the square adjacency matrix 'M'.
#' @param target Indicates the nodes that will be the target of deletion. If the argument 'attr' is categorical, then 'target' indicates the attribute of the node target of deletion. If the argument 'attr' is numeric,
#' then 'target' can take one of two character elements 1) 'decreasing' or 2) 'increasing' indicating whether the target of deletions are nodes with the greatest or lowest attribute's values respectively.
#' @param ndel an integer indicating the number of deletions to perform in each simulation.
#' @param nsim an integer indicating the number of simulations, \emph{i.e.} how many times to perform \emph{ndel} deletions.
#' @param weighted if \emph{true}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the higher met.strength as the shortest path.
#' @param normalization normalizes the weights of the links i.e. divides them by the average strength of the network. Argument normalization can't be TRUE when argument weighted is FALSE.
#' @param directed if \emph{false}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers outgoing ties for the diameter and the geodesic distances.
#' @param progress a boolean if \emph{TRUE} it prints the progress of the simulations.
#' @param return.mat a boolean if \emph{TRUE} it returns a list of two elements : 1) a list of matrix deletions through target deletion; 2) a list of matrix deletions through random deletion
#' @return A list of two elements:
#' \itemize{
#' \item The first element is the diameter of the network according to the option specified (weighted or not, directed or not, through lowest weights or greatest weights)
#' \item The second element is the geodesic distances between all nodes according to the option specified (weighted or not, directed or not, through lowest weights or greatest weights)
#' }
#' @details Knockout analysis allows the study of resilience properties of networks when specific nodes are removed. It is usually compared with random deletions.
#' @references Lusseau D. 2003. The emergent properties of a dolphin social network. Proceedings of the Royal Society of London Series B: Biological Sciences 270(Suppl 2):S186-S188.
#' @references Manno TG. 2008. Social networking in the Columbian ground squirrel, Spermophilus columbianus. Animal Behaviour 75(4):1221-1228.
#' @references Kanngiesser P, Sueur C, Riedl K, Grossmann J, Call J. 2011. Grooming network cohesion and the role of individuals in a captive chimpanzee group. American journal of primatology 73(8):758-767.
#' @references Sosa S. 2014. Structural Architecture of the Social Network of a Non-Human Primate (Macaca sylvanus): A Study of Its Topology in La Forêt des Singes, Rocamadour. Folia Primatologica 85(3):154-163.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples
#' # Simulating data
#' m <- matrix(sample(c(0:5), 50 * 50, TRUE), 50, 50)
#' diag(m) <- 0
#' mb <- mat.binaryzation(m)
#'
#' # Weighted categorical attribute example
#' attr <- sample(c("a", "b"), 50, TRUE)
#' t <- stat.deletions(m, attr = attr, target = "a", nsim = 2, ndel = 10)
#' t <- stat.deletions(mb, attr = attr, target = "a", nsim = 2, ndel = 10)
#' #  continous attribute example
#' attr <- c(sample(c(1:10), 50, TRUE))
#'
#' t <- stat.deletions(m, attr = attr, target = "decreasing", nsim = 2, ndel = 4)
#' t <- stat.deletions(mb, attr = attr, target = "decreasing", nsim = 2, ndel = 4)
stat.deletions <- function(M, attr, target, ndel, nsim, weighted = TRUE, shortest.weight = FALSE, normalization = FALSE, directed = TRUE, out = TRUE, progress = TRUE, return.mat = FALSE) {
  if (!is.factor(attr) & !is.character(attr) & !is.numeric(attr)) {
    stop("Argument attr must be a numeric or a vector of factors.")
  }
  # Parametrization --------------------------------------------------------------------
  ids <- colnames(M)

  # Compute metrics on argument M
  Diam <- vector(mode = "numeric", length = ndel + 1)
  Diam[1] <- met.geodesicDiameter.single(M, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]

  GE <- vector(mode = "numeric", length = ndel + 1)
  GE[1] <- met.ge(M, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)

  Density <- vector(mode = "numeric", length = ndel + 1)
  Density[1] <- met.density(M)

  R.Diam <- vector(mode = "numeric", length = ndel + 1)
  R.Diam[1] <- Diam[1]

  R.GE <- vector(mode = "numeric", length = ndel + 1)
  R.GE[1] <- GE[1]

  R.Density <- vector(mode = "numeric", length = ndel + 1)
  R.Density[1] <- Density[1]

  # If user selected return matrix, create an object that stores the matrix along the deletions
  if (return.mat == TRUE) {
    MAT <- rep(list(NA), 2)
    TMAT <- rep(list(NA), ndel + 1)
    TMAT[[1]] <- M
    RMAT <- rep(list(NA), ndel + 1)
    RMAT[[1]] <- M
  }

  # Deletion simulations for categorical attributes---------------------------------------------------------------------
  if (is.factor(attr) | is.character(attr)) {
    # Extract nodes to delete
    target.nodes <- which(attr == target)
    if (!is.character(target)) {
      stop("Argument target must be a character when argument attr is a character or a vector of factors.")
    }
    if (length(target) != 1) {
      "Argument target must be of length 1. Only one category of individuals can be set as target nodes."
    }
    if (ndel > length(target.nodes)) {
      stop("Argument ndel is greater than the number of nodes in the target category.")
    }
    if (nsim > factorial(length(target.nodes))) {
      stop("Argument nsim is greater than the total number of possible combinations of target deletions")
    }
    DIAM <- rep(list(NA), nsim)
    GLOBAL <- rep(list(NA), nsim)
    DENSITY <- rep(list(NA), nsim)

    R.DIAM <- rep(list(NA), nsim)
    R.GLOBAL <- rep(list(NA), nsim)
    R.DENSITY <- rep(list(NA), nsim)

    # For each simulation
    for (j in 1:nsim) {
      # Selecting target nodes
      T.target.nodes <- target.nodes
      R.nodes <- c(1:length(attr))

      S <- NULL
      R.S <- NULL
      for (i in 2:(ndel + 1)) {
        ### Target deletion-----
        S[i - 1] <- sample(T.target.nodes, 1) # pick a node randomly
        T.target.nodes <- T.target.nodes[!T.target.nodes %in% S] # delete in target.nodes node the node picked.
        MT <- M[-S, -S] # delete the row and the column of the node picked.
        # Computing networks metrics
        Diam[i] <- met.geodesicDiameter.single(MT, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
        GE[i] <- met.ge(MT, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        Density[i] <- met.density(MT)

        ### Random deletion-----
        R.S[i - 1] <- sample(R.nodes, 1) # pick a node randomly in attr attributes.
        R.nodes <- R.nodes[!R.nodes %in% R.S] # delete in vec node the node picked.
        MR <- M[-R.S, -R.S] # delete the row and the column of the node picked.
        # Computing network metrics
        R.Diam[i] <- met.geodesicDiameter.single(MR, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
        R.GE[i] <- met.ge(MR, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        R.Density[i] <- met.density(MR)

        if (return.mat == TRUE) {
          TMAT[i] <- MT
          RMAT[i] <- MR
        }
      }

      # Storing results
      DIAM[[j]] <- Diam
      GLOBAL[[j]] <- GE
      DENSITY[[j]] <- Density

      R.DIAM[[j]] <- R.Diam
      R.GLOBAL[[j]] <- R.GE
      R.DENSITY[[j]] <- R.Density

      if (return.mat == TRUE) {
        MAT[[1]] <- TMAT
        MAT[[2]] <- RMAT
      }
    }
  }
  # Deletion simulations for continuous attributes---------------------------------------------------------------------
  if (is.numeric(attr)) {
    if (target != "decreasing" & target != "increasing") {
      stop("Argument target must be 'decreasing' or 'increasing' when argument attr is numeric.")
    }
    if (ndel > (length(attr) - 2)) {
      stop("Argument ndel must be lower than group size-2")
    }
    if (!is.numeric(ndel)) {
      stop("Argument ndel must be numeric when argument attr is a numeric vector.")
    }
    if (length(ndel) != 1) {
      stop("Argument ndel must be of length 1.")
    }
    if (factorial(ndel) < nsim) {
      stop("Argument nsim is higher than argument ndel. You are trying to realize a number of simulations higher than the possible combinations of deletions.")
    }
    # Selecting target nodes
    if (target == "decreasing") {
      ids.ordered <- order(attr, decreasing = TRUE)
    }
    if (target == "increasing") {
      ids.ordered <- order(attr, decreasing = FALSE)
    }
    DIAM <- rep(list(NA), nsim)
    GLOBAL <- rep(list(NA), nsim)
    DENSITY <- rep(list(NA), nsim)

    R.DIAM <- rep(list(NA), nsim)
    R.GLOBAL <- rep(list(NA), nsim)
    R.DENSITY <- rep(list(NA), nsim)

    # For each simulation
    for (j in 1:nsim) {
      # Selecting target nodes for target deletion
      T.target.nodes <- ids.ordered[1:ndel]
      # Selecting all nodes for random deletion
      R.nodes <- c(1:length(attr))

      S <- NULL
      R.S <- NULL
      for (i in 2:(ndel + 1)) {
        ### Target deletion-----
        S[i - 1] <- sample(T.target.nodes, 1) # pick a node randomly
        T.target.nodes <- T.target.nodes[!T.target.nodes %in% S] # delete in target.nodes node the node picked.
        MT <- M[-S, -S] # delete the row and the column of the node picked.
        # Computing network metrics
        Diam[i] <- met.geodesicDiameter.single(MT, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
        GE[i] <- met.ge(MT, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        Density[i] <- met.density(MT)

        ### Random deletion-----
        R.S[i - 1] <- sample(R.nodes, 1) # pick a node randomly in attr attributes.
        R.nodes <- R.nodes[!R.nodes %in% R.S] # delete in vec node the node picked.
        MR <- M[-R.S, -R.S] # delete the row and the column of the node picked.
        # Computing network metrics
        R.Diam[i] <- met.geodesicDiameter.single(MR, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[1]]
        R.GE[i] <- met.ge(MR, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        R.Density[i] <- met.density(MR)

        if (return.mat == TRUE) {
          TMAT[i] <- MT
          RMAT[i] <- MR
        }
      }

      # store results
      DIAM[[j]] <- Diam
      GLOBAL[[j]] <- GE
      DENSITY[[j]] <- Density

      R.DIAM[[j]] <- R.Diam
      R.GLOBAL[[j]] <- R.GE
      R.DENSITY[[j]] <- R.Density

      if (return.mat == TRUE) {
        MAT[[1]] <- TMAT
        MAT[[2]] <- RMAT
      }
    }
  }
  # Data organization ---------------------------------------------------------------------
  # Merge network metric variations
  target.deletion <- rep(list(vector(mode = "numeric", length = ndel + 1)), 3)
  names(target.deletion) <- c("diameter", "global.efficiency", "density")
  target.deletion[[1]] <- do.call("rbind", DIAM)
  target.deletion[[2]] <- do.call("rbind", GLOBAL)
  target.deletion[[3]] <- do.call("rbind", DENSITY)

  random.deletion <- rep(list(vector(mode = "numeric", length = ndel + 1)), 3)
  names(target.deletion) <- c("diameter", "global.efficiency", "density")
  random.deletion[[1]] <- do.call("rbind", R.DIAM)
  random.deletion[[2]] <- do.call("rbind", R.GLOBAL)
  random.deletion[[3]] <- do.call("rbind", R.DENSITY)

  # Create a data frame with mean, sd, deletion number and type of deletion
  Tndel <- c(0:ndel)
  Dtype <- rep("target", ndel + 1)
  stat.target.deletion <- lapply(target.deletion, function(x, Dtype, Tndel) {
    r1 <- apply(x, 2, mean)
    r2 <- apply(x, 2, sd)
    r3 <- Tndel
    r4 <- Dtype
    r <- data.frame(r1, r2, r3, r4)
    colnames(r) <- c("mean", "sd", "deletion.nbr", "deletion.type")
    return(r)
  }, Dtype, Tndel)
  names(stat.target.deletion) <- c("diameter", "global.efficiency", "density")

  Dtype <- rep("random", ndel + 1)
  stat.random.deletion <- lapply(random.deletion, function(x, Dtype, Tndel) {
    r1 <- apply(x, 2, mean)
    r2 <- apply(x, 2, sd)
    r3 <- Tndel
    r4 <- Dtype
    r <- data.frame(r1, r2, r3, r4)
    colnames(r) <- c("mean", "sd", "deletion.nbr", "deletion.type")
    return(r)
  }, Dtype, Tndel)
  names(stat.random.deletion) <- c("diameter", "global.efficiency", "density")

  # merge the two data frames
  result <- mapply(function(x, y) {
    r <- rbind(x, y)
    rownames(r) <- c(1:nrow(r))
    return(r)
  }, stat.target.deletion, stat.random.deletion, SIMPLIFY = FALSE)

  # return -----------------------------------------------------------------------------------------
  cat("\n")
  if (return.mat == FALSE) {
    attr(result, "ANT") <- c("ANT deletions simulations whithout matrices")
    attr(result, "deletions") <- paste(ndel)
    return(result)
  }
  else {
    result(list("deletions.stat" = result, "deletion.mat" = MAT))
    attr(result, "ANT") <- c("ANT deletions simulations whith matrices")
    attr(result, "deletions") <- paste(ndel)
    return(result)
  }
}
