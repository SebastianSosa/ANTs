# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
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

met.all.single.mat <- function(M, df, vec) {
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
  if (option[1] == TRUE) {
    t <- met.affinity.single(M, binary = TRUE)

    df$instrength <- t
  }
  if (option[2] == TRUE) {
    t <- met.affinity.single(M, binary = TRUE)

    df$affinityB <- t
  }

  # Betweenness ---------------------------------------------------------------------------------
  # Binary not noarmalized  betweenness
  if (option[3] == TRUE) {
    t <- met.betweenness.single(M, binary = TRUE, shortest.weight = FALSE, normalization = FALSE, sym = TRUE, out = TRUE)

    df$betweennessB <- t
  }
  if (option[4] == TRUE) {
    t <- met.betweenness.single(M, binary = TRUE, shortest.weight = FALSE, normalization = FALSE, sym = FALSE, out = FALSE)

    df$inbetweennessB <- t
  }
  if (option[5] == TRUE) {
    t <- met.betweenness.single(M, binary = TRUE, shortest.weight = FALSE, normalization = FALSE, sym = FALSE, out = TRUE)

    df$outbetweennessB <- t
  }
  # Binary noarmalized  betweenness
  if (option[6] == TRUE) {
    t <- met.betweenness.single(M, binary = TRUE, shortest.weight = FALSE, normalization = TRUE, sym = TRUE, out = TRUE)

    df$norm.betweennessB <- t
  }
  if (option[7] == TRUE) {
    t <- met.betweenness.single(M, binary = TRUE, shortest.weight = FALSE, normalization = TRUE, sym = FALSE, out = FALSE)

    df$norm.inbetweennessB <- t
  }
  if (option[8] == TRUE) {
    t <- met.betweenness.single(M, binary = TRUE, shortest.weight = FALSE, normalization = TRUE, sym = FALSE, out = TRUE)

    df$norm.outbetweennessB <- t
  }

  # weighted non noarmalized and through strongest links betweenness
  if (option[9] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = FALSE, normalization = FALSE, sym = TRUE, out = TRUE)

    df$betweenness <- t
  }
  if (option[10] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = FALSE, normalization = FALSE, sym = FALSE, out = FALSE)

    df$inbetweenness <- t
  }
  if (option[11] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = FALSE, normalization = FALSE, sym = FALSE, out = FALSE)

    df$outbetweenness <- t
  }

  # weighted non noarmalized and through weakest links betweenness
  if (option[12] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = TRUE, normalization = FALSE, sym = TRUE, out = TRUE)

    df$short.betweenness <- t
  }
  if (option[13] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = TRUE, normalization = FALSE, sym = FALSE, out = FALSE)

    df$short.inbetweenness <- t
  }
  if (option[14] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = TRUE, normalization = FALSE, sym = FALSE, out = FALSE)

    df$short.outbetweenness <- t
  }

  # weighted noarmalized and through strongest links betweenness
  if (option[15] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = FALSE, normalization = TRUE, sym = TRUE, out = TRUE)

    df$norm.betweenness <- t
  }
  if (option[16] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = FALSE, normalization = TRUE, sym = FALSE, out = FALSE)

    df$norm.inbetweenness <- t
  }
  if (option[17] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = FALSE, normalization = TRUE, sym = FALSE, out = FALSE)

    df$norm.outbetweenness <- t
  }

  # weighted noarmalized and through weakest links betweenness
  if (option[18] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = TRUE, normalization = TRUE, sym = TRUE, out = TRUE)

    df$norm.short.betweenness <- t
  }
  if (option[19] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = TRUE, normalization = TRUE, sym = FALSE, out = FALSE)

    df$norm.short.inbetweenness <- t
  }
  if (option[20] == TRUE) {
    t <- met.betweenness.single(M, binary = FALSE, shortest.weight = TRUE, normalization = TRUE, sym = FALSE, out = FALSE)

    df$norm.short.outbetweenness <- t
  }

  # Degree --------------------------------------------------------------------------------------
  if (option[21] == TRUE) {
    t <- met.degree.single(M)

    df$degree <- t
  }
  if (option[22] == TRUE) {
    t <- met.outdegree.single(M)

    df$outdegree <- t
  }
  if (option[23] == TRUE) {
    t <- met.indegree.single(M)

    df$indegree <- t
  }

  # Disparity -----------------------------------------------------------------------------------
  if (option[24] == TRUE & any(option[c(25, 26)]) == FALSE) {
    t <- met.disparity.single(M)

    df$disparity <- t
  }
  # else{
  # tmp=met.disparity.single(M,directed=TRUE)
  # tmp=do.call('rbind',tmp)
  # if(option[24]==TRUE){df$disparity=tmp[,1]}
  # if(option[25]==TRUE){df$indisparity=tmp[,1]}
  # if(option[26]==TRUE){df$outdisparity=tmp[,1]}
  # }

  # Eigenvector ---------------------------------------------------------------------------------
  if (option[27] == TRUE) {
    t <- met.eigen(M, sym = TRUE, binary = TRUE, out = FALSE)

    df$eigenB <- t
  }
  if (option[28] == TRUE) {
    t <- met.eigen(M, binary = TRUE, sym = FALSE, out = TRUE)

    df$outeigenB <- t
  }
  if (option[29] == TRUE) {
    t <- met.eigen(M, binary = TRUE, sym = FALSE, out = FALSE)

    df$ineigenB <- t
  }
  if (option[30] == TRUE) {
    t <- met.eigen(M, binary = FALSE, sym = TRUE, out = FALSE)

    df$eigen <- t
  }

  if (option[31] == TRUE) {
    t <- met.eigen(M, binary = FALSE, sym = FALSE, out = TRUE)

    df$outeigen <- t
  }
  if (option[32] == TRUE) {
    t <- met.eigen(M, binary = FALSE, sym = FALSE, out = FALSE)

    df$ineigen <- t
  }

  # Laplacian centrality ------------------------------------------------------------------------
  if (option[33] == TRUE) {
    t <- met.lpcB(M)

    df$lpB <- t
  }
  if (option[34] == TRUE) {
    t <- met.lpcW(M)

    df$lp <- t
  }

  # Reach ---------------------------------------------------------------------------------------
  if (option[35] == TRUE) {
    t <- met.reach(M)
    df$reach <- t
  }

  # RI index ------------------------------------------------------------------------------------
  if (option[37] == TRUE) {
    t <- met.ri.single(M)

    df$ri <- t
  }

  # strength ------------------------------------------------------------------------------------
  if (option[38] == TRUE) {
    t <- met.strength.single(M)

    df$strength <- t
  }
  if (option[39] == TRUE) {
    t <- met.outstrength.single(M)

    df$outstrength <- t
  }
  if (option[40] == TRUE) {
    t <- met.instrength.single(M)

    df$instrength <- t
  }
  return(df)
}
