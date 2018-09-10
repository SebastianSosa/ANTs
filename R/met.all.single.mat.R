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
  if (option[1] == T) {
    t <- met.affinity.single(M, binary = T)

    df$instrength <- t
  }
  if (option[2] == T) {
    t <- met.affinity.single(M, binary = T)

    df$affinityB <- t
  }

  # Betweenness ---------------------------------------------------------------------------------
  # Binary not noarmalized  betweenness
  if (option[3] == T) {
    t <- met.betweenness.single(M, binary = T, shortest.weight = F, normalization = F, sym = T, out = T)

    df$betweennessB <- t
  }
  if (option[4] == T) {
    t <- met.betweenness.single(M, binary = T, shortest.weight = F, normalization = F, sym = F, out = F)

    df$inbetweennessB <- t
  }
  if (option[5] == T) {
    t <- met.betweenness.single(M, binary = T, shortest.weight = F, normalization = F, sym = F, out = T)

    df$outbetweennessB <- t
  }
  # Binary noarmalized  betweenness
  if (option[6] == T) {
    t <- met.betweenness.single(M, binary = T, shortest.weight = F, normalization = T, sym = T, out = T)

    df$norm.betweennessB <- t
  }
  if (option[7] == T) {
    t <- met.betweenness.single(M, binary = T, shortest.weight = F, normalization = T, sym = F, out = F)

    df$norm.inbetweennessB <- t
  }
  if (option[8] == T) {
    t <- met.betweenness.single(M, binary = T, shortest.weight = F, normalization = T, sym = F, out = T)

    df$norm.outbetweennessB <- t
  }

  # weighted non noarmalized and through strongest links betweenness
  if (option[9] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = F, normalization = F, sym = T, out = T)

    df$betweenness <- t
  }
  if (option[10] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = F, normalization = F, sym = F, out = F)

    df$inbetweenness <- t
  }
  if (option[11] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = F, normalization = F, sym = F, out = F)

    df$outbetweenness <- t
  }

  # weighted non noarmalized and through weakest links betweenness
  if (option[12] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = T, normalization = F, sym = T, out = T)

    df$short.betweenness <- t
  }
  if (option[13] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = T, normalization = F, sym = F, out = F)

    df$short.inbetweenness <- t
  }
  if (option[14] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = T, normalization = F, sym = F, out = F)

    df$short.outbetweenness <- t
  }

  # weighted noarmalized and through strongest links betweenness
  if (option[15] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = F, normalization = T, sym = T, out = T)

    df$norm.betweenness <- t
  }
  if (option[16] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = F, normalization = T, sym = F, out = F)

    df$norm.inbetweenness <- t
  }
  if (option[17] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = F, normalization = T, sym = F, out = F)

    df$norm.outbetweenness <- t
  }

  # weighted noarmalized and through weakest links betweenness
  if (option[18] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = T, normalization = T, sym = T, out = T)

    df$norm.short.betweenness <- t
  }
  if (option[19] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = T, normalization = T, sym = F, out = F)

    df$norm.short.inbetweenness <- t
  }
  if (option[20] == T) {
    t <- met.betweenness.single(M, binary = F, shortest.weight = T, normalization = T, sym = F, out = F)

    df$norm.short.outbetweenness <- t
  }

  # Degree --------------------------------------------------------------------------------------
  if (option[21] == T) {
    t <- met.degree.single(M)

    df$degree <- t
  }
  if (option[22] == T) {
    t <- met.outdegree.single(M)

    df$outdegree <- t
  }
  if (option[23] == T) {
    t <- met.indegree.single(M)

    df$indegree <- t
  }

  # Disparity -----------------------------------------------------------------------------------
  if (option[24] == T & any(option[c(25, 26)]) == F) {
    t <- met.disparity.single(M)

    df$disparity <- t
  }
  # else{
  # tmp=met.disparity.single(M,directed=T)
  # tmp=do.call('rbind',tmp)
  # if(option[24]==T){df$disparity=tmp[,1]}
  # if(option[25]==T){df$indisparity=tmp[,1]}
  # if(option[26]==T){df$outdisparity=tmp[,1]}
  # }

  # Eigenvector ---------------------------------------------------------------------------------
  if (option[27] == T) {
    t <- met.eigen(M, sym = T, binary = T, out = F)

    df$eigenB <- t
  }
  if (option[28] == T) {
    t <- met.eigen(M, binary = T, sym = F, out = T)

    df$outeigenB <- t
  }
  if (option[29] == T) {
    t <- met.eigen(M, binary = T, sym = F, out = F)

    df$ineigenB <- t
  }
  if (option[30] == T) {
    t <- met.eigen(M, binary = F, sym = T, out = F)

    df$eigen <- t
  }

  if (option[31] == T) {
    t <- met.eigen(M, binary = F, sym = F, out = T)

    df$outeigen <- t
  }
  if (option[32] == T) {
    t <- met.eigen(M, binary = F, sym = F, out = F)

    df$ineigen <- t
  }

  # Laplacian centrality ------------------------------------------------------------------------
  if (option[33] == T) {
    t <- met.lpcB(M)

    df$lpB <- t
  }
  if (option[34] == T) {
    t <- met.lpcW(M)

    df$lp <- t
  }

  # Reach ---------------------------------------------------------------------------------------
  if (option[35] == T) {
    t <- met.reach(M)
    df$reach <- t
  }

  # RI index ------------------------------------------------------------------------------------
  if (option[37] == T) {
    t <- met.ri.single(M)

    df$ri <- t
  }

  # strength ------------------------------------------------------------------------------------
  if (option[38] == T) {
    t <- met.strength.single(M)

    df$strength <- t
  }
  if (option[39] == T) {
    t <- met.outstrength.single(M)

    df$outstrength <- t
  }
  if (option[40] == T) {
    t <- met.instrength.single(M)

    df$instrength <- t
  }
  return(df)
}
