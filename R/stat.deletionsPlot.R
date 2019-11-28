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

#' @title Plot for network deletion simulations
#' @description Plot mean and stard deviation of the deletion simulation in the form of dual line plots.
#' @param ant an ant object returned by function sta.deletion
#' @param col.target a character indicating the color of the target deletions
#' @param col.random a character indicating the color of the random deletions
#' @details knockout analysis allows the study of resilience properties of networks when specific nodes are removed. It is usually compared with random deletions.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples
#' M=matrix(sample(c(1:20),100,TRUE),ncol=10,nrow=10)
#' diag(M)=0
#' attr=sample(c('a','b'),10,TRUE)
#' target='a' #categorical target node attributes
#' t=stat.deletions(M,attr = attr,target = target,nsim = 2,ndel=4)
#' stat.deletionsPlot(t)

stat.deletionsPlot <- function(ant, col.target = NULL, col.random = NULL) {
  if (is.null(col.target)) {
    col.target <- "white"
  }
  if (is.null(col.random)) {
    col.random <- "black"
  }
  par(bg = "gray63")
  par(mfrow = c(1, 3))
  for (a in 1:length(ant)) {
    # First plot----
    ant1 <- split(ant[[a]], ant[[a]]$deletion.type)
    attr(ant1, "comment") <- paste(names(ant)[a])

    # Plot mean variation (obtained through simulations) for each deletion
    plot(ant1[[1]][, 1],
      type = "l", col = col.target, ylab = comment(ant1), xlab = "Deletions", xaxt = "n",
      ylim = c(min(ant[[a]]$mean) - (max(ant[[a]]$s) / 2), max(ant[[a]]$mean) + (max(ant[[a]]$s) / 2))
    )

    # sd bar
    y0 <- ant1[[1]]$mean - ant1[[1]]$sd
    y1 <- ant1[[1]]$mean + ant1[[1]]$sd
    x0 <- ant1[[1]]$deletion.nbr + 1
    segments(y0 = y0, y1 = y1, x0 = x0, col = col.target)
    e <- 0.1
    segments(y0 = y0, x0 = x0 - e, x1 = x0 + e, col = col.target)
    segments(y0 = y1, x0 = x0 - e, x1 = x0 + e, col = col.target)
    points(ant1[[1]][, 1], col = col.target)

    # second plot----
    # Plot mean variation (obtained through simulations) for each deletion
    lines(ant1[[2]][, 1], type = "l", col = col.random)
    y0 <- ant1[[2]]$mean - ant1[[2]]$sd
    y1 <- ant1[[2]]$mean + ant1[[2]]$sd
    x0 <- ant1[[2]]$deletion.nbr + 1
    segments(y0 = y0, y1 = y1, x0 = x0, col = col.random)
    e <- 0.1
    # sd bar
    segments(y0 = y0, x0 = x0 - e, x1 = x0 + e, col = col.random)
    segments(y0 = y1, x0 = x0 - e, x1 = x0 + e, col = col.random)
    points(ant1[[2]][, 1], col = col.random)
    axis(1, at = c(0:as.integer(attributes(ant)$deletions) + 1), las = 2)
  }
  title(expression("Deletions simulations ( " * phantom("target ") * "V" * phantom(" random") * " )"), col.main = "black", outer = TRUE, line = -2)
  title(expression(phantom("Deletions simulations ( ") * "target " * phantom("V") * phantom(" random )")), col.main = col.target, outer = TRUE, line = -2)
  title(expression(phantom("Deletions simulations ( target V") * " random" * phantom(" )")), col.main = col.random, outer = TRUE, line = -2)
  result <- recordPlot()
  return(result)
}
