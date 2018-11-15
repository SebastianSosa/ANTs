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

#' @title Diagnostic plot for lm, glm, lmer,glmer models
#' @param model a lm, glm, lmer or glmer object
#' @param formula the formula use in the model
#' @description plot Fitted values Vs residuals and residuals qqnorm plot
#' @author Sebastian Sosa, Ivan Puga Gonzalez
#' @keywords internal
stat.model.diag <- function(model) {
  par(bg = "gray63")
  par(mfrow = c(1, 2))
  if (attr(model, "class") == "summary.lm") {
    formula <- model$call
    # Fitted values versus residuals
    plot(model$fit, resid(model), xlab = "fitted values", ylab = "residuals", main = c("Fitted values Vs residuals", paste(c(formula))), col = "white")
    abline(h = 0, col = "white")
    # Residuals qqplot
    qqnorm(resid(model), col = "white")
    qqline(resid(model), col = "white")
  }
  if (attr(model, "class") == "summary.glm") {
    formula <- model$call
    # Fitted values versus residuals
    plot(model$fit, model$resid, xlab = "fitted values", ylab = "residuals", main = c("Fitted values Vs residuals", paste(c(formula))), col = "white")
    abline(h = 0, col = "white")
    # Residuals qqplot
    qqnorm(model$resid, col = "white")
    qqline(model$resid, col = "white")
  }
  if (attr(model, "class") == "summary.merMod") {
    formula <- attributes(model)$formula
    # Fitted values versus residuals
    plot(model$fit, resid(model), main = c("Fitted values Vs residuals", paste(c(formula))), ylab = "residuals", xlab = "fitted values", col = "white")
    abline(h = 0, col = "white")
    # Residuals qqplot
    qqnorm(resid(model), col = "white")
    qqline(resid(model), col = "white")
  }
  diag <- recordPlot()
  dev.off()
  return(diag)
}
