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

#' @title ANT tcltk GUI code.
#' @description tcltk code for ANT GUI.
#' @keywords internal
nodeskMetrics <- function() {
  tclServiceMode(FALSE)
  win1 <- tktoplevel()
  tcl("wm", "attributes", win1, topmost = TRUE)
  tkconfigure(win1, background = "#3f3f3f")
  tkwm.state(win1, "withdrawn")
  tcltk::tktitle(win1) <- "Animal Network Toolkit"
  img <- tkimage.create("photo", file = paste(system.file(package = "ant"), "/ressources/ANT.png", sep = ""))
  tcl("wm", "iconphoto", win1, img)
  tclServiceMode(TRUE)
  # Window label
  # Labels --------------------------------------------------------------------------------------
  space <- tklabel(win1, text = " ", background = "#3f3f3f")
  Binary <- tklabel(win1, text = "Binary", foreground = "#ffffff", background = "#3f3f3f")
  Weighted <- tklabel(win1, text = "Weigthed", foreground = "#ffffff", background = "#3f3f3f")
  Undirected <- tklabel(win1, text = "Undirected", foreground = "#ffffff", background = "#3f3f3f")
  Out.ties <- tklabel(win1, text = "Out-ties", foreground = "#ffffff", background = "#3f3f3f")
  in.ties <- tklabel(win1, text = "In-ties", foreground = "#ffffff", background = "#3f3f3f")
  ShortestPaths <- tklabel(win1, text = "strongest ties", foreground = "#ffffff", background = "#3f3f3f")
  Normalize <- tklabel(win1, text = "Normalized", foreground = "#ffffff", background = "#3f3f3f")

  # affinity ------------------------------------------------------------------------------------
  affinity <- tklabel(win1, text = "Affinity", foreground = "#ffffff", background = "#3f3f3f")

  win1$affinity$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'affinity'
  affBi <- tclVar("0")
  tkconfigure(win1$affinity$binary, variable = affBi) # configurer la valeur initiale de la checkboxe

  win1$affinity$w <- tkcheckbutton(win1, background = "#3f3f3f")
  affW <- tclVar("0")
  tkconfigure(win1$affinity$w, variable = affW)

  win1$affinity$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  affUn <- tclVar("0")
  tkconfigure(win1$affinity$undir, variable = affUn)

  win1$affinity$out <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$affinity$out, state = "disabled")

  win1$affinity$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$affinity$ind, state = "disabled")

  win1$affinity$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$affinity$n, state = "disabled")

  win1$affinity$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$affinity$s, state = "disabled")


  # betweenness ---------------------------------------------------------------------------------
  betweenness <- tklabel(win1, text = "Betweenness", foreground = "#ffffff", background = "#3f3f3f")

  win1$betweenness$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'betweenness'
  betBi <- tclVar("0")
  tkconfigure(win1$betweenness$binary, variable = betBi) # configurer la valeur initiale de la checkboxe
  win1$betweenness$w <- tkcheckbutton(win1, background = "#3f3f3f")
  betW <- tclVar("0")
  tkconfigure(win1$betweenness$w, variable = betW)
  win1$betweenness$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  betUn <- tclVar("0")
  tkconfigure(win1$betweenness$undir, variable = betUn)
  win1$betweenness$out <- tkcheckbutton(win1, background = "#3f3f3f")
  betOut <- tclVar("0")
  tkconfigure(win1$betweenness$out, variable = betOut)
  win1$betweenness$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  betIn <- tclVar("0")
  tkconfigure(win1$betweenness$ind, variable = betIn)
  win1$betweenness$n <- tkcheckbutton(win1, background = "#3f3f3f")
  betN <- tclVar("0")
  tkconfigure(win1$betweenness$n, variable = betN)
  win1$betweenness$s <- tkcheckbutton(win1, background = "#3f3f3f")
  betS <- tclVar("0")
  tkconfigure(win1$betweenness$s, variable = betS)

  # degree --------------------------------------------------------------------------------------
  degree <- tklabel(win1, text = "Degree", foreground = "#ffffff", background = "#3f3f3f")

  win1$degree$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'degree'
  tkconfigure(win1$degree$binary, state = "disabled") # configurer la valeur initiale de la checkboxe

  win1$degree$w <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$degree$w, state = "disabled")

  win1$degree$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  degUn <- tclVar("0")
  tkconfigure(win1$degree$undir, variable = degUn)

  win1$degree$out <- tkcheckbutton(win1, background = "#3f3f3f")
  degOut <- tclVar("0")
  tkconfigure(win1$degree$out, variable = degOut)

  win1$degree$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  degIn <- tclVar("0")
  tkconfigure(win1$degree$ind, variable = degIn)

  win1$degree$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$degree$n, state = "disabled")

  win1$degree$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$degree$s, state = "disabled")

  # disparity -----------------------------------------------------------------------------------
  disparity <- tklabel(win1, text = "Disparity", foreground = "#ffffff", background = "#3f3f3f")

  win1$disparity$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'disparity'
  tkconfigure(win1$disparity$binary, state = "disabled") # configurer la valeur initiale de la checkboxe

  win1$disparity$w <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$disparity$w, state = "disabled")

  win1$disparity$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  dispUn <- tclVar("0")
  tkconfigure(win1$disparity$undir, variable = dispUn)

  win1$disparity$out <- tkcheckbutton(win1, background = "#3f3f3f")
  dispOut <- tclVar("0")
  tkconfigure(win1$disparity$out, variable = dispOut, state = "disabled")

  win1$disparity$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  dispIn <- tclVar("0")
  tkconfigure(win1$disparity$ind, variable = dispIn, state = "disabled")

  win1$disparity$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$disparity$n, state = "disabled")

  win1$disparity$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$disparity$s, state = "disabled")

  # eigenvector ---------------------------------------------------------------------------------
  eigenvector <- tklabel(win1, text = "Eigenvector", foreground = "#ffffff", background = "#3f3f3f")

  win1$eigenvector$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'eigenvector'
  eigBi <- tclVar("0")
  tkconfigure(win1$eigenvector$binary, variable = eigBi) # configurer la valeur initiale de la checkboxe
  win1$eigenvector$w <- tkcheckbutton(win1, background = "#3f3f3f")
  eigW <- tclVar("0")
  tkconfigure(win1$eigenvector$w, variable = eigW)
  win1$eigenvector$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  eigUn <- tclVar("0")
  tkconfigure(win1$eigenvector$undir, variable = eigUn)
  win1$eigenvector$out <- tkcheckbutton(win1, background = "#3f3f3f")
  eigOut <- tclVar("0")
  tkconfigure(win1$eigenvector$out, variable = eigOut)
  win1$eigenvector$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  eigIn <- tclVar("0")
  tkconfigure(win1$eigenvector$ind, variable = eigIn)
  win1$eigenvector$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$eigenvector$n, state = "disabled")
  win1$eigenvector$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$eigenvector$s, state = "disabled")

  # laplacian centrality ------------------------------------------------------------------------
  lp <- tklabel(win1, text = "Laplacian centrality", foreground = "#ffffff", background = "#3f3f3f")

  win1$lp$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'lp'
  lpBi <- tclVar("0")
  tkconfigure(win1$lp$binary, variable = lpBi) # configurer la valeur initiale de la checkboxe
  win1$lp$w <- tkcheckbutton(win1, background = "#3f3f3f")
  lpW <- tclVar("0")
  tkconfigure(win1$lp$w, variable = lpW)
  win1$lp$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$lp$undir, state = "disabled")
  win1$lp$out <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$lp$out, state = "disabled")
  win1$lp$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$lp$ind, state = "disabled")
  win1$lp$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$lp$n, state = "disabled")
  win1$lp$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$lp$s, state = "disabled")

  # reach ---------------------------------------------------------------------------------------
  reach <- tklabel(win1, text = "Reach", foreground = "#ffffff", background = "#3f3f3f")

  win1$reach$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'reach'
  tkconfigure(win1$reach$binary, state = "disabled") # configurer la valeur initiale de la checkboxe

  win1$reach$w <- tkcheckbutton(win1, background = "#3f3f3f")
  reachW <- tclVar("0")
  tkconfigure(win1$reach$w, variable = reachW)

  win1$reach$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$reach$undir, state = "disabled")

  win1$reach$out <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$reach$out, state = "disabled")

  win1$reach$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$reach$ind, state = "disabled")

  win1$reach$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$reach$n, state = "disabled")

  win1$reach$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$reach$s, state = "disabled")

  # r-index -------------------------------------------------------------------------------------
  ri <- tklabel(win1, text = "R-index", foreground = "#ffffff", background = "#3f3f3f")

  win1$ri$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'ri'
  riBi <- tclVar("0")
  tkconfigure(win1$ri$binary, variable = riBi) # configurer la valeur initiale de la checkboxe
  win1$ri$w <- tkcheckbutton(win1, background = "#3f3f3f")
  riW <- tclVar("0")
  tkconfigure(win1$ri$w, variable = riW)
  win1$ri$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$ri$undir, state = "disabled")
  win1$ri$out <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$ri$out, state = "disabled")
  win1$ri$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$ri$ind, state = "disabled")
  win1$ri$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$ri$n, state = "disabled")
  win1$ri$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$ri$s, state = "disabled")

  # strength ------------------------------------------------------------------------------------
  strength <- tklabel(win1, text = "Strength", foreground = "#ffffff", background = "#3f3f3f")

  win1$strength$binary <- tkcheckbutton(win1, background = "#3f3f3f") # creation of a check boxe in envirowin1ent 'strength'
  tkconfigure(win1$strength$binary, state = "disabled") # configurer la valeur initiale de la checkboxe
  win1$strength$w <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$strength$w, state = "disabled")
  win1$strength$undir <- tkcheckbutton(win1, background = "#3f3f3f")
  strUn <- tclVar("0")
  tkconfigure(win1$strength$undir, variable = strUn)
  win1$strength$out <- tkcheckbutton(win1, background = "#3f3f3f")
  strOut <- tclVar("0")
  tkconfigure(win1$strength$out, variable = strOut)
  win1$strength$ind <- tkcheckbutton(win1, background = "#3f3f3f")
  strIn <- tclVar("0")
  tkconfigure(win1$strength$ind, variable = strIn)
  win1$strength$n <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$strength$n, state = "disabled")
  win1$strength$s <- tkcheckbutton(win1, background = "#3f3f3f")
  tkconfigure(win1$strength$s, state = "disabled")

  # Lunch analysis ------------------------------------------------------------------------------

  onOK <- function() {
    affBi.value <- as.integer(tclvalue(affBi))
    affW.value <- as.integer(tclvalue(affW))

    betBi.value <- as.integer(tclvalue(betBi))
    betUn.value <- as.integer(tclvalue(betUn))
    betOut.value <- as.integer(tclvalue(betOut))
    betIn.value <- as.integer(tclvalue(betIn))
    betW.value <- as.integer(tclvalue(betW))
    betN.value <- as.integer(tclvalue(betN))
    betS.value <- as.integer(tclvalue(betS))

    degUn.value <- as.integer(tclvalue(degUn))
    degOut.value <- as.integer(tclvalue(degOut))
    degIn.value <- as.integer(tclvalue(degIn))

    dispUn.value <- as.integer(tclvalue(dispUn))
    dispOut.value <- as.integer(tclvalue(dispOut))
    dispIn.value <- as.integer(tclvalue(dispIn))

    eigBi.value <- as.integer(tclvalue(eigBi))
    eigUn.value <- as.integer(tclvalue(eigUn))
    eigOut.value <- as.integer(tclvalue(eigOut))
    eigIn.value <- as.integer(tclvalue(eigIn))
    eigW.value <- as.integer(tclvalue(eigW))

    lpBi.value <- as.integer(tclvalue(lpBi))
    lpW.value <- as.integer(tclvalue(lpW))

    reachW.value <- as.integer(tclvalue(reachW))

    riBi.value <- as.integer(tclvalue(riBi))
    riW.value <- as.integer(tclvalue(riW))

    strUn.value <- as.integer(tclvalue(strUn))
    strOut.value <- as.integer(tclvalue(strOut))
    strIn.value <- as.integer(tclvalue(strIn))

    e <- parent.env(environment())
    e$c <- c(
      affBi.value, affW.value, betBi.value, betUn.value, betOut.value, betIn.value, betW.value, betN.value,
      betS.value, degUn.value, degOut.value, degIn.value, dispUn.value, dispOut.value, dispIn.value,
      eigBi.value, eigUn.value, eigOut.value, eigIn.value, eigW.value, lpBi.value, lpW.value, reachW.value,
      riBi.value, riW.value, strUn.value, strOut.value, strIn.value
    )
    tkdestroy(win1)
  }

  win1$butOK <- tkbutton(win1, text = "OK", width = 10, command = onOK)
  tkgrid(space, Binary, Weighted, Undirected, Out.ties, in.ties, ShortestPaths, Normalize, padx = 50, pady = 20)
  tkgrid(affinity, win1$affinity$binary, win1$affinity$w, win1$affinity$undir, win1$affinity$out, win1$affinity$ind, win1$affinity$s, win1$affinity$n, padx = 50, pady = 20)
  tkgrid(betweenness, win1$betweenness$binary, win1$betweenness$w, win1$betweenness$undir, win1$betweenness$out, win1$betweenness$ind, win1$betweenness$s, win1$betweenness$n, padx = 50, pady = 15)
  tkgrid(degree, win1$degree$binary, win1$degree$w, win1$degree$undir, win1$degree$out, win1$degree$ind, win1$degree$s, win1$degree$n, padx = 50, pady = 15)
  tkgrid(disparity, win1$disparity$binary, win1$disparity$w, win1$disparity$undir, win1$disparity$out, win1$disparity$ind, win1$disparity$s, win1$disparity$n, padx = 50, pady = 15)
  tkgrid(eigenvector, win1$eigenvector$binary, win1$eigenvector$w, win1$eigenvector$undir, win1$eigenvector$out, win1$eigenvector$ind, win1$eigenvector$s, win1$eigenvector$n, padx = 50, pady = 15)
  tkgrid(lp, win1$lp$binary, win1$lp$w, win1$lp$undir, win1$lp$out, win1$lp$ind, win1$lp$s, win1$lp$n, padx = 50, pady = 15)
  tkgrid(reach, win1$reach$binary, win1$reach$w, win1$reach$undir, win1$reach$out, win1$reach$ind, win1$reach$s, win1$reach$n, padx = 50, pady = 15)
  tkgrid(ri, win1$ri$binary, win1$ri$w, win1$ri$undir, win1$ri$out, win1$ri$ind, win1$ri$s, win1$ri$n, padx = 50, pady = 15)
  tkgrid(strength, win1$strength$binary, win1$strength$w, win1$strength$undir, win1$strength$out, win1$strength$ind, win1$strength$n, win1$strength$s, padx = 50, pady = 15)
  tkgrid(win1$butOK, padx = 50, pady = 20)
  tkwm.state(win1, "normal")
  tkwait.window(win1)
  # tcltk:::tkfocus(win1)
  return(c)
}
