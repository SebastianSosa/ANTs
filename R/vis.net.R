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

#' @title Network vizualisation
#' @description This function draw network.
#' @param m A square adjacency matrix.
#' @param df A data frame of node attributes.
#' @param id A string or integer indicating the column in the data frame
#'   with nodes ids.
#' @param shape A string or integer indicating the column in the data frame
#'   according to the shape nodes must have.
#' @param size A string or integer indicating the column in the data frame
#'   according to the size nodes must have.
#' @param color A string or integer indicating the column in the data frame
#'   according to the color nodes must have.
#' @param label A string or integer indicating the column in the data frame
#'   with the labels to use.
#' @param n.col1 A string indicating the first color value with which to start
#'   the color gradiant.
#' @param n.col2 A string indicating the last color value with which to end
#'   the color gradiant.
#' @param n.size An integer indicating the size of nodes.
#' @param e.width An integer indicating the width of edges.
#' @param background A string of html color code.
#' @param solver A string indicating the spatialization solver to use among
#'   the following options: 'barnesHut', 'repulsion', 'hierarchicalRepulsion',
#'   'forceAtlas2Based'.
#' @param n.shape An optional string vector if you want to set nodes' shapes.
#'   It must be unique values of same length as argument df in shape column.
#' @param viewer A boolean indicating if plots must be displayed in R Studio
#'   viewer.
#' @return The network representation in your browser.
#' @author Sebastian Sosa, Jérôme Pansanel
#' @examples
#' sim.df= met.eigen(sim.m,sim.df)
#' head(sim.df)
#' vis.net(sim.m, sim.df, id = 'id', shape = 'sex', n.shape = c('circle', 'triangle'),
#'   size = 'eigen', color = 'age')
vis.net = function(m, df, id, shape = NULL, size = NULL, color = NULL,
                   label = NULL, n.col1 = "blue", n.col2 = "red",  n.size = 1,
                   e.width = 1, background = NULL,solver = NULL, 
                   n.shape = NULL, viewer = FALSE) {

  if (is.null(solver)) {
    solver = "forceAtlas2Based"
  }
  if(is.null(background)){
    background = 'white'
  }

  # Edge creation
  e = mat.to.edgl(m)
  
  # Removing null weights
  edges = e[e$weight!=0,]
  
  # Set column name of weights
  colnames(edges)[3] = 'width'
  
  # Preparation of nodes
  if (!is.null(n.size)) {
    edges$width = edges$width*e.width
  }

  colnames(df)[df.col.findId(df,id)] = 'id'
  # Nodes' shapes
  if (!is.null(shape)) {
    shape = df.col.findId(df,shape)

    #Nodes' shapes
    if (is.null(n.shape)) {
      s <- sample(c("square", "triangle", "box", "circle", "dot", "star",
                    "ellipse", "database", "text", "diamond"),
                  length(unique(df[,shape])),replace = FALSE)
      s2 <- rep('',nrow(df))
      for (a in 1:length(unique(df[,shape]))) {
        s2 = ifelse(df[,shape] == unique(df[,shape])[a], s[a], s2)
      }

      df$shape = s2
    } else {
      if (length(unique(df[,shape])) == length(n.shape)) {
        s2 <- rep('',nrow(df))        
        for (a in 1:length(unique(df[,shape]))) {
          s2 = ifelse(df[,shape] == unique(df[,shape])[a],n.shape[a],s2)
        }

        df$shape = s2
      } else {
        stop("Argument shape and n.shape must have the same number of levels.")
      }
    }
  }

  # Nodes' sizes
  if (!is.null(size)) {
    size = df.col.findId(df,size)
    df$value = df[,size]*n.size
  }

  # Nodes' colors
  if (!is.null(color)) {
    color = df.col.findId(df,color)
    colfunc <- colorRampPalette(c(paste(n.col1), paste(n.col2)))
    df$color = colfunc(nrow(df))
  }

  # Nodes' labels
  if (!is.null(label)) {
    df$label = df[,df.col.findId(df,label)]
  }

  if (viewer) {
    net = visNetwork(nodes = df, edges = edges)
    net = visPhysics(net, solver = solver)
    net = visNodes(net, scaling = list(label = list(enabled = T)))
    net = visOptions(net,highlightNearest = list(enabled = T, degree = 1, hover = T))
    net
  } else {
    # Preparation of visualization
    # Creating temporary html
    tempdir <- tempfile()
    dir.create(tempdir)
    htmlFile <- file.path(tempdir, "index.html")

    # Saving plot information
    net = visNetwork(nodes = df, edges = edges)
    net = visPhysics(net, solver = solver)
    net = visNodes(net, scaling = list(label = list(enabled = T)))
    net = visOptions(net,highlightNearest = list(enabled = T, degree = 1, hover = T))
    visSave(net,file = htmlFile, background = background)
    
    # Read html file in browser
    utils::browseURL(htmlFile)
  }
}

