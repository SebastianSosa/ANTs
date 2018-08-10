# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Disparity
#' @description Calculates the node metric disparity for all vertices.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param dfid an integer or a string indicating the column with individual ids in argument \emph{df}.
#' @return
#' \itemize{
#' \item An integer vector of nodes \emph{disparity} if argument \emph{df} is \emph{NULL}.
#' \item A list of integer vectors of nodes \emph{disparity} if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{disparity} titled 'disparity', if argument \emph{df} is not \emph{NULL}.
#' \item A list of arguments df with a new column for nodes \emph{disparity} if 1) argument \emph{df} is not \emph{NULL}, 2) argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and 3) argument \emph{df} is a list of data frames of same length as argument \emph{M}. The name of the column of each element of the list is adapted according to argument value \emph{binary}.
#' }
#' @details Disparity measures the variation of the weight of a node. This metric compares a node's strength with its degree. It informs on the type of edges a node \emph{i} has, \emph{i.e.} few strong edges or many weak edges.
#' Disparity of a vertex \emph{i} is the sum of the squares of the division between the weigthed edge between node \emph{i} and \emph{j} and the met.strength of node \emph{i}
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Barthélemy, M., Barrat, A., Pastor-Satorras, R., & Vespignani, A. (2005). Characterization and modeling of weighted networks. Physica a: Statistical mechanics and its applications, 346(1-2), 34-43.
#' @references Whitehead, H. (2009). SOCPROG programs: analysing animal social structures. Behavioral Ecology and Sociobiology, 63(5), 765-778.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.disparity(sim.m)
#' head(sim.df)
#' met.disparity(sim.m,df=sim.df)

met.disparity <- function(M, df = NULL, dfid = NULL) {
  test <- is.matrix(M)
  if (test) {
    if (is.null(df)) {
      result <- met.disparity.single(M, df = df, dfid = dfid)
      return(result)
    }
    else {
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        result <- met.disparity.single(M, df = df, dfid = dfid)
        return(result)
      }
      else{
        result <- met.disparity.single(M, df = df, dfid = dfid)
        return(result)
      }
    }
  }

  if (!is.null(attributes(M)$ANT)) {
    if (attributes(M)$ANT == "ANT data stream group sampling single matrix") {
      if (!is.null(df)) {
        if (!is.data.frame(df)) {
          stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
        }
      }
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      result <- lapply(M, function(x, df, dfid) {
        r <- met.disparity.single(x, df = df, dfid = dfid)
        attr(r, "permutation") <- attributes(x)$permutation
        return(r)
      }, df = df, dfid = dfid)
      attr(result, "scan") <- attributes(M)$scan
      attr(result, "ctrlf") <- attributes(M)$ctrlf
      attr(result, "method") <- attributes(M)$method
      attr(result, "ANT") <- attributes(M)$ANT
      return(result)
    }

    if (attributes(M)$ANT == "ANT data stream focal sampling single matrix") {
      if (!is.null(df)) {
        if (!is.data.frame(df)) {
          stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
        }
      }
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      result <- lapply(M, function(x, df = df, dfid = dfid) {
        r <- met.disparity.single(x, df = df, dfid = dfid)
        attr(r, "permutation") <- attributes(x)$permutation
        return(r)
      }, df = df, dfid = dfid)
      attr(result, "focal") <- attributes(M)$focal
      attr(result, "ctrl") <- attributes(M)$ctrl
      attr(result, "alters") <- attributes(M)$alters
      attr(result, "method") <- attributes(M)$method
      attr(result, "ANT") <- attributes(M)$ANT
      return(result)
    }

    if (attributes(M)$ANT == "ANT link permutations single matrix") {
      if (!is.null(df)) {
        if (!is.data.frame(df)) {
          stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
        }
      }
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      result <- lapply(M, function(x, df, dfid) {
        r <- met.disparity.single(x, df = df, dfid = dfid)
        attr(r, "permutation") <- attributes(x)$permutation
        return(r)
      }, df = df, dfid = dfid)
      attr(result, "ANT") <- attributes(M)$ANT
      return(result)
    }

    if (attributes(M)$ANT == "ANT data stream group sampling multiple matrices") {
      if (is.null(df)) {
        result <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
            attr(r2, "permutation") <- attributes(y)$permutation
            return(r2)
          })
        })
        attr(result, "scan") <- attributes(M)$scan
        attr(result, "ctrlf") <- attributes(M)$ctrlf
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
      if (!is.null(df) & is.data.frame(df)) {
        stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
      }
      if (is.null(warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way."))) {
        if (sum(unlist(lapply(seq_along(M), function(i, a) {
          nrow(a[[i]][[1]])
        }, a = M))) == nrow(df[[1]])) {
          tmp <- lapply(M, function(x) {
            r1 <- lapply(x, function(y) {
              r2 <- met.disparity.single(y)
            })
          })

          tmp <- do.call(Map, c(c, tmp))

          result <- lapply(seq_along(df), function(i, a, b) {
            a[[i]]$disparity <- b[[i]]
            return(a[[i]])
          }, a = df, b = tmp)

          return(result)
        }
        else {
          # data fame manipulation
          if (!is.null(dfid)) {
            dfid <- df.col.findId(df[[1]], dfid)
            df <- lapply(df, function(x) {
              x <- x[order(x[[dfid]]), ]
            })
          }
          else {
            warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
          }
          ldf <- do.call("rbind", df)

          tmp <- lapply(M, function(x) {
            r1 <- lapply(x, function(y) {
              r2 <- met.disparity.single(y)
            })
          })
          tmp <- do.call(Map, c(c, tmp))
          result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
            ldf$disparity <- tmp[[i]]
            attr(ldf, "permutation") <- i
            return(ldf)
          }, tmp = tmp, ldf = ldf)
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }
    }

    if (attributes(M)$ANT == "ANT data stream focal sampling multiple matrices") {
      if (is.null(df)) {
        result <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
            attr(r2, "permutation") <- attributes(y)$permutation
            return(r2)
          })
        })
        attr(result, "focal") <- attributes(M)$focal
        attr(result, "ctrl") <- attributes(M)$ctrl
        attr(result, "alters") <- attributes(M)$alters
        attr(result, "method") <- attributes(M)$method
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
      if (!is.null(df) & is.data.frame(df)) {
        stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
      }
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      if (sum(unlist(lapply(seq_along(M), function(i, a) {
        nrow(a[[i]][[1]])
      }, a = M))) == nrow(df[[1]])) {
        tmp <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
          })
        })

        tmp <- do.call(Map, c(c, tmp))

        result <- lapply(seq_along(df), function(i, a, b) {
          a[[i]]$disparity <- b[[i]]
          return(a[[i]])
        }, a = df, b = tmp)

        return(result)
      }
      else {
        # data fame manipulation
        if (!is.null(dfid)) {
          dfid <- df.col.findId(df[[1]], dfid)
          df <- lapply(df, function(x) {
            x <- x[order(x[[dfid]]), ]
          })
        }
        else {
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        }
        ldf <- do.call("rbind", df)

        tmp <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
          })
        })
        tmp <- do.call(Map, c(c, tmp))
        result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
          ldf$disparity <- tmp[[i]]
          attr(ldf, "permutation") <- i
          return(ldf)
        }, tmp = tmp, ldf = ldf)

        attr(result, "focal") <- attributes(M)$focal
        attr(result, "ctrl") <- attributes(M)$ctrl
        attr(result, "alters") <- attributes(M)$alters
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
    }

    if (attributes(M)$ANT == "ANT link permutations multiple matrices") {
      if (is.null(df)) {
        result <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
            attr(r2, "permutation") <- attributes(y)$permutation
            return(r2)
          })
        })
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
      if (!is.null(df) & is.data.frame(df)) {
        stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
      }
      if (is.null(dfid)) {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
      }
      if (sum(unlist(lapply(seq_along(M), function(i, a) {
        nrow(a[[i]][[1]])
      }, a = M))) == nrow(df[[1]])) {
        tmp <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
          })
        })

        tmp <- do.call(Map, c(c, tmp))

        result <- lapply(seq_along(df), function(i, a, b) {
          a[[i]]$disparity <- b[[i]]
          return(a[[i]])
        }, a = df, b = tmp)
        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
      else {
        # data fame manipulation
        if (!is.null(dfid)) {
          dfid <- df.col.findId(df[[1]], dfid)
          df <- lapply(df, function(x) {
            x <- x[order(x[[dfid]]), ]
          })
        }
        else {
          warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        }
        ldf <- do.call("rbind", df)

        tmp <- lapply(M, function(x) {
          r1 <- lapply(x, function(y) {
            r2 <- met.disparity.single(y)
          })
        })

        tmp <- do.call(Map, c(c, tmp))
        result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
          ldf$disparity <- tmp[[i]]
          attr(ldf, "permutation") <- i
          return(ldf)
        }, tmp = tmp, ldf = ldf)

        attr(result, "ANT") <- attributes(M)$ANT
        return(result)
      }
    }
  }

  if (!test & is.list(M)) {
    if (is.null(df) & !is.null(dfid)) {
      stop("Argument 'df' can't be NULL when argument 'dfid' isn't", "\r")
    }

    if (is.null(df) & is.null(dfid)) {
      result <- lapply(M, met.disparity.single)
      return(result)
    }

    if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
      if (!is.null(dfid)) {
        result <- mapply(met.disparity.single, M, df = df, dfid = dfid, SIMPLIFY = F)
        return(result)
      }
      else {
        warning("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.")
        result <- mapply(met.disparity.single, M, df = df, SIMPLIFY = F)
        return(result)
      }
    }
  }
}
