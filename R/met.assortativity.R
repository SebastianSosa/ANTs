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

#' @title Assortativity
#' @description Calculates the binary or weighted version of vertices Newman's assortativity for categorical or continuous attributes.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param attr a factor vector of attributes for categorical assortativity.
#' a numeric vector of attributes for continuous assortativity.
#' @param se a boolean, if \emph{TRUE} it computes the assortativity standard error.
#' @param weighted a boolean, if \emph{TRUE} it computes the weighted assortativity version of the network.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param perm.nl a boolean, if \emph{TRUE} it permutes argument \emph{attr}.
#' @param nperm an integer indicating the number of permutations wanted.
#' @return
#' \itemize{
#' \item a double representing the assortativity index of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and  if argument \emph{df} is \emph{NULL}. Each double representing the assortativity index of the corresponding matrix of the list.
#' \item A list of arguments \emph{df} with a new column of network assortativity if argument\emph{df} is not \emph{NULL} and if argument \emph{M} is a list of matrices. The name of the column is adapted according to arguments values \emph{weighted} and \emph{attr}.
#' \item A list of arguments \emph{df} with a new column of network assortativity if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @details Assortativity allows the study of homophily (preferential interaction between nodes with similar attributes) and heterophily (the preferential interaction between nodes with different attributes). Attributes can be individual characteristics such as sex or age, or individual node metrics such as the met.degree.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Newman, M. E. (2003). Mixing patterns in networks. Physical Review E, 67(2), 026126.
#' @references Farine, D. R. (2014). Measuring phenotypic assortment in animal social networks: weighted associations are more robust than binary edges. Animal Behaviour, 89, 141-153.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.

met.assortativity <- function(M, attr, se = FALSE, weighted = TRUE, df = NULL, perm.nl = TRUE, nperm = NULL) {
  # Checking if argument M is a square matrix 
  test <- is.matrix(M)
  if (test) {
    # Checking if argument attr is a factor, a character or a numeric vector
    if (all(!is.factor(attr), !is.character(attr), !is.numeric(attr)) == TRUE) {
      stop("Argument attr must be a factor or numeric vector.")
    }
    # If argument attr is a factor or a character vector, function computes categorical assortativity.
    if (is.factor(attr) | is.character(attr)) {
      # Simple matrix
      if (se == TRUE) {
        warning(cat("se method is currently not available for categorical attributes.", "\r"))
      }
      if(!is.null(nperm)){
        if(!perm.nl ){stop(cat("Testing assortativity signficance if argument M as a matrix can only be done by permuting vector of attributes.","\n",
                               "Set argument perm.nl to TRUE or use function perm.net.lk to create an object matrices with permuted links."))}
        result = met_assor_cat(M, attr)[[1]]
        for (a in 2:(nperm+1)) {
          result[a] = met_assor_cat(M, sample(attr))[[1]]
        }
        return(result)
      }
      else{
        return(met.assortativityCat(M, attr, df = df))
      }
      
    }
    # Else, it computes continous assorativity.
    else {
      if(!is.null(nperm)){
        if(!perm.nl ){stop(cat("Testing assortativity signficance if argument M as a matrix can only be done by permuting vector of attributes.","\n",
                               "Set argument perm.nl to TRUE or use function perm.net.lk to create an object matrices with permuted links."))}
        result = met_assor_cat(M, attr)[[1]]
        for (a in 2:(nperm+1)) {
          result[a] = met_assor_cat(M, sample(attr))[[1]]
        }
        return(result)
      }
      else{
        return(met.assortativityCat(M, attr, df = df))
      }
      return(met.assortatvityContinuous(M, se = se, values = attr, weighted = weighted, df = df))
    }
  }
  # If argument M is not a matrix, function works on permutation approaches
  else {
    if (se) {
      stop("Argument se is not available for permutations approaches")
    }
    # If argument perm.nl is equal to TRUE, argument attr is permuted
    if (perm.nl) {
      # Check if argument M is an object returned by perm.ds.grp, perm.ds.focal or perm.net.nl----------------------
      # This part was created to handle ANTs permutations protocols
      if (!is.null(attributes(M)$ANT)) {
        if (all(!is.factor(attr[[1]]), !is.character(attr[[1]]), !is.numeric(attr[[1]])) == TRUE) {
          stop("Argument attr must be a factor or numeric vector.")
        }
        # Check if argument M originates from a single network protocol
        test1 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
        test2 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
        test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

        # Check if argument M originates from a multiple network protocol
        test4 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
        test5 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
        test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"

        # If argumpent M originates from a single network protocol, we work on a list of matrices
        if (any(test1, test2, test3)) {
          # Checking if argument attr is a factor, a character or a numeric vector
          if (all(!is.factor(attr), !is.character(attr), !is.numeric(attr)) == TRUE) {
            stop("Argument attr must be a factor or numeric vector.")
          }
          # If argument attr is a factor or a character vector, function computes categorical assortativity on each matrix of the list M and permutes argument attr for each element.
          if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
            if (is.null(df)) {
              # Compute categorical assortativity
              result <- lapply(seq_along(M), function(x, M, attr) {
                # First element is the categorical assortativity of the original network
                if (x == 1) {
                  r <- met.assortativityCat(M[[x]], attr)[[1]]
                }
                else {
                  r <- met.assortativityCat(M[[x]], sample(attr))[[1]]
                }
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, M = M, attr = attr)

              # Merge results
              result <- do.call("rbind", result)

              # Adapt name of result according to user option selection
              if (weighted) {
                attr(result, "comment") <- paste("Weighted categorical assortativity")
              }
              else {
                attr(result, "comment") <- paste("Binary categorical assortativity")
              }
              attr(result, "class") <- "ant assortativity single matrix"
            }
            # If argument df is equal to TRUE, same as previously but adding results in the data frame
            else {
              # Compute categorical assortativity
              result <- lapply(seq_along(M), function(x, M, attr, df) {
                # First element is the categorical assortativity of the original network
                if (x == 1) {
                  r <- met.assortativityCat(M[[x]], sample(attr), df = df)
                }
                else {
                  r <- met.assortativityCat(M[[x]], sample(attr), df = df)
                }
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, M = M, attr = attr, df = df)
            }

            # If argument M is an object returned by perm.ds.grp, 
            # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
            # In case of future repermutations            
            if (test1) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.ds.focal, 
            # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
            # In case of future repermutations
            if (test2) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.net.nl, 
            # Store argument M attributes 'ANT'
            # In case of future repermutations            
            if (test3) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }

          # If argument attr is a numeric vector, function computes continuous assortativity on each matrix of the list M and permutes argument attr for each element.
          else {
            if (is.null(df)) {
              # Compute continuous assortativity
              result <- lapply(seq_along(M), function(x, M, values, weighted) {
                # First element is the continuous assortativity of the original network
                if (x == 1) {
                  r <- met.assortatvityContinuous(M[[x]], values = values, weighted = weighted)
                }
                else {
                  r <- met.assortatvityContinuous(M[[x]], values = sample(values), weighted = weighted)
                }
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, M = M, values = attr, weighted = weighted)

              # Merge results
              result <- do.call("rbind", result)

              # Adapt name of result according to user option selection
              if (weighted) {
                attr(result, "comment") <- paste("Weighted continuous assortativity")
              }
              else {
                attr(result, "comment") <- paste("Binary continuous assortativity")
              }
              attr(result, "class") <- "ant assortativity single matrix"
            }
            # If argument df is equal to TRUE, same as previously but adding results in the data frame
            else {
              result <- lapply(seq_along(M), function(x, M, values, weighted, df) {
                if (x == 1) {
                  r <- met.assortatvityContinuous(M[[x]], values = values, weighted = weighted, df = df)
                }
                else {
                  r <- met.assortatvityContinuous(M[[x]], values = sample(values), weighted = weighted, df = df)
                }
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, M = M, values = attr, weighted = weighted, df = df)
            }

            # If argument M is an object returned by perm.ds.grp, 
            # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
            # In case of future repermutations
            if (test1) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.ds.focal, 
            # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
            # In case of future repermutations
            if (test2) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.net.nl, 
            # Store argument M attributes 'ANT'
            # In case of future repermutations
            if (test3) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
        }

        # If argument M originates from a multiple network protocol, we work on a list of lists of matrices
        if (any(test4, test5, test6)) {
          # Checking if argument attr is a factor, a character or a numeric vector
          if (all(!is.factor(attr), !is.character(attr), !is.numeric(attr)) == TRUE) {
            stop("Argument attr must be a factor or numeric vector.")
          }
          # If argument attr is a factor or a character vector, function computes categorical assortativity on each matrix of the list of lists M.
          # M[i] being a list of permutations of a specific matrix.
          # Function permutes argument attr for each element.
          if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
            if (is.null(df)) {
              # Compute categorical assortativity
              result <- lapply(seq_along(M), function(i, M, attr) {
                r1 <- lapply(seq_along(M[[i]]), function(j, m, attr) {
                  # First element is the categorical assortativity of the original network
                  if (j == 1) {
                    r2 <- met.assortativityCat(m[[j]], attr)[[1]]
                  }
                  else {
                    r2 <- met.assortativityCat(m[[j]], sample(attr))[[1]]
                  }
                  attr(r2, "permutation") <- attributes(m[[j]])$permutation
                  return(r2)
                }, m = M[[i]], attr = attr[[i]])
              }, M = M, attr)
            }
            # If argument df is equal to TRUE, same as previously but adding results in the data frame
            else {
              if (!is.null(df) & is.data.frame(df)) {
                stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
              }
              if (length(M) == nrow(df)) {
                tmp <- lapply(seq_along(M), function(i, M, attr) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, attr) {
                    if (j == 1) {
                      r2 <- met.assortativityCat(m[[j]], attr)[[1]]
                    }
                    else {
                      r2 <- met.assortativityCat(m[[j]], sample(attr))[[1]]
                    }
                    return(r2)
                  }, m = M[[i]], attr[[i]])
                }, M = M, attr = attr)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(x, tmp, df) {
                  df[[x]]$assor.cat <- tmp[[x]]
                  return(df[[x]])
                }, tmp = tmp, df = df)
              }
              else {
                ldf <- do.call("rbind", df)

                tmp <- lapply(seq_along(M), function(i, M, attr) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, attr) {
                    r2 <- met.assortativityCat(m[[j]], sample(attr))[[1]]
                  }, m = M[[i]], attr[[i]])
                }, M = M, attr = attr)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                  ldf$assor.cat <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
            }

            # If argument M is an object returned by perm.ds.grp, 
            # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
            # In case of future repermutations
            if (test4) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.ds.focal, 
            # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
            # In case of future repermutations
            if (test5) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.net.nl, 
            # Store argument M attributes 'ANT'
            # In case of future repermutations
            if (test6) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
          # If argument attr is a numeric vector, function computes continuous assortativity on each matrix of the list of lists M.
          # M[i] being a list of permutations of a specific matrix.
          # Function permutes argument attr for each element.          
          else {
            if (is.null(df)) {
              # Compute continuous assortativity
              result <- lapply(seq_along(M), function(i, M, att, weighted) {
                r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                  # First element is the categorical assortativity of the original network
                  if (j == 1) {
                    r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)
                  }
                  else {
                    r2 <- met.assortatvityContinuous(m[[j]], values = sample(values), weighted = weighted)
                  }
                  attr(r2, "permutation") <- attributes(m[[j]])$permutation
                  return(r2)
                }, m = M[[i]], values = att[[i]], weighted = weighted)
              }, M = M, att = attr, weighted = weighted)
              result <- do.call(Map, c(c, result))
            }
            # If argument df is equal to TRUE, same as previously but adding results in the data frame
            else {
              if (!is.null(df) & is.data.frame(df)) {
                stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
              }
              if (length(M) == nrow(df)) {
                tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                    if (j == 1) {
                      r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)[[1]]
                    }
                    else {
                      r2 <- met.assortatvityContinuous(m[[j]], values = sample(values), weighted = weighted)[[1]]
                    }
                    return(r2)
                  }, m = M[[i]], values = values[[i]], weighted = weighted)
                }, M = M, values = attr, weighted = weighted)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(x, tmp, df) {
                  df[[x]]$assor.conti <- tmp[[x]]
                  return(df[[x]])
                }, tmp = tmp, df = df)
              }
              else {
                ldf <- do.call("rbind", df)

                tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                    if (j == 1) {
                      r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)[[1]]
                    }
                    else {
                      r2 <- met.assortatvityContinuous(m[[j]], values = sample(values), weighted = weighted)[[1]]
                    }
                    return(r2)
                  }, m = M[[i]], values = values[[i]], weighted = weighted)
                }, M = M, values = attr, weighted = weighted)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                  ldf$assor.conti <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
            }

            # If argument M is an object returned by perm.ds.grp, 
            # Store argument M attributes 'scan', 'ctrlf', 'method' and 'ANT'
            # In case of future repermutations
            if (test4) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.ds.focal, 
            # Store argument M attributes 'focal', 'ctrl', 'alters', 'method' and 'ANT'
            # In case of future repermutations            
            if (test5) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }

            # If argument M is an object returned by perm.net.nl, 
            # Store argument M attributes 'ANT'
            # In case of future repermutations
            if (test6) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
        }
      }

      # If argument M is a list of square matrices----------------------
      else {
        if (is.list(M)) {
          # Check if argument dfid is NULL
          if (is.null(df)) {
            # Check if argument attr is a list of factors, characters or numeric vectors. If so, function uses each element of the list attr to compute the assortativity of the corresponding element of the list of matrices M.
            if (is.list(attr)) {
              if (all(!is.factor(attr[[1]]), !is.character(attr[[1]]), !is.numeric(attr[[1]])) == TRUE) {
                stop("Argument attr must be a list of factors, characters or numeric vectors.")
              }
              # If argument attr is a list of factors or character vectors, function computes categorical assortativity on each matrix of the list M.
              if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
                # Compute categorical assortativity
                result <- lapply(seq_along(M), function(i, M, attr) {
                  r <- met.assortativityCat(M[[i]], sample(attr[[i]]))[[1]]
                  attr(r, "permutation") <- attributes(M[[i]])$permutation
                  return(r)
                }, M = M, attr)
                return(result)
              }
              # If argument attr is a list of numeric vectors, function computes categorical assortativity on each matrix of the list M.
              else {
                # Compute continuous assortativity
                result <- lapply(seq_along(M), function(i, M, att, weighted) {
                  r <- met.assortatvityContinuous(M[[i]], values = sample(att[[i]]), weighted = weighted)
                  attr(r, "permutation") <- attributes(M[[i]])$permutation
                  return(r)
                }, M = M, att = attr, weighted = weighted)
                return(result)
              }
            }
            # If argument attr is a vector, the function computes the assortativity on all elements of the list M with this specific vector.
            if (is.vector(attr)) {
              # If argument attr is a factor or character vector, compute categorical assortativity
              if (is.factor(attr) | is.character(attr)) {
                result <- lapply(M, met.assortativityCat, sample(attr), df = df)[[1]]
                return(result)
              }
              # If argument attr is a numeric vector, compute continuous assortativity
              else {
                result <- lapply(M, met.assortatvityContinuous, se = se, values = sample(attr), weighted = weighted, df = df)
                return(result)
              }
            }
          }
          # If argument df is a list of data frames
          else {
            if (!is.data.frame(df) & is.list(df)) {
              # If argument attr is a vector
              if (is.vector(attr)) {
                # Check if it is a character or factor vector and compute the categorical assortativity
                if (is.factor(attr) | is.character(attr)) {
                  result <- mapply(M, met.assortativityCat, df = df, attr = sample(attr), SIMPLIFY = FALSE)
                  return(result)
                }
                # Else, compute continuous assortativity
                else {
                  result <- lapply(M, met.assortatvityContinuous, df = df, se = se, values = sample(attr), weighted = weighted)
                  return(result)
                }
              }
              # If argument attr is a list
              if (is.list(attr)) {
                # Check if argument df is not null 
                if (!is.null(df) & is.data.frame(df)) {
                  stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
                }
                # If argument df is null, return a list of assortativity values for each element of the list M
                else {
                  if (all(!is.factor(attr[[1]]), !is.character(attr[[1]]), !is.numeric(attr[[1]])) == TRUE) {
                    stop("Argument attr must be a factor or numeric vector.")
                  }
                  # If argument attr is a list of factors or character vectors, function computes categorical assortativity on each matrix of the list M.
                  if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
                    # Merge data frame list
                    ldf <- do.call("rbind", df)

                    # Compute categorical assortativity
                    tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                      r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                        r2 <- met.assortatvityContinuous(m[[j]], values = sample(values), weighted = weighted)[[1]]
                      }, m = M[[i]], values = values[[i]], weighted = weighted)
                    }, M = M, values = attr, weighted = weighted)

                    # Merge results
                    tmp <- do.call(Map, c(c, tmp))

                    # Combine results with data frame in a new column named 'assor.conti'
                    result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                      ldf$assor.conti <- tmp[[i]]
                      attr(ldf, "permutation") <- i
                      return(ldf)
                    }, tmp = tmp, ldf = ldf)

                    # Return results
                    return(result)
                  }
                  # Same as previously but for continuous assortativity
                  else {
                    if (!is.null(df) & is.data.frame(df)) {
                      stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
                    }
                    else {
                      ldf <- do.call("rbind", df)

                      tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                        r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                          r2 <- met.assortatvityContinuous(m[[j]], values = sample(values), weighted = weighted)[[1]]
                        }, m = M[[i]], values = values[[i]], weighted = weighted)
                      }, M = M, values = attr, weighted = weighted)

                      tmp <- do.call(Map, c(c, tmp))

                      result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                        ldf$assor.conti <- tmp[[i]]
                        attr(ldf, "permutation") <- i
                        return(ldf)
                      }, tmp = tmp, ldf = ldf)

                      return(result)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    # If argument perm.nl is equal to FALSE, same as previously but without permuting argument attr
    else {
      if (!is.null(attributes(M)$ANT)) {
        if (all(!is.factor(attr[[1]]), !is.character(attr[[1]]), !is.numeric(attr[[1]])) == TRUE) {
          stop("Argument attr must be a factor or numeric vector.")
        }
        test1 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
        test2 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
        test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

        test4 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
        test5 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
        test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"

        if (any(test1, test2, test3)) {
          if (all(!is.factor(attr), !is.character(attr), !is.numeric(attr)) == TRUE) {
            stop("Argument attr must be a factor or numeric vector.")
          }
          if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
            if (is.null(df)) {
              result <- lapply(M, function(x, attr) {
                r <- met.assortativityCat(x, attr)[[1]]
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, attr = attr)

              result <- do.call("rbind", result)

              if (weighted) {
                attr(result, "comment") <- paste("Weighted categorical assortativity")
              }
              else {
                attr(result, "comment") <- paste("Binary categorical assortativity")
              }
              attr(result, "class") <- "ant assortativity single matrix"
            }
            else {
              result <- lapply(M, function(x, attr, df) {
                r <- met.assortativityCat(x, attr, df = df)
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, attr = attr, df = df)
            }
            if (test1) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test2) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test3) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
          else {
            if (is.null(df)) {
              result <- lapply(M, function(x, values, weighted) {
                r <- met.assortatvityContinuous(x, values = values, weighted = weighted)
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, values = attr, weighted = weighted)

              result <- do.call("rbind", result)

              if (weighted) {
                attr(result, "comment") <- paste("Weighted continuous assortativity")
              }
              else {
                attr(result, "comment") <- paste("Binary continuous assortativity")
              }
              attr(result, "class") <- "ant assortativity single matrix"
            }
            else {
              result <- lapply(M, function(x, values, weighted, df) {
                r <- met.assortatvityContinuous(x, values = values, weighted = weighted, df = df)
                attr(r, "permutation") <- attributes(r)$permutation
                return(r)
              }, values = attr, weighted = weighted, df = df)
            }

            if (test1) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test2) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test3) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
        }

        if (any(test4, test5, test6)) {
          if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
            if (is.null(df)) {
              result <- lapply(seq_along(M), function(i, M, attr) {
                r1 <- lapply(seq_along(M[[i]]), function(j, m, attr) {
                  r2 <- met.assortativityCat(m[[j]], attr)[[1]]
                  attr(r2, "permutation") <- attributes(m[[j]])$permutation
                  return(r2)
                }, m = M[[i]], attr = attr[[i]])
              }, M = M, attr)
            }
            else {
              if (!is.null(df) & is.data.frame(df)) {
                stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
              }
              if (length(M) == nrow(df)) {
                tmp <- lapply(seq_along(M), function(i, M, attr) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, attr) {
                    r2 <- met.assortativityCat(m[[j]], attr)[[1]]
                  }, m = M[[i]], attr[[i]])
                }, M = M, attr = attr)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(x, tmp, df) {
                  df[[x]]$assor.cat <- tmp[[x]]
                  return(df[[x]])
                }, tmp = tmp, df = df)
              }
              else {
                ldf <- do.call("rbind", df)

                tmp <- lapply(seq_along(M), function(i, M, attr) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, attr) {
                    r2 <- met.assortativityCat(m[[j]], attr)[[1]]
                  }, m = M[[i]], attr[[i]])
                }, M = M, attr = attr)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                  ldf$assor.cat <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
            }

            if (test1) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test2) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test3) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
          else {
            if (is.null(df)) {
              result <- lapply(seq_along(M), function(i, M, att, weighted) {
                r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                  r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)
                  attr(r2, "permutation") <- attributes(m[[j]])$permutation
                  return(r2)
                }, m = M[[i]], values = att[[i]], weighted = weighted)
              }, M = M, att = attr, weighted = weighted)
              result <- do.call(Map, c(c, result))
            }
            else {
              if (!is.null(df) & is.data.frame(df)) {
                stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
              }
              if (length(M) == nrow(df)) {
                tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                    r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)[[1]]
                  }, m = M[[i]], values = values[[i]], weighted = weighted)
                }, M = M, values = attr, weighted = weighted)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(x, tmp, df) {
                  df[[x]]$assor.conti <- tmp[[x]]
                  return(df[[x]])
                }, tmp = tmp, df = df)
              }
              else {
                ldf <- do.call("rbind", df)

                tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                  r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                    r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)[[1]]
                  }, m = M[[i]], values = values[[i]], weighted = weighted)
                }, M = M, values = attr, weighted = weighted)

                tmp <- do.call(Map, c(c, tmp))

                result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                  ldf$assor.conti <- tmp[[i]]
                  attr(ldf, "permutation") <- i
                  return(ldf)
                }, tmp = tmp, ldf = ldf)
              }
            }

            if (test1) {
              attr(result, "focal") <- attributes(M)$focal
              attr(result, "ctrl") <- attributes(M)$ctrl
              attr(result, "alters") <- attributes(M)$alters
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test2) {
              attr(result, "scan") <- attributes(M)$scan
              attr(result, "ctrlf") <- attributes(M)$ctrlf
              attr(result, "method") <- attributes(M)$method
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
            if (test3) {
              attr(result, "ANT") <- attributes(M)$ANT
              return(result)
            }
          }
        }
      }
      else {
        if (is.list(M)) {
          if (is.null(df)) {
            if (is.list(attr)) {
              if (all(!is.factor(attr[[1]]), !is.character(attr[[1]]), !is.numeric(attr[[1]])) == TRUE) {
                stop("Argument attr must be a factor or numeric vector.")
              }
              if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
                result <- lapply(seq_along(M), function(i, M, attr) {
                  r <- met.assortativityCat(M[[i]], sample(attr[[i]]))[[1]]
                  attr(r, "permutation") <- attributes(M[[i]])$permutation
                  return(r)
                }, M = M, attr)
                result <- do.call("rbind", result)

                if (weighted) {
                  attr(result, "comment") <- paste("Weighted continuous assortativity")
                }
                else {
                  attr(result, "comment") <- paste("Binary continuous assortativity")
                }
                attr(result, "class") <- "ant assortativity single matrix"
                return(result)
              }
              else {
                result <- lapply(seq_along(M), function(i, M, att, weighted) {
                  r <- met.assortatvityContinuous(M[[i]], values = sample(att[[i]]), weighted = weighted)
                  attr(r, "permutation") <- attributes(M[[i]])$permutation
                  return(r)
                }, M = M, att = attr, weighted = weighted)
                return(result)
              }
            }
            if (is.vector) {
              if (is.factor(attr) | is.character(attr)) {
                result <- lapply(M, met.assortativityCat, sample(attr), df = df)[[1]]
                return(result)
              }
              else {
                result <- lapply(M, met.assortatvityContinuous, se = se, values = sample(attr), weighted = weighted, df = df)
                return(result)
              }
            }
          }
          else {
            if (!is.data.frame(df) & is.list(df)) {
              if (is.vector(attr)) {
                if (is.factor(attr) | is.character(attr)) {
                  result <- mapply(M, met.assortativityCat, df = df, attr = attr, SIMPLIFY = FALSE)
                  return(result)
                }
                else {
                  result <- lapply(M, met.assortatvityContinuous, df = df, se = se, values = attr, weighted = weighted)
                  return(result)
                }
              }
              if (is.list(attr)) {
                if (!is.null(df) & is.data.frame(df)) {
                  stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
                }
                else {
                  if (all(!is.factor(attr[[1]]), !is.character(attr[[1]]), !is.numeric(attr[[1]])) == TRUE) {
                    stop("Argument attr must be a factor or numeric vector.")
                  }
                  if (is.factor(attr[[1]]) | is.character(attr[[1]])) {
                    ldf <- do.call("rbind", df)

                    tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                      r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                        r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)[[1]]
                      }, m = M[[i]], values = values[[i]], weighted = weighted)
                    }, M = M, values = attr, weighted = weighted)

                    tmp <- do.call(Map, c(c, tmp))

                    result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                      ldf$assor.conti <- tmp[[i]]
                      attr(ldf, "permutation") <- i
                      return(ldf)
                    }, tmp = tmp, ldf = ldf)

                    return(result)
                  }
                  else {
                    if (!is.null(df) & is.data.frame(df)) {
                      stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
                    }
                    else {
                      ldf <- do.call("rbind", df)

                      tmp <- lapply(seq_along(M), function(i, M, values, weighted) {
                        r1 <- lapply(seq_along(M[[i]]), function(j, m, values, weighted) {
                          r2 <- met.assortatvityContinuous(m[[j]], values = values, weighted = weighted)[[1]]
                        }, m = M[[i]], values = values[[i]], weighted = weighted)
                      }, M = M, values = attr, weighted = weighted)

                      tmp <- do.call(Map, c(c, tmp))

                      result <- lapply(seq_along(tmp), function(i, tmp, ldf) {
                        ldf$assor.conti <- tmp[[i]]
                        attr(ldf, "permutation") <- i
                        return(ldf)
                      }, tmp = tmp, ldf = ldf)

                      return(result)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
