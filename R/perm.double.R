
#' @title Double permutation approach for gambit of the group
#' @param df a data frame of individual characteristics in which store permutations.
#' @param obs a data frame of gambit of the group observations. The data frame must have a column named 'ID'.
#' @param scan  an integer indicating the column of scans of individual associations in obs.
#' @param ctrlf A confounding factor by which to control group associationsin obs.
#' @param method Which type of index of associations to calculate:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x \div x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @param nperm number of permutations to perform.
#' @param progress a boolean indicating if the permutation process must be visible.
#' @param measure a character indicating the social network measure to compute (Only those available in ANTs)
#' @param test a character indicating the test to realize to account for the social network measure
#' @param ... Additional arguments related to the social network measure to compute (argument measure).
#' @details Output need to be incorporated in a data frame and node label permutations with ANTs function perm.net.nl need to be performed before using any ANTs functions "stat.".
#' @keywords export
#' @return A numeric vector of individuals social measure corrected by double permutation approch (node label permutation can the be perfomed on this output)
#' @examples 
#' perm.double.grp(obs = sim.grp, scan ='location', ctrlf ='time', nperm = 10, method = 'sri', progress = FALSE, measure = "met.strength", test = "median")
#' @references Farine, D. R., & Carter, G. G. (2022). Permutation tests for hypothesis testing with animal social network data: Problems and potential solutions. Methods in Ecology and Evolution, 13, 144- 156. https://doi.org/10.1111/2041-210X.13741
perm.double.grp <- function(df, obs, scan, ctrlf, nperm, progress = TRUE, method = 'sri', measure, test = "median", ...){
  # Data stream permutations 
  ds = perm.ds.grp(df = sim.grp, scan = scan, ctrlf = ctrlf, perm = nperm, method = method, progress = progress)
  
  # Compute social network measure
  m = do.call("rbind", lapply(ds, function(x, measure, ...){
    do.call(measure, list(M = x, ...))
  }, measure = measure, ...))
  
  mO = m[1,] 
  m = m[-1,] 
  
  # Double permutation
  result <- mO - apply(m, 2, function(x){(do.call(test, list(x)))})
  return(result)
}


#' @title Double permutation approach for focal sampling
#' @param df a data frame of individual characteristics in which store permutations.
#' @param obs a data frame of focal observations.
#' @param focal an integer indicating the column of the focal in obs.
#' @param alters an integer indicating the column of focal's alters in obs.
#' @param ctrl a numeric vector indicating the focal number in obs..
#' @param nperm an integer indicating the number of permutations to performed.
#' @param progress a boolean indicating if the permutation process must be visible.
#' @param method Which type of index of associations to calculate:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x \div x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @param nperm number of permutations to perform.
#' @param progress a boolean indicating if the permutation process must be visible.
#' @param measure a character indicating the social network measure to compute (Only those available in ANTs)
#' @param test a character indicating the test to realize to account for the social network measure
#' @param ... Additional arguments related to the social network measure to compute (argument measure).
#' @details Output need to be incorporated in a data frame and node label permutations with ANTs function perm.net.nl need to be performed before using any ANTs functions "stat.".
#' @keywords export
#' @return A numeric vector of individuals social measure corrected by double permutation approch (node label permutation can the be perfomed on this output)
#' @examples 
#' perm.double.focal(obs = sim.focal.undirected, focal = 3, alters = 4, ctrlf = 1, nperm = 10, method = 'sri', progress = FALSE, measure = "met.strength", test = "median")
#' @references Farine, D. R., & Carter, G. G. (2022). Permutation tests for hypothesis testing with animal social network data: Problems and potential solutions. Methods in Ecology and Evolution, 13, 144- 156. https://doi.org/10.1111/2041-210X.13741
perm.double.focal <- function(df = NULL, obs, focal, alters, ctrlf, nperm, progress = TRUE, method = 'sri', measure, test = "median", ...){
  # Data stream permutations 
  ds = perm.ds.focal(df = obs, focal = focal, alters = alters, ctrl = ctrlf, nperm = nperm, method = method, progress = progress)

    # Compute social network measure
  m = do.call("rbind", lapply(ds, function(x, measure, ...){
    do.call(measure, list(M = x, ...))
  }, measure = measure, ...))

  mO = m[1,] 
  m = m[-1,] 
  
  # Double permutation
  result <- mO - apply(m, 2, function(x){(do.call(test, list(x)))})

  return(result)
}

