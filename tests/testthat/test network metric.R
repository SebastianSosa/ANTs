
library(ant)
load("C:/Users/Sebastian/Dropbox/ant/data/sim.m.RData")
load("C:/Users/Sebastian/Dropbox/ant/data/sim.df.RData")
#Testing network metrics--------------------------------------------------------------------------------------------------
context("Testing network metrics")
# Affinity-------------
test_that("testing affinity",{
  # Argument M as single matrix
  expect_output(str(met.affinity(sim.m)), "Named num")
  expect_equal(round(met.affinity(sim.m),2),
               structure(c(36.09,44.71,46.85,40.91,54.31,46.42,49.48,50.04,44.05,50.38,55.86,48.09,44.17,45.27,42.25,47.38,53.95,43.52,51.02,56.72),
                         names=colnames(sim.m)))
  expect_warning(met.affinity(sim.m,df=sim.df))
  
  # Argument M as single matrix and df as a data frame
  expect_output(str(met.affinity(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.affinity(sim.m,df=sim.df,1)$affinity,2),c(36.09,44.71,46.85,40.91,54.31,46.42,49.48,50.04,44.05,50.38,55.86,48.09,44.17,45.27,42.25,47.38,53.95,43.52,51.02,56.72))

  # Argument M as a list of matrices
  expect_output(str(met.affinity(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.affinity(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  expect_equal(round(met.affinity(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)[[1]]$affinity,2),c(36.09,44.71,46.85,40.91,54.31,46.42,49.48,50.04,44.05,50.38,55.86,48.09,44.17,45.27,42.25,47.38,53.95,43.52,51.02,56.72))
  expect_equal(round(met.affinity(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)[[2]]$affinity,2),c(36.09,44.71,46.85,40.91,54.31,46.42,49.48,50.04,44.05,50.38,55.86,48.09,44.17,45.27,42.25,47.38,53.95,43.52,51.02,56.72))
  
  
}) 

# Betweenness-------------
test_that("testing betweenness",{
  # Argument M as single matrix
  expect_output(str(met.betweenness(sim.m)), "Named num")
  expect_equal(round(met.betweenness(sim.m),digits = 2),structure(c(20.89,9.89,40.81,8.67,17.34,16.78,22.20,5.23,7.46,1.43,30.21,24.98,21.72,32.49,20.55,10.29,12.51,5.63,7.53,2.57),
                                                                  names=colnames(sim.m)))
  expect_warning(met.betweenness(sim.m,df=sim.df))
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.betweenness(sim.m,df=sim.df,dfid=1)), "data.frame")
  expect_equal(round(met.betweenness(sim.m,df=sim.df,dfid=1)$norm.outbetweenness,2),c(20.89,9.89,40.81,8.67,17.34,16.78,22.20,5.23,7.46,1.43,30.21,24.98,21.72,32.49,20.55,10.29,12.51,5.63,7.53,2.57))
  
  # Argument M as a list of matrices
  expect_output(str(met.betweenness(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.betweenness(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  expect_equal(round(met.betweenness(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)[[1]]$norm.outbetweenness,2),c(20.89,9.89,40.81,8.67,17.34,16.78,22.20,5.23,7.46,1.43,30.21,24.98,21.72,32.49,20.55,10.29,12.51,5.63,7.53,2.57))
  expect_equal(round(met.betweenness(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)[[2]]$norm.outbetweenness,2),c(20.89,9.89,40.81,8.67,17.34,16.78,22.20,5.23,7.46,1.43,30.21,24.98,21.72,32.49,20.55,10.29,12.51,5.63,7.53,2.57))
  
})

# Centralisation index-------------
test_that("testing Centralisation index",{
  # Argument M as single matrix
  expect_output(str(met.ci(sim.m)), "Named num")
  expect_equal(round(met.ci(sim.m),2),structure(345.09,names='CI'))
  
  # Argument M as a list of matrices
  expect_output(str(met.ci(list(sim.m,sim.m))), "List of 2")
  
})

# Degree-------------
test_that("testing degree",{
  # Argument M as single matrix
  expect_output(str(met.degree(sim.m)), "Named num")
  expect_equal(met.degree(sim.m),structure(c(29,34,31,26,32,31,35,29,32,30,32,34,31,34,34,30,33,32,28,31), names=colnames(sim.m)))
  expect_warning(met.degree(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.degree(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(met.degree(sim.m,df=sim.df,1)$degree,c(29,34,31,26,32,31,35,29,32,30,32,34,31,34,34,30,33,32,28,31))
  
  # Argument M as a list of matrices
  expect_output(str(met.degree(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.degree(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Density-------------
test_that("testing density",{
  # Argument M as single matrix
  expect_output(str(met.density(sim.m)), "Named num")
  expect_equal(round(met.density(sim.m),2),structure(0.83,names='Density'))
  
  # Argument M as a list of matrices
  expect_output(str(met.density(list(sim.m,sim.m))), "List of 2")
  
})

# Diameter-------------
test_that("testing diameter",{
  # Argument M as single matrix
  expect_output(str(met.diameter(sim.m)), "Named num")
  expect_equal(round(met.diameter(sim.m),2),structure(1.62,names='Diameter'))
  
  # Argument M as a list of matrices
  expect_output(str(met.diameter(list(sim.m,sim.m))), "List of 2")
  
})

# Disparity-------------
test_that("testing disparity",{
  # Argument M as single matrix
  expect_output(str(met.disparity(sim.m)), "Named num")
  expect_equal(round(met.disparity(sim.m),2),structure(c(0.04,0.10,0.06,0.05,0.09,0.06,0.10,0.08,0.06,0.09,0.10,0.08,0.04,0.07,0.08,0.09,0.11,0.09,0.09,0.10), names=colnames(sim.m)))
  expect_warning(met.disparity(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.disparity(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.disparity(sim.m,df=sim.df,1)$disparity,2),c(0.04,0.10,0.06,0.05,0.09,0.06,0.10,0.08,0.06,0.09,0.10,0.08,0.04,0.07,0.08,0.09,0.11,0.09,0.09,0.10))

  # Argument M as a list of matrices
  expect_output(str(met.disparity(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.disparity(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Eigenvector-------------
test_that("testing eigenvector",{
  # Argument M as single matrix
  expect_output(str(met.eigen(sim.m)), "Named num")
  expect_equal(round(met.eigen(sim.m),2),structure(c(0.70,1.00,0.76,0.69,0.85,0.82,0.89,0.81,0.84,0.94,0.82,0.83,0.71,0.80,0.91,0.90,0.92,0.96,0.81,0.79), names=colnames(sim.m)))
  expect_warning(met.eigen(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.eigen(sim.m,df=sim.df,1)), "data.frame") 
  expect_equal(round(met.eigen(sim.m,df=sim.df,1)$eigen,2),c(0.70,1.00,0.76,0.69,0.85,0.82,0.89,0.81,0.84,0.94,0.82,0.83,0.71,0.80,0.91,0.90,0.92,0.96,0.81,0.79))

  # Argument M as a list of matrices
  expect_output(str(met.eigen(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.eigen(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Global efficiency-------------
test_that("testing Global efficiency",{
  # Argument M as single matrix
  expect_output(str(met.ge(sim.m)), "Named num")
  expect_equal(round(met.ge(sim.m),2),structure(0.83,names="Global efficiency"))
  
  # Argument M as a list of matrices
  expect_output(str(met.ge(list(sim.m,sim.m))), "List of 2")

})

# Indegree-------------
test_that("testing indegree",{
  # Argument M as single matrix
  expect_output(str(met.indegree(sim.m)), "Named num")
  expect_equal(met.indegree(sim.m),structure(c(16,17,16,15,15,15,17,14,15,15,15,16,16,18,18,14,15,17,14,16), names=colnames(sim.m)))
  expect_warning(met.indegree(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.indegree(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(met.indegree(sim.m,df=sim.df,1)$indegree,c(16,17,16,15,15,15,17,14,15,15,15,16,16,18,18,14,15,17,14,16))
  
  # Argument M as a list of matrices
  expect_output(str(met.indegree(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.indegree(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Instrength-------------
test_that("testing instrength",{
  # Argument M as single matrix
  expect_output(str(met.instrength(sim.m)), "Named num")
  expect_equal(met.instrength(sim.m),structure(c(49,60,44,45,42,48,48,43,51,51,38,46,43,48,57,50,45,59,43,36), names=colnames(sim.m)))
  expect_warning(met.instrength(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.instrength(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(met.instrength(sim.m,df=sim.df,1)$instrength,c(49,60,44,45,42,48,48,43,51,51,38,46,43,48,57,50,45,59,43,36))
  
  # Argument M as a list of matrices
  expect_output(str(met.instrength(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.instrength(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Laplacian-------------
test_that("testing laplacian",{
  # Argument M as single matrix
  expect_output(str(met.lp(sim.m)), "Named num")
  expect_equal(round(met.lp(sim.m),2),structure(c(0.11,0.18,0.12,0.11,0.15,0.14,0.16,0.14,0.14,0.17,0.14,0.14,0.11,0.14,0.16,0.16,0.16,0.17,0.14,0.13), names=colnames(sim.m)))
  expect_warning(met.lp(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.lp(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.lp(sim.m,df=sim.df,1)$lp,2),c(0.11,0.18,0.12,0.11,0.15,0.14,0.16,0.14,0.14,0.17,0.14,0.14,0.11,0.14,0.16,0.16,0.16,0.17,0.14,0.13))
  
  # Argument M as a list of matrices
  expect_output(str(met.lp(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.lp(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Outdegree-------------
test_that("testing outdegree",{
  # Argument M as single matrix
  expect_output(str(met.outdegree(sim.m)), "Named num")
  expect_equal(met.outdegree(sim.m),structure(c(13,17,15,11,17,16,18,15,17,15,17,18,15,16,16,16,18,15,14,15), names=colnames(sim.m)))
  expect_warning(met.outdegree(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.outdegree(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.outdegree(sim.m,df=sim.df,1)$outdegree,2),c(13,17,15,11,17,16,18,15,17,15,17,18,15,16,16,16,18,15,14,15))
  
  # Argument M as a list of matrices
  expect_output(str(met.outdegree(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.outdegree(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Outstrength-------------
test_that("testing outstrength",{
  # Argument M as single matrix
  expect_output(str(met.outstrength(sim.m)), "Named num")
  expect_equal(met.outstrength(sim.m),structure(c(30,55,40,32,53,44,55,49,44,54,54,48,35,43,46,51,61,50,48,54), names=colnames(sim.m)))
  expect_warning(met.outstrength(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.outstrength(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.outstrength(sim.m,df=sim.df,1)$outstrength,2),c(30,55,40,32,53,44,55,49,44,54,54,48,35,43,46,51,61,50,48,54))
  
  # Argument M as a list of matrices
  expect_output(str(met.outstrength(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.outstrength(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# Reach-------------
test_that("testing reach",{
  # Argument M as single matrix
  expect_output(str(met.reach(sim.m)), "Named num")
  expect_equal(met.reach(sim.m),structure(c(2851,5142,3935,3150,5159,4271,5096,4604,4185,5290,5139,4520,3445,4120,4352,4785,5719,4744,4643,5105), names=colnames(sim.m)))
  expect_warning(met.reach(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.reach(sim.m,df=sim.df,1)), "data.frame")  
  expect_equal(round(met.reach(sim.m,df=sim.df,1)$reach,2),c(2851,5142,3935,3150,5159,4271,5096,4604,4185,5290,5139,4520,3445,4120,4352,4785,5719,4744,4643,5105))
  
  # Argument M as a list of matrices
  expect_output(str(met.reach(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.reach(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
  
})

# R-index-------------
test_that("testing R-index",{
  # Argument M as single matrix
  expect_output(str(met.ri(sim.m)), "Named num")
  expect_equal(round(met.ri(sim.m),2),structure(c(0.38,0.48,0.48,0.42,0.56,0.48,0.53,0.53,0.46,0.51,0.59,0.51,0.45,0.47,0.45,0.50,0.58,0.46,0.53,0.60), names=colnames(sim.m)))
  expect_warning(met.ri(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.ri(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.ri(sim.m,df=sim.df,1)$ri,2),c(0.38,0.48,0.48,0.42,0.56,0.48,0.53,0.53,0.46,0.51,0.59,0.51,0.45,0.47,0.45,0.50,0.58,0.46,0.53,0.60))
  
  # Argument M as a list of matrices
  expect_output(str(met.ri(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.ri(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
})

# Strength-------------
test_that("testing strength",{
  # Argument M as single matrix
  expect_output(str(met.strength(sim.m)), "Named num")
  expect_equal(met.strength(sim.m),structure(c(79,115,84,77,95,92,103,92,95,105,92,94,78,91,103,101,106,109,91,90), names=colnames(sim.m)))
  expect_warning(met.strength(sim.m,df=sim.df))
  
  # Argument M as single matrix and argument df as a data frame
  expect_output(str(met.strength(sim.m,df=sim.df,1)), "data.frame")
  expect_equal(round(met.strength(sim.m,df=sim.df,1)$strength,2),c(79,115,84,77,95,92,103,92,95,105,92,94,78,91,103,101,106,109,91,90))
  
  # Argument M as a list of matrices
  expect_output(str(met.strength(list(sim.m,sim.m))), "List of 2")
  
  # Argument M as a list of matrices and argument df as a list of data frames
  expect_output(str(met.strength(list(sim.m,sim.m), df=list(sim.df,sim.df), dfid=1)), "List of 2")
})





#Testing permutations approaches-----------------------------------------------------------------------------------------------
context("Testing permutations approaches")
df=met.strength(sim.m,df=sim.df,1)

# Nodes labels permutations---------------
test_that("testing node label permutations",{
  # Argument df as single data frame
  expect_output(str(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=FALSE)), "List of 1001")
  expect_equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=FALSE)[[2]]$id,df$id)
  expect_equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=FALSE)[[2]]$age,df$age)
  expect_equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=FALSE)[[2]]$strength,df$strength)
  expect_false(isTRUE(all.equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=FALSE)[[2]]$sex,df$sex)))
}) 

# Links permutations----------------------
test_that("Testing link permutations",{
  # Argument M as a single matrix
  expect_output(str(perm.net.lk(sim.m, sym = FALSE, erase.diag = TRUE, nperm=10, progress=F)), "List of 11")
})

#Testing cpp functions----------------------------------------------------------------------------------------------
test_that("Testing cpp vector functions",{
  expect_equal(ant:::vec_intersect(c('a','b','e'),c('e','t','z')), intersect(c('a','b','e'),c('e','t','z')))#not handling integers
  expect_equal(ant:::vec_num_extract_IdValue(c(3,4,10,22), c(4,1)),c(3,4,10,22)[c(4,1)])
  expect_equal(ant:::vec_sum(c(1,2,3,4)), sum(c(1,2,3,4)))
  expect_equal(ant:::vec_vec_multiply(c(1,2,3),c(1,2,3)), c(1,2,3)*c(1,2,3))
  expect_equal(ant:::vec_id_Equal0(c(1,2,3,4,0,5,6,7)), which(c(1,2,3,4,0,5,6,7)==0))
  expect_equal(ant:::vec_id_sup0(c(0,1,0,1,0.2)),which(c(0,1,0,1,0.1)>0))
  expect_equal(ant:::vec_char_extract_IdValue(c('a','b','e','f'),c(1,2)),c('a','b','e','f')[c(1,2)])
}) 


