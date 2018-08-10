context("Testing permutations approaches")
library(ant)
load("C:/Users/Sebastian/Dropbox/ant/data/sim.m.RData")
load("C:/Users/Sebastian/Dropbox/ant/data/sim.df.RData")
df=met.strength(sim.m,df=sim.df)
test_that("testing node label permutations",{
  # Argument df as single data frame
  expect_output(str(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)), "List of 1001")
  expect_equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)[[2]]$id,df$id)
  expect_equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)[[2]]$age,df$age)
  expect_equal(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)[[2]]$strength,df$strength)
  expect_false(perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)[[2]]$sex,df$sex)
  
  
}) 