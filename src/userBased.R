# user based recomender system
# Una Singo 24 July 2019

library(tidyverse)
load('data/Processed/recommender.RData')
load('data/Processed/userSimilarity.RData')
set.seed(1)


# function calculating cosine similarity
cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))}

cosineSimilarityMatrix = function(matrix){
  nObjects = nrow(matrix) # number of users/items
  dotProducts = matrix %*% t(matrix) # dot products againts each row
  diagCrossProducts = diag(dotProducts) %*% t(c(rep(1, nObjects))) # take diagonal cross products and compuet cros a and cros b
  diagCrossProducts =  1/sqrt(diagCrossProducts * t(diagCrossProducts)) # sqrt and invert
  return( dotProducts * diagCrossProducts )  # return similarity matrix. i-th similarity to j-th object
}



userBased = function(matrix, ...){
  
  
  
}





userSimilarity = cosineSimilarityMatrix(as.matrix(viewedMoviesMatrix))


#save(userSimilarity, file='data/Processed/userSimilarity.RData')


#****************************************************************************************#
#***************************************TESTING******************************************# 
#****************************************************************************************#


#test similarity matrix function
A = (matrix (if_else(rbernoulli(9, 0.65), 1, 0), nrow = 3, ncol =3)) # random matrix of 3 people's views


# bases cases 
#first row
cosine_sim(A[1,], A[2,])
cosine_sim(A[1,], A[3,])
cosine_sim(A[3,], A[3,])

#second row
cosine_sim(A[2,], A[3,])

# must be symetric
cosineSimilarityMatrix(A)



crosses = diag(A%*%t(A))%*% t(c(1,1,1))
