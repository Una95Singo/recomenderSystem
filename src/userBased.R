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


viewedMoviesMatrix = ratings%>% 
  complete(userId, movieId) %>% 
  select(userId, movieId, rating) %>% 
  spread(key = movieId, value = rating)


sub = viewedMoviesMatrix[1:20,-1]

non_NA_indices= which(!is.na(sub),arr.ind = T) 

# mask the value
N = 3
non_NA_indices_sample= sample(non_NA_indices,N)
true_rating = c()
predicted_rating = c()

for (i in non_NA_indices_sample){
  masked_row = non_NA_indices[i,1] # position of non NA to be masked
  masked_col = non_NA_indices[i,2] # position of non NA to be masked
  masked_duplicate = sub # duplicate the subset matrix followed by masking
  true_rating =   c(as.numeric(true_rating,masked_duplicate[masked_row, masked_col] ))# store true rating
  masked_duplicate[masked_row, masked_col] = NA
  masked_duplicate[is.na(masked_duplicate)] = 0
  # calculate similarity matrix of subset
  sims <- c()
  
  for(j in 1:nrow(masked_duplicate))
  {
    if(j!= 1)
    {
      sims <- c(sims, cosine_sim(as.numeric(masked_duplicate[1,]), as.numeric(masked_duplicate[j,] ) ))
    }
  }
  # calculate predicted rating
  #function to predict rating 
  #a = vector of seen movie
  #b - vector of user similiarities
  predicted_rating = c(predicted_rating ,predictRating(as.vector(masked_duplicate[-masked_row,masked_col]), sims) )
  # return prediction and true value
  #iterate to the next.
}


predictRating <- function(a,b) {
  predictRating <- crossprod(a,b)/sum(b)
}

predictRating(as.vector(masked_duplicate[-masked_row,masked_col]), as.vector(sims))


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
