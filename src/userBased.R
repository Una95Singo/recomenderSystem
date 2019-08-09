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


subset = sample(x = 1:10000,size =1000)

viewedMoviesMatrix = ratings%>% 
  complete(userId, movieId) %>% 
  select(userId, movieId, rating) %>% 
  spread(key = movieId, value = rating)

subset= 1:1000
centeredRatings =viewedMoviesMatrix[subset,-1] - rowMeans(viewedMoviesMatrix[subset,-1], na.rm=T)
centeredRatings[is.na(centeredRatings)] = 0

counter = function(vect){
  length(vect[which(!is.na(vect))])
}

sum(apply(viewedMoviesMatrix, 1, counter)==2)

predict = function(usr, mov, neighbourhood){
  # collect the users ratings
  ratings_list = centeredRatings[-usr,mov]
  # data store for similarities 
  sims <- c()
  # iteratively compute similarities
  for(j in 1:nrow(centeredRatings))
  {
    if(j!= 1)
    {
      sims <- c(sims, cosine_sim(as.numeric(centeredRatings[usr,]), as.numeric(centeredRatings[j,])))
    }
  }
  # Most similarities are so we need to deal with them by removing them. 
  temp = cbind(sims,ratings_list)
  temp = temp[!is.na(temp[,1]),]
  temp = temp[order(temp[,1],decreasing = T),]
  # compute predictions
  prediction = sum(temp[1:neighbourhood,1]* temp[1:neighbourhood,2])  / sum(temp[1:neighbourhood,1])
  return(prediction)
}

rowMean=rowMeans(viewedMoviesMatrix[,-1])
sub = viewedMoviesMatrix[1:600,]
sub = sub[,]

non_NA_indices= which(!is.na(sub),arr.ind = T) 

# mask the value
N = 10
non_NA_indices_sample= sample(nrow(non_NA_indices),size =N )
true_rating = c()
predicted_rating = c()

for (i in non_NA_indices_sample){
  masked_row = non_NA_indices[i,1] # position of non NA to be masked
  masked_col = non_NA_indices[i,2] # position of non NA to be masked
  masked_duplicate = sub # duplicate the subset matrix followed by masking
  true_rating =   c(true_rating,as.numeric(masked_duplicate[masked_row, masked_col] ))# store true rating
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
  sims[is.nan(sims)] = 0 # mask nans with zero
  
  # calculate predicted rating
  #function to predict rating 
  #a = vector of seen movie
  #b - vector of user similiarities
  predicted_rating = c(predicted_rating ,predictRating(unlist(masked_duplicate[-masked_row,masked_col]), sims))
  # return prediction and true value
  #iterate to the next.
}


RMSE = (true_rating - predicted_rating)^2
paste('-----Prediction RMSE-----')
RMSE = sqrt(sum(RMSE)/length(RMSE))
predicted_rating
paste(RMSE)
---------------

predictRating <- function(a,b) {
  return( crossprod(a,b)/sum(b))
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
