# user based recomender system
# Una Singo 24 July 2019
library(tidyverse)
load('data/Processed/recommender.RData')
#load('data/Processed/userSimilarity.RData') Don't load this for now.
set.seed(1)


# function calculating cosine similarity
cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))}

#cosineSimilarityMatrix = function(matrix){
#  nObjects = nrow(matrix) # number of users/items
#  dotProducts = matrix %*% t(matrix) # dot products againts each row
#  diagCrossProducts = diag(dotProducts) %*% t(c(rep(1, nObjects))) # take diagonal cross products and compuet cros a and cros b
#  diagCrossProducts =  1/sqrt(diagCrossProducts * t(diagCrossProducts)) # sqrt and invert
#  return( dotProducts * diagCrossProducts )  # return similarity matrix. i-th similarity to j-th object
#}

viewedMoviesMatrix = ratings%>% 
  complete(userId, movieId) %>% 
  select(userId, movieId, rating) %>% 
  spread(key = movieId, value = rating)

####################################################################
subset = 1:10

centeredRatings =viewedMoviesMatrix[subset,-1] - rowMeans(viewedMoviesMatrix[subset,-1], na.rm=T)
trueRatings = viewedMoviesMatrix[subset,-1]
centeredRatings[is.na(centeredRatings)] = 0


predict = function(usr, mov, neighbourhood){
  # collect the users ratings
  #ratings_list = trueRatings[-usr,mov]
  ratings_list = trueRatings[,mov]
  # data store for similarities 
  sims <- c()
  # iteratively compute similarities
  
  # mask user's rating
  temp_masked = centeredRatings
  temp_masked[usr,mov] = 0
  
  for(j in 1:nrow(centeredRatings))
  {
      sims <- c(sims, cosine_sim(as.numeric(temp_masked[usr,]), as.numeric(temp_masked[j,])))
  }
  # Most similarities are so we need to deal with them by removing them. 
  temp = cbind(sims,ratings_list)
  temp = temp[-usr,]
  temp = temp[order(temp[,1],decreasing = T),]
  temp = temp[!is.na(temp[,2]),1:2] 
  
  
  # compute predictions based on neighbourhood. If neighbours are less then just use all the data. 
  if (nrow(temp)<neighbourhood) {
    prediction= sum(temp[,1]*temp[,2])/sum(temp[,1])
  }
  else{
    prediction = sum(temp[1:neighbourhood,1]* temp[1:neighbourhood,2])  / sum(temp[1:neighbourhood,1])
  }
  
  # return prediction and true rating if it exists
  if ( is.na(trueRatings[usr,mov])){
    return(list('prediction' = prediction, 'trueRating' = NA))
  }
  else{
    return(list('prediction' = prediction, 'trueRating' = as.numeric(trueRatings[usr,mov])))
  }
}

