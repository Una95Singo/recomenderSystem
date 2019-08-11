# injests data object and computes predictions
# Kaluba

library(tidyverse)
load('data/Processed/recommender.RData')
set.seed(1)


viewedMoviesMatrix = ratings%>%
  complete(userId, movieId) %>% 
  select(userId, movieId, rating) %>% 
  spread(key = movieId, value = rating)

####################################################################
subset = 1:100
# tranpose to make it item based
viewedMoviesMatrix = t(viewedMoviesMatrix[,-1])

centeredRatings =viewedMoviesMatrix[-1, subset] - rowMeans(viewedMoviesMatrix[-1,subset], na.rm=T)
trueRatings = viewedMoviesMatrix[-1,subset]
centeredRatings[is.na(centeredRatings)] = 0


predict = function(usr, mov, neighbourhood, trueRatings, centeredRatings){
  # collect the users ratings
  ratings_list = trueRatings[,usr]
  
  # returns a similarity vector 
  similarity = function(x){
    return(cosine(as.numeric(temp_masked[mov,]), as.numeric(x))  )
  }
  
  
  # mask user's rating
  temp_masked = centeredRatings
  temp_masked[mov,usr] = 0
  
  sims = apply(temp_masked,1, similarity)
  # Most similarities are so we need to deal with them by removing them. 
  temp = cbind(sims,ratings_list)
  temp = temp[-mov,]
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
  if ( is.na(trueRatings[mov,usr])){
    return(list('prediction' = prediction, 'trueRating' = NA))
  }
  else{
    return(list('prediction' = prediction, 'trueRating' = as.numeric(trueRatings[mov,usr])))
  }
}


