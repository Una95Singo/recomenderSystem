# injests data object and computes predictions
# Kaluba

library(tidyverse)
<<<<<<< HEAD
library(dplyr)
library(ggplot2)
=======
>>>>>>> 0263b9c4288a5924ffd2d5e4566563fd42f06340
load('data/Processed/recommender.RData')
set.seed(1)


<<<<<<< HEAD
#convert ratings dataset into tibble 
Ratings_matrix <- as_tibble(ratings)

movieId <- Ratings_matrix$movieId
userId <- Ratings_matrix$userId
rating <- Ratings_matrix$rating

viewed <- Ratings_matrix %>%  select(userId, movieId, rating) %>% spread(key = movieId, value = rating)
head(viewed)





# function calculating cosine similarity
cosine_sim <- function(a, b){
  crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))
}

#function to predict rating b
# a = vector of movies by user x 
# b = vector of movie similarities 
predictRating <- function(a,b) {
  predictRating <- crossprod(a,b)/sum(b)
=======
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
>>>>>>> 0263b9c4288a5924ffd2d5e4566563fd42f06340
}


sub = viewed[1:20,-1]

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
  true_rating =   c(true_rating,masked_duplicate[masked_row, masked_col] )# store true rating
  masked_duplicate[masked_row, masked_col] = NA
  # calculate similarity matrix of subset
  sims <- c()
  
  for(j in 1:nrow(masked_duplicate))
  {
    if(j!= 1)
    {
      sims <- c(sims, cosine_sim(as.numeric(masked_duplicate[1,]),as.numeric( masked_duplicate[j,]) ) )
    }
  }
  
  
  # calculate predicted rating
  #predicted_rating = c(predicted_rating)
  
  # return prediction and true value
  #iterate to the next.
  
}



