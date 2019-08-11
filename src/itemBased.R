# injests data object and computes predictions
# Kaluba

library(tidyverse)
library(dplyr)
library(ggplot2)
load('data/Processed/recommender.RData')
load('data/Processed/recommender2.RData')
set.seed(1)


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



