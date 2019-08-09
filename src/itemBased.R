# injests data object and computes predictions
# Kaluba

library(tidyverse)
library(dplyr)
load('data/Processed/recommender.RData')
load('data/Processed/recommender2.RData')
set.seed(1)

head(ratings)
movieId <- ratings$movieId
userId <- ratings$userId
ratings <- ratings$rating

matrix <- as_tibble(cbind(movieId,userId,ratings))

matrix <- as_tibble(ratings)

viewed <- matrix %>%  select(userId, movieId, rating) %>% spread(key = movieId, value = rating)
viewed_movies <- t(viewed)
head(viewed)



# function calculating cosine similarity
cosine_sim <- function(a, b){
  crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))
}

?complete
head(ratings)
head(viewedMoviesMatrix) 
head(viewed_movies)
head(ratings_red)