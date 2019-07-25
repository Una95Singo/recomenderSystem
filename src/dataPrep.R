# Takes raw data and produces an rData file that will be used for analysis
# Una Singo
# 25 July 2019
library(tidyverse)

movies <- read.csv("data/Raw/ml-20m/movies.csv") 
ratings <-  read.csv("data/Raw/ml-20m/ratings.csv")
ratings <- left_join(movies, ratings) #j
# select the first 6000 ratings
ratings <- subset(ratings, userId<=6000)
# length(unique(ratings$userId)) check how many unique users 
# 13759 unique movies
rm(movies)
paste("number of users is", length(unique(ratings$userId)))
paste("number of unique movies is", length(unique(ratings$movieId)))

# indicator matrix of viewed movies. 
viewedMoviesMatrix <- ratings%>% 
  complete(userId, movieId) %>% 
  mutate(seen = ifelse(is.na(rating), 0, 1)) %>% 
  select(userId, movieId, seen) %>% 
  spread(key = title, value = seen)

head(table(ratings_red$userId, ratings_red$movieId)) # test 

save(ratings, viewedMoviesMatrix, file = "output/recommender.RData")
