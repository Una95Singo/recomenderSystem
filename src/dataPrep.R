# Takes raw data and produces an rData file that will be used for analysis
# Una Singo
# 25 July 2019
library(tidyverse)

movies <- read.csv("data/Raw/ml-20m/movies.csv") 
ratings <-  read.csv("data/Raw/ml-20m/ratings.csv")
ratings <- left_join(movies, ratings) #j
# select the first 6000 ratings
ratings <- subset(ratings, userId<=6000)
rm(movies)

save.image('data/Raw/ratings.RData')
