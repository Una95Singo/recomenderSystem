library(tidyverse)
library(dplyr)
library(ggplot2)

#loads ratings csv files 
load('data/Processed/recommender.RData')

set.seed(1)

#convert ratings dataset into tibble called Ratings_matrix
Ratings_matrix <- as_tibble(ratings)
head(Ratings_matrix) #first 6 observations

#get movieId, userId and ratings and store them in vectors
movieId <- Ratings_matrix$movieId
userId <- Ratings_matrix$userId
rating <- Ratings_matrix$rating

#create tbble with rows = userId and colummns = movieId with value = rating
viewed <- Ratings_matrix %>%  select(userId, movieId, rating) %>% spread(key = movieId, value = rating)
head(viewed)


#############Summary of ratings##########
summary(rating)
var(rating) 
########################

########## Histogram of ratings ##############
vector_ratings <-Ratings_matrix$rating
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings) #convert vector of ratings into factors 


qplot(vector_ratings) + ggtitle("Distribution of the ratings") + xlab("Movie ratings") + ylab("Frequency")

###################################

#####group ratings by userId #########
ratings_by_user <- group_by(Ratings_matrix,userId)

# apply summarize() to see how many movies each user has rated
ratings_by_user <- ratings_by_user  %>% summarize(count = n()) %>% arrange(desc(count))%>% head(15)

ratings_by_user$userId <- as.character(ratings_by_user$userId)
userHist <- ggplot(ratings_by_user, aes(userId, count)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("UserId") + ylab("Frequency of ratings") +ggtitle("Histogram showing distribution of top 15 users who made the most ratings")

##############

#############group ratings by Movie ############################

#most rated movies by movieId
values <- Ratings_matrix %>% group_by(movieId) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(15) 

#store movieIds in vector
movieIds <- values$movieId

#get movie titles for movieIds from ratings matrix
temp =Ratings_matrix  %>% distinct(movieId,title)   %>%  filter(movieId %in% movieIds) 

#innerjoin frequency table(movieId, count) and (movieId, title) tibble on movieId
Top_movies <- inner_join(temp,values, by= "movieId")

#plot histogram
Top_movie_hist  <- ggplot(Top_movies, aes(title, count)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Movie Title") + ylab("count") +ggtitle("Histogram showing the distribution of most rated movies")
######################################################################

