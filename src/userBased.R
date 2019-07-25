# user based recomender system
# Una Singo 24 July 2019

library(tidyverse)
load('data/Processed/ratings.RData')



prepareMatrix = function(){
  
}

# function calculating cosine similarity
cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))}

userBased = function(matrix, ...){
  
  
  
}
