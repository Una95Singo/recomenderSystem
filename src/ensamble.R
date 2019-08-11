# user based recomender system
# Una Singo 24 July 2019
library(dplyr)
library(tidyr)
library(NNLM)
library(scales)
load('data/Processed/recommender.RData')
#load('data/Processed/userSimilarity.RData') Don't load this for now.
set.seed(1)


# function calculating cosine similarity
cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))}

setwd("C:/Users/seray/2019 Work/DSFI/Assignment 1")
load("recommender.RData")
load("V1.RData")
#ratings <- ratings[1:10000,]
M_rats <- data.frame(cbind(ratings$userId, ratings$movieId, ratings$rating))
colnames(M_rats) <- c("userId", "movieId", "rating")
ratings_all <- complete(M_rats, userId, movieId)

# reshape from long to wide format for use with NNLM package
rat_matA <- ratings_all %>% spread(key = movieId, value = rating) %>% as.matrix()

rat_mat <- rat_matA[1:5000, ]#For testing purposes

#Remove row and column ids
rat_mat1 <- rat_mat[,2:ncol(rat_mat)]
rownames(rat_mat1) <- seq(1:nrow(rat_mat))

# viewedMoviesMatrix = ratings%>% 
#   complete(userId, movieId) %>% 
#   select(userId, movieId, rating) %>% 
#   spread(key = movieId, value = rating)

viewedMoviesMatrix <- rat_mat1

####################################################################
subset = 1:10

centeredRatings =viewedMoviesMatrix[subset,-1] - rowMeans(viewedMoviesMatrix[subset,-1], na.rm=T)
trueRatings = viewedMoviesMatrix[subset,-1]
centeredRatings[is.na(centeredRatings)] = 0


predict_UB = function(usr, mov, neighbourhood, trueRatings){
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
  
  
}#End UserBased PredictFunction

#################################
######Matrix Decomp##############

init = list(
  H0 = matrix(1, nrow = 1, ncol = ncol(rat_mat1)),
  W0 = matrix(1, nrow = nrow(rat_mat1), ncol = 1)
)

# matrix factorization with bias in
system.time(
decomp <- nnmf(A = rat_mat1,
               method = "scd",
               k = 25,
               alpha = c(0.001,0,0),
               beta = c(0.001,0,0),
               init = init, # bias terms
               max.iter = 500)
)
# results
rat_pred <- decomp$W %*% decomp$H  # includes bias terms in it (see ?nnmf)
max(rat_pred)
mse = tail(decomp$mse, n = 1)
sqrt(mse)
rat_pred1 <- rescale(rat_pred, to = c(0,5), from = range(rat_pred, na.rm = TRUE))

#Errors
train_errors <- (rat_pred - rat_mat1)^2
train_accuracy <- sqrt(mean(train_errors[!is.na(rat_mat1)]))
train_accuracy


#####################################################

library(doParallel)

corz = detectCores()

cl <- makeCluster(corz-1)
registerDoParallel(cl)

no_pred <- 8
steps <- c(100, 500, 1000, 1500, 2000, 3000, 4000, 5000)
  
#Remove row and column ids
#rownames(rat_mat1) <- seq(1:nrow(rat_mat))
rat_matB <- rat_matA[,2:ncol(rat_mat)]
preds = vector("list", 8)
rmses <- vector("logical", 8)
steps <- c(100, 500, 1000, 1500, 2000, 3000, 4000, 5000)

for(i in 1:8)
{
  rat_mat1 <- rat_matB[1:steps[i], ]#For testing purposes
  
  init = list(
    H0 = matrix(1, nrow = 1, ncol = ncol(rat_mat1)),
    W0 = matrix(1, nrow = nrow(rat_mat1), ncol = 1)
  )
  
  # matrix factorization with bias in
  library(NNLM)
    decomp <- nnmf(A = rat_mat1,
                   method = "scd",
                   k = 25,
                   alpha = c(0.001,0,0),
                   beta = c(0.001,0,0),
                   init = init, # bias terms
                   max.iter = 1000)
  
  # results
  rat_pred <- decomp$W %*% decomp$H  # includes bias terms in it (see ?nnmf)
  mse = tail(decomp$mse, n = 1)
  preds[[i]] = rat_pred
  rmses[i] = sqrt(mse)
  
}






    ######################################################

Ensamble_pred <- function(usr, mov, nghbr, pred_mat, no_users, obs_mat)
{
  Actual <- rat_mat1[usr, mov] 
  Usr_based <- predict_UB(usr = usr, mov = mov, trueRatings = obs_mat)
  Mat_based <- pred_mat(usr, mov)
  Item_based <- 0
  ens <- (Usr_based + Mat_based + Item_based) / 3
  
  return(ens)
}



