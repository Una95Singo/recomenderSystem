# Recomender systems
# Una Singo
# Serayen Govender
# Kaluba Chikonde
# 24 July 2019
library(dplyr)
library(tidyr)
library(NNLM)
library(scales)
load('data/Processed/recommender.RData')

#load('data/Processed/userSimilarity.RData') Don't load this for now.
set.seed(1)


# function calculating cosine similarity
cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))}

movie_subset = sample(1:13000,5000, replace = FALSE)
ratings1 <- ratings[ratings$movieId<1050 , ]
viewedMoviesMatrix = ratings1%>%
  complete(userId, movieId) %>%
  select(userId, movieId, rating) %>%
  spread(key = movieId, value = rating)



####################################################################
##############USER BASED PREDICTIONS################################
mov = 8429
usr = 11
neighbourhood = 7

predict_UB = function(usr, mov, neighbourhood, trueRatings, centeredRatings){
  # collect the users ratings
  ratings_list = trueRatings[,mov]
  
  # returns a similarity vector 
  similarity = function(x){
    return(cosine_sim(as.numeric(temp_masked[usr,]), as.numeric(x))  )
  }
  
  
  # mask user's rating
  temp_masked = centeredRatings
  temp_masked[usr,mov] = 0
  
  sims = apply(temp_masked,1, similarity)
  # Most similarities are so we need to deal with them by removing them. 
  temp = cbind(sims,ratings_list)
  temp = temp[-usr,]
  temp = temp[order(temp[,1],decreasing = T),]
  temp = temp[!is.na(temp[,2]),1:2] 
  
  
  # if temp only has one row.
  if (is.null(nrow(temp))){
    prediction = as.numeric(temp[1])* as.numeric(temp[2])
  }
  # compute predictions based on neighbourhood. If neighbours are less then just use all the data. 
  else if (nrow(temp) < neighbourhood) {
    prediction= sum(temp[,1]*temp[,2])/sum(temp[,1])
  }
  else{
    prediction = sum(temp[1:neighbourhood,1]* temp[1:neighbourhood,2])  / sum(temp[1:neighbourhood,1])
  }
  
  # return prediction and true rating if it exists
  if ( is.na(trueRatings[usr,mov])){
    if(is.nan(prediction)   ){ 
      return(list('prediction' = mean(temp[,2]), 'trueRating' = NA))
    }
    return(list('prediction' = prediction, 'trueRating' = NA))
  }
  else{
    if(is.nan(prediction)   ){ 
      return(list('prediction' = mean(temp[,2]), 'trueRating' = NA))
    }
    return(list('prediction' = prediction, 'trueRating' = as.numeric(trueRatings[usr,mov])))
  }
}

#test ub function
predict_UB(usr = 11, mov = 8429 , neighbourhood = 8 , trueRatings = trueRatings, centeredRatings = centeredRatings)


############################ITEMBASED########################################

#subset = 1:100
# tranpose to make it item based
viewedMoviesMatrix_IB = t(viewedMoviesMatrix[,-1])

centeredRatings_IB =viewedMoviesMatrix_IB[, subset] - rowMeans(viewedMoviesMatrix_IB[,subset], na.rm=T)
trueRatings_IB = viewedMoviesMatrix_IB[,subset]
centeredRatings_IB[is.na(centeredRatings_IB)] = 0


predict_IB = function(usr, mov, neighbourhood, trueRatings, centeredRatings){
  # collect the users ratings
  
  
  # returns a similarity vector 
  similarity = function(x){
    return(cosine_sim(as.numeric(temp_masked[mov,]), as.numeric(x))  )
  }
  
  ratings_list = trueRatings[,usr]
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
    if(is.nan(prediction)   ){ 
      return(list('prediction' = mean(temp[,2]), 'trueRating' = NA))
    }
    return(list('prediction' = prediction, 'trueRating' = NA))
  }
  else{
    if(is.nan(prediction)   ){ 
      return(list('prediction' = mean(temp[,2]), 'trueRating' = NA))
    }
    return(list('prediction' = prediction, 'trueRating' = as.numeric(trueRatings[mov,usr])))
  }
}


#################################
######Matrix Decomp##############
#RUN THIS FOR 100 users#
init = list(
  H0 = matrix(1, nrow = 1, ncol = ncol(trueRatings)),
  W0 = matrix(1, nrow = nrow(trueRatings), ncol = 1)
)

# matrix factorization with bias in
system.time(
decomp <- nnmf(A = trueRatings,
               method = "scd",
               k = 25,
               alpha = c(0.001,0,0),
               beta = c(0.001,0,0),
               init = init, # bias terms
               max.iter = 500)
)
# results
#prediction matrix
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


#number of user permutations
no_pred <- 5
#Prediction matrices and mses for different numbers of users
preds1 = vector("list", 5)
rmses1 <- vector("logical", 5)
#Number of users for testing
steps <- c(100, 500, 1000, 1500, 2000)
for(i in 1:no_pred)
{
  rat_mat1 <- as.matrix(viewedMoviesMatrix[1:steps[i], -1])
  
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
  rat_pred <- decomp$W %*% decomp$H
  mse = tail(decomp$mse, n = 1)
  preds1[[i]] = rat_pred
  rmses1[i] = sqrt(mse)
  
}





######################################################
################################
#Prediction Errors

#RMSEs for each prediction method
rmse_UB <- vector("logical", 5)
rmse_IB <- vector("logical", 5)
rmse_ens <- vector("logical", 5)

set.seed(1)

no_predictions = 10
no_pred <- 5
steps <- c(100, 500, 1000, 1500, 2000)
for(j in 1:no_pred)
{
  no_users = steps[j]
  subset = 1:no_users
  #For User Based
  centeredRatings =viewedMoviesMatrix[subset,-1] - rowMeans(viewedMoviesMatrix[subset,-1], na.rm=T)
  trueRatings = viewedMoviesMatrix[subset,-1]
  centeredRatings[is.na(centeredRatings)] = 0
  #indexes of rated movies
  rated <- which(!is.na(trueRatings), arr.ind = TRUE)
  #sum square Errors at each number of users
  sq_error_UB <- 0
  sq_error_IB <- 0
  sq_error_ens <- 0
  #Item Based Predictions Matrix
  viewedMoviesMatrix_IB = t(viewedMoviesMatrix[,-1])
  
  centeredRatings_IB =viewedMoviesMatrix_IB[, subset] - rowMeans(viewedMoviesMatrix_IB[,subset], na.rm=T)
  trueRatings_IB = viewedMoviesMatrix_IB[,subset]
  centeredRatings_IB[is.na(centeredRatings_IB)] = 0
  
  #at each number of users do no_pred predictions to calculate errors
  for(i in 1:no_predictions)
  {
    #Sample indexes of ratings for testing
    samp_rat = sample(1:nrow(rated), 1)
    user = as.numeric(rated[samp_rat,][1])
    movie = as.numeric(rated[samp_rat,][2])
    #user based predictions
    ub_preds <- predict_UB(usr = user, mov = movie , neighbourhood = steps[j] , trueRatings = trueRatings, centeredRatings = centeredRatings)
    ub_prediction <- ub_preds$prediction
    ub_actual <- viewedMoviesMatrix[user, movie + 1]
    #item based predictions
    ib_preds <- predict_IB(usr = user, mov = movie , neighbourhood = steps[j] , trueRatings = trueRatings_IB, centeredRatings = centeredRatings_IB)
    ib_prediction <- ib_preds$prediction
    #dealing with nans if returned
    if(is.nan(ib_prediction) || is.nan(ub_prediction))
    {
     print( paste("rejected",i,j))
      no_users = no_users - 1
      next
    }
    #calculation of squared erros at number of users
    sq_error_UB <-  sq_error_UB + (ub_prediction - ub_actual)^2
    sq_error_IB <-  sq_error_IB + (ib_prediction - ub_actual)^2
    #matrix decomp predictions
    MD_prediction <- preds1[[j]] [user, movie]
    
    #ensemble prediction
    ens_prediction <- (ub_prediction  + ib_prediction + MD_prediction) /2
    #Sqaure errors of ensemble
    sq_error_ens <-  sq_error_ens + (ens_prediction - ub_actual)^2
    
    
  }
  #Computing RMSEs
  rmse_UB[j] <- sqrt(sq_error_UB/no_users)
  rmse_IB[j] <- sqrt(sq_error_IB/no_users)
  rmse_ens[j] <- sqrt(sq_error_ens / no_users)
  print(paste("Calculated RMSEs", rmse_ens[j]) )
  
}


#plotting RMSEs
plot(steps, rmses1, xlab = "Number of users", ylab = "RMSE", main = "RMSE by Users", type = "l")

plot(steps, rmse_UB , xlab = "Number of users", ylab = "RMSE", main = "RMSE by Users", type = "l")


plot(steps, rmse_IB, xlab = "Number of users", ylab = "RMSE", main = "RMSE by Users", type = "l")

plot(steps, rmse_ens, xlab = "Number of users", ylab = "RMSE", main = "RMSE by Users", type = "l")


