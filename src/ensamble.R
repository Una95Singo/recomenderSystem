# Ensamble function that will call the individual RS systems and produce outputs.
# Una Singo 24 July 2019

library(tidyverse)
source('src/itemBased.R')
source('src/userBased.R')
source('src/matrixDecomp.R')

load('data/Processed/ratings.RData')
