library(RSQLite)
library(caret)
library(caretEnsemble)
library(ggplot2)
library(pROC)
library(randomForest)
library(C50)
library(nnet)
library(gbm)
library(Boruta)
library(dplyr) # by rinda

fb.db <- "~/Documents/kaggle/facebook_IV_Human_or_Robot/fb.db"

ffile <- "~/Documents/kaggle/facebook_IV_Human_or_Robot/extractedFeatures.Rda"
