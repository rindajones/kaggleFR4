set.seed(1)

load(ffile)

train <- trainlog[, -1] # Omit bidder_id column.
train$outcome <- factor(train$outcome, labels = c('legit', 'bot'))

# Consider this: should be remove the "suspicious" case?
#train <- train[-which(train$outcome == 0 & train$bid_count > 13.15), ]

train$merchandise <- NULL
# Try to mess with merchandise
# train$merchandise <- ifelse(train$merchandise == "computers", "computers", "not computers")
# test$merchandise <- ifelse(test$merchandise == "computers", "computers", "not computers")
# train$merchandise <- factor(train$merchandise)
# test$merchandise <- factor(test$merchandise)

tc <- trainControl(method = "repeatedcv", 
                   number = 10,
                   repeats = 1,
                   summaryFunction = twoClassSummary, 
                   classProb = TRUE,                   
                   verbose = TRUE)

# form1 <- outcome ~ crimed_max_p + bid_count + crimed_median_p +
#                    max_bids + device_count + tmedian + tfast
#train <- train[, c(1:15)]

train <- train[, -(findCorrelation(cor(train[, -1])) + 1)]
test <- testlog[, c("bidder_id", names(train)[-1])]

#varSelRF output
#form1 <- outcome ~ bid_count + crimed_max_p + device_count + max_bids

train1 <- train(outcome ~ ., 
                train, 
                method = "rf", 
                metric = "ROC",
                trControl = tc, 
                verbose = FALSE,
                ntree = 500,
                tuneGrid = expand.grid(mtry = 1:4)
                )
# 
train2 <- train(outcome ~ ., 
                train, 
                method = "LogitBoost", 
                metric = "ROC",
                trControl = tc)


train3 <- train(outcome ~ ., 
                train, 
                method = "gbm", 
                metric = "ROC",
                trControl = tc, 
                verbose = FALSE,
# rinda
#                tuneGrid = expand.grid(shrinkage = c(0.1, 0.01, 0.2, 0.3),
#                                       n.trees = c(50, 100, 200, 500),
#                                       interaction.depth = 1:5))
                tuneGrid = expand.grid(shrinkage = c(0.1, 0.01, 0.2, 0.3),
                                       n.trees = c(50, 100, 200, 500),
                                       interaction.depth = 1:5,
                                       n.minobsinnode = 20))

train4 <- train(outcome ~ ., 
                train, 
                method = "fda", 
                metric = "ROC",
                trControl = tc,
                tuneGrid = expand.grid(nprune = 2:30, 
                                       degree = 1:2)
                )


# predTest <- apply(cbind(predict(train1, test, type = "prob")[, 2],
#                      predict(train2, test, type = "prob")[, 2],
#                         predict(train3, test, type = "prob")[, 2]),
#                   1,
#                   mean)

goodtrain <- train3

predTest <- predict(goodtrain, test, type = "prob")[, 2]

result <- data.frame(bidder_id = test$bidder_id, prediction = predTest)
# Add the missing ones.
missingTest <- getMissingBidders("test")
result <- rbind(result, data.frame(bidder_id = missingTest, prediction = 0))
# Save result
resultfile <- sprintf("~/Downloads/kaggle/facebook_IV_Human_or_Robot/tmp/fr4_result_%s.csv.gz", 
                      gsub(":", "", gsub(" ", "_", Sys.time())))
gzresultfile <- gzfile(resultfile)
write.csv(result, file = gzresultfile, quote = FALSE, row.names = FALSE)
cat("Result saved to", resultfile, "\n")