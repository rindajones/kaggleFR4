set.seed(1)

load(ffile)

train02 <- trainlog[, -1] # Omit bidder_id column.
train02$outcome <- factor(train02$outcome, labels = c('legit', 'bot'))
train02$merchandise <- NULL

tc <- trainControl(method = "repeatedcv", 
                   number = 10,
                   repeats = 1,
                   summaryFunction = twoClassSummary, 
                   classProb = TRUE,                   
                   verbose = TRUE)

# Omit columngs for variable correaltions. 
train02 <- train02[, -(findCorrelation(cor(train02[, -1])) + 1)]
test02 <- testlog[, c("bidder_id", names(train02)[-1])]

fit01 <- train(outcome ~ ., 
                train02, 
                method = "rf", 
                metric = "ROC",
                trControl = tc, 
                verbose = FALSE,
                ntree = 500,
                tuneGrid = expand.grid(mtry = 1:4)
                )
fit02 <- train(outcome ~ ., 
                train02, 
                method = "LogitBoost", 
                metric = "ROC",
                trControl = tc)
fit03 <- train(outcome ~ ., 
                train02, 
                method = "gbm", 
                metric = "ROC",
                trControl = tc, 
                verbose = FALSE,
                tuneGrid = expand.grid(shrinkage = c(0.1, 0.01, 0.2, 0.3),
                                       n.trees = c(50, 100, 200, 500),
                                       interaction.depth = 1:5,
                                       n.minobsinnode = 20))
fit04 <- train(outcome ~ ., 
                train02, 
                method = "fda", 
                metric = "ROC",
                trControl = tc,
                tuneGrid = expand.grid(nprune = 2:30, 
                                       degree = 1:2)
                )

check <- function (fits) {
  for (fit in fits) {
    t <- tolerance(fit$results, metric = "ROC", 
                 tol = 2, maximize = TRUE)  
    print(fit$results[t,]$ROC)
  }
}
  
check(c(fit01, fit02, fit03, fit04))
## [1] 0.8985989
## [1] 0.8413388
## [1] 0.8948515
## [1] 0.8624087
goodtrain <- fit03
predTest <- predict(goodtrain, test02, type = "prob")[, 2]
result <- data.frame(bidder_id = test01$bidder_id, prediction = predTest)
