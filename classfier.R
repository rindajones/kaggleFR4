set.seed(123)

predictByGlm <- function(formula, train, test) {
  train$outcome <- factor(train$outcome)
  lm <- glm(formula, data = train, family = binomial())
  res <- predict(lm, test, type = "response")
  return(res)
}

# AUC 0.85687
formula <- as.formula(paste0("outcome ~  resp_sd + resp_mean + resp_median + resp_min"
                  , " + bids_per_country_sd + bids + ip_sum"))
# AUC 0.86474
formula <- as.formula(paste0("outcome ~  resp_sd + resp_mean + resp_median + resp_min"
                  , " + bids_per_country_sd + bids + ip_sum"))
# AUC: 0.87187
formula <- as.formula(paste0("outcome ~  resp_sd + resp_mean + resp_median + resp_min"
                  , " + bids_per_country_sd + bids + ip_sum + bids_per_auction_sd"))

# Try a 10-fold cross-validation
aucs <- rep(0, nrow(trainlog))
folds <- createFolds(factor(trainlog$outcome), 10)
for (f in folds) {
    aucs[f] <- predictByGlm(formula, trainlog[-f, ], trainlog[f, ])    
}
auc <- roc(train$outcome, aucs)$auc
cat(sprintf("CV AUC: %.5f\n", auc))

predTest <- predictByGlm(formula, trainlog, testlog)
result <- data.frame(bidder_id = testlog$bidder_id, prediction = predTest)


# Add the missing ones.
missingTest <- getMissingBidders("test")
result <- rbind(result, data.frame(bidder_id = missingTest, prediction = 0))
# Save result
resultfile <- sprintf("~/Documents/kaggle/facebook_IV_Human_or_Robot/kaggleFR4/tmp/fr4_result_%s.csv.gz", 
                      gsub(":", "", gsub(" ", "_", Sys.time())))
gzresultfile <- gzfile(resultfile)
write.csv(result, file = gzresultfile, quote = FALSE, row.names = FALSE)
cat("Result saved to", resultfile, "\n")