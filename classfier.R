set.seed(123)

predictByGlm <- function(formula, train, test) {
  train$outcome <- factor(train$outcome)
  lm <- glm(formula, data = train, family = binomial())
  res <- predict(lm, test, type = "response")
  return(res)
}

train01 <- trainlog
test01 <- testlog

formula01 <- as.formula(paste0("outcome ~  resp_sd + resp_mean + resp_median + resp_min"
                  , " + bids_per_country_sd + bids"))
formula02 <- as.formula(paste0("outcome ~  resp_sd + resp_mean + resp_median + resp_min"
                  , " + bids_per_country_sd + bids + ip_sum"))
formula03 <- as.formula(paste0("outcome ~  resp_sd + resp_mean + resp_median + resp_min"
                  , " + bids_per_country_sd + bids + ip_sum + bids_per_auction_sd"))

# All the columns without correlations.
#
# Omit columns(bidder_id, outcome, merchandise), then delete some columns to avoid correlation.
t <- train01[,-(findCorrelation(cor(train01[, c(-1,-2,-16)]))+1)]
t <- t[,-1] # delete bidder_id
f <- "outcome ~ "
flg <- TRUE
for (n in names(t)) {
  if (flg) {
    f <- paste0(f, n)
    flg <- FALSE
  } else {
    f <- paste0(f, "+", n)
  }
}
f
## [1]"outome ~ bids+resp_mean+resp_median+resp_min+resp_fast_rate+resp_instant+resp_instant_rate+device_sum+ip_sum+merchandise+bids_per_auction_mean+bids_per_auction_median+bids_per_auction_sd+bids_per_device_mean+bids_per_device_median+bids_per_device_sd+bids_per_country_mean+bids_per_country_median+bids_per_ip_mean+bids_per_ip_median+bids_per_ip_sd"
formula04 <- as.formula(f)

# CV AUC: 0.85823
#formula <- formula01
# CV AUC: 0.86474
#formula <- formula02
# CV AUC: 0.86744
#formula <- formula03
# CV AUC: 0.88906
formula <- formula04

# Try a 10-fold cross-validation
aucs <- rep(0, nrow(train01))
folds <- createFolds(factor(train01$outcome), 10)
for (f in folds) {
    aucs[f] <- predictByGlm(formula, train01[-f, ], train01[f, ])
}
auc <- roc(train01$outcome, aucs)$auc
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