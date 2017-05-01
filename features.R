source("init.R")
source("functions.R")

### Global Variables ###
db <- SQLite()
con <- dbConnect(drv = db, fb.db)

# The Numbers of records
# bids:7656335, train:2014, test:4701

#### Start ###
# Initial data.frames...

# No good since column names are returned.
#   query <- "SELECT bidder_id, outcome FROM train"
query <- "SELECT bidder_id FROM train"
train <- data.frame(bidder_id = dbGetQuery(con, query))
train <- train %>% filter(bidder_id!="bidder_id")
query <- "SELECT bidder_id, outcome FROM train"
outcome  <- dbGetQuery(con, query)
train <- dplyr::left_join(train, outcome, by="bidder_id")
train$outcome <- as.factor(train$outcome)
query <- "SELECT bidder_id FROM test"
test  <- data.frame(bidder_id = dbGetQuery(con, query))
test  <- test %>% filter(bidder_id!="bidder_id")
# Done.

# The number of bids per bidder.
# The query below replaced to bid_count to bids
query <- paste0("SELECT bidder_id, COUNT(bidder_id) as bids ",
                "FROM bids GROUP BY bidder_id")
bids <- dbGetQuery(con, query)
bids <- bids %>% filter(bidder_id!="bidder_id")
dim(bids)
## [1] 6614    2
anti_train<- dplyr::anti_join(train, bids, by="bidder_id")
anti_test <- dplyr::anti_join(test, bids, by="bidder_id")
dim(anti_train)
## [1] 29  2
dim(anti_test)
## [1] 70  1

#bids$bids <- log1p(bids$bids)
#train <- dplyr::left_join(train, bids, by="bidder_id")
#test  <- dplyr::left_join(test,  bids, by="bidder_id")
train <- dplyr::inner_join(train, bids, by="bidder_id")
test  <- dplyr::inner_join(test,  bids, by="bidder_id")
# Done.

# Time difference statistics per user... ")
resp <- getResponseDiff(con)
train <- dplyr::inner_join(train, resp, by="bidder_id")
test  <- dplyr::inner_join(test,  resp, by="bidder_id")

totals <- getTotals(con)
train <- dplyr::inner_join(train, totals, by="bidder_id")
test  <- dplyr::inner_join(test,  totals, by="bidder_id")

# merchandise
query <- paste0("SELECT bidder_id, merchandise",
                " FROM bids GROUP by bidder_id")
merch <- dbGetQuery(con, query)
merch[,2] <- as.factor(merch[,2])
train <- dplyr::inner_join(train, merch, by="bidder_id")
test  <- dplyr::inner_join(test,  merch, by="bidder_id")

bidsRates <- getBidsRates(con)
train <- dplyr::inner_join(train, bidsRates, by="bidder_id")
test  <- dplyr::inner_join(test,  bidsRates, by="bidder_id")

dbDisconnect(con)

str(train)
trainlog <- train
trainlog[,c(-1,-2,-16)] <-  log1p(trainlog[,c(-1,-2,-16)])
testlog <- test
testlog[,c(-1,-15)] <-  log1p(testlog[,c(-1,-15)])

save(train, test, file = ffile)


