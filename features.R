source("init.R")

### Global Variables ###
db <- SQLite()
con <- dbConnect(drv = db, fb.db)

### Functions ###


#### Start ###
# Initial data.frames...

# No good since column names are returned.
# query <- "SELECT bidder_id, outcome FROM train"
query <- "SELECT bidder_id FROM train"
train  <- data.frame(bidder_id = dbGetQuery(con, query))
query <- "SELECT bidder_id, outcome FROM train"
outcome  <- dbGetQuery(con, query)
train <- dplyr::left_join(train, outcome, by="bidder_id")
query <- "SELECT bidder_id FROM test"
test  <- data.frame(bidder_id = dbGetQuery(con, query))
# Done.

# The number of bids per bidder.
# The query below replaced to bid_count to bids
query <- paste0("SELECT bidder_id, COUNT(bidder_id) as bids ",
                "FROM bids GROUP BY bidder_id")
bids <- dbGetQuery(con, query)
train <- dplyr::left_join(train, bids, by="bidder_id")
test  <- dplyr::left_join(test, bids, by="bidder_id")
# Done.


dbDisconnect(con)
