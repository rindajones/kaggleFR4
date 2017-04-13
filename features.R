source("init.R")

db <- SQLite()
con <- dbConnect(drv = db, fb.db)

### Functions ###


#### Start ###
query <- "SELECT bidder_id, outcome FROM train"
tain  <- dbGetQuery(con, statement = query)
#train <- dplyr::left_join(train, res, by="bidder_id")
query <- "SELECT bidder_id FROM test"
test  <- dbGetQuery(con, statement = query)

dbDisconnect(con)
