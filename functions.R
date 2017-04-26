# resp_mean, resp_sd, resp_meian, resp_min
# resp_fast, resp_fasst_rate
getResponseDiff <- function(con) {
    query <- "SELECT bidder_id, auction, time FROM bids"
    res <- dbGetQuery(con, query)

    # delete the colum name line 
    res <- res[-1,]
    res$time <- as.numeric(res$time)
    
    fast <- function(x) {
      dv <- diff(sort(x))
      md <- mean(dv)
      if (!is.na(md)) {
        return(sum(dv < md) / length(dv))
      } else {
        return(0.0)
      }
    }
    ret <- res %>% dplyr::group_by(bidder_id) %>%
         dplyr::summarise(resp_mean=mean(diff(sort(time))), resp_sd=sd(diff(sort(time))),
                   resp_median=median(diff(sort(time))), resp_min=min(diff(sort(time))),
                   resp_fast_rate=fast(diff(sort(time))))
    
    # NA check: Replaced by the value which is the maximum x 2.
    #  最大値の二倍値で欠損値を補完
    ret[is.na(ret$resp_mean),  "resp_mean"]   <- 2*max(ret$resp_mean,na.rm=TRUE)
    ret[is.na(ret$resp_sd),    "resp_sd"]     <- 2*max(ret$resp_sd,na.rm=TRUE)
    ret[is.na(ret$resp_median),"resp_median"] <- 2*max(ret$resp_median,na.rm=TRUE)
    
    # Infinite check: Replaced by the value which is the maximum x 2.
    ret[is.infinite(ret$resp_min), "resp_min"] <- 2*max(ret[!is.infinite(ret$resp_min),"resp_min"],
                                                    na.rm=TRUE)
    
    # instant_resp: the number of responses done on same time
    # Get the difference of NON_DISTINCT and DISTINCT of time.
    query <- "SELECT bidder_id, COUNT(time) AS time FROM bids GROUP BY bidder_id"
    ins <- dbGetQuery(con, query)
    query <- "SELECT bidder_id, COUNT(DISTINCT time) AS time FROM bids GROUP BY bidder_id"
    ins2 <- dbGetQuery(con, query)
    ins$resp_instant <- ins$time - ins2$time
    ins$resp_instant_rate <- ins$resp_instant/ins$time
    ins <- ins[,-2] # delete time column
    ret <- dplyr::left_join(ret, ins, by="bidder_id")
    #ret[,2:7] <- log1p(ret[,2:7])
    return(ret)
}

# url_sum: Bids coming out from a particular Domain
# mean, median of the number GROUP BY bidder_id, auction
# UNUSED!! Since Apr 18, 2016
getUrlSum <- function(con) {
    query <- paste0("SELECT bidder_id, COUNT(DISTINCT url) as link_count, ",
                    "auction FROM bids GROUP by bidder_id, auction")
    res <- dbGetQuery(con, query)
    ret <- res %>% dplyr::group_by(bidder_id) %>%
      dplyr::summarise(mean_url_sum=mean(link_count),
                       median_url_sum=median(link_count))
    return(ret)
}


# Get the totals of each variables.
getTotals <- function(con) {
  # The early virsion: 
  #  query <- "SELECT bidder_id, COUNT(DISTINCT url) AS url_count, url FROM bids GROUP BY bidder_id, url"
  #  res <- dbGetQuery(con, query)
  #  urlSum <- res %>% dplyr::group_by(bidder_id) %>% summarise(url_sum=n())

  query <- paste0("SELECT bidder_id,",
                  "COUNT(DISTINCT url)     AS url_sum,",
                  "COUNT(DISTINCT auction) AS auction_sum,",    
                  "COUNT(DISTINCT device)  AS device_sum,",    
                  "COUNT(DISTINCT country) AS country_sum,",
                  "COUNT(DISTINCT ip)      AS ip_sum",    
                  " FROM bids GROUP BY bidder_id")
  res <- dbGetQuery(con, query)
  #res[,2:5] <- log1p(res[,2:5])
  return(res)
}

# bids_per_*_mean, bids_per_*_median
# auction, device, country, ip
getBidsRates <- function(con) {
  query <- paste0("SELECT bidder_id, auction, COUNT(auction) AS no",
                   " FROM bids GROUP BY bidder_id, auction")
  res <- dbGetQuery(con, query)
  auction <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_auction_mean=mean(no),
                         bids_per_auction_median=median(no),
　　　　　　　　　　　　 bids_per_auction_sd=sd(no))

  query <- paste0("SELECT bidder_id, device, COUNT(device) AS no",
                   " FROM bids GROUP BY bidder_id, device")
  res <- dbGetQuery(con, query)
  device <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_device_mean=mean(no),
                         bids_per_device_median=median(no),
                         bids_per_device_sd=sd(no))  
  ret <- dplyr::left_join(auction, device, by="bidder_id")
  
  query <- paste0("SELECT bidder_id, country, COUNT(country) AS no",
                   " FROM bids GROUP BY bidder_id, country")
  res <- dbGetQuery(con, query)
  country <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_country_mean=mean(no),
                         bids_per_country_median=median(no),
                         bids_per_country_sd=sd(no))
  ret <- dplyr::left_join(ret, country, by="bidder_id")

  query <- paste0("SELECT bidder_id, ip, COUNT(ip) AS no",
                   " FROM bids GROUP BY bidder_id, ip")
  res <- dbGetQuery(con, query)
  ip <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_ip_mean=mean(no),
                         bids_per_ip_median=median(no),
                         bids_per_ip_sd=sd(no))  
  ret <- dplyr::left_join(ret, ip, by="bidder_id")
  
  # NA check: Replaced by the value which is the maximum x 2.
  # 最大値で欠損値を補完
  ret[is.na(ret$bids_per_auction_sd), "bids_per_auction_sd"]<- max(ret$bids_per_auction_sd,na.rm=TRUE)
  ret[is.na(ret$bids_per_device_sd),  "bids_per_device_sd"] <- max(ret$bids_per_device_sd,na.rm=TRUE)
  ret[is.na(ret$bids_per_country_sd), "bids_per_country_sd"]<- max(ret$bids_per_country_sd,na.rm=TRUE)
  ret[is.na(ret$bids_per_ip_sd),      "bids_per_ip_sd"]     <- max(ret$bids_per_ip_sd,na.rm=TRUE)  

  #ret[,2:13] <- log1p(ret[,2:13])
  return(ret)
}

# The differences between human's and robot's values.
getDiffOfResult <- function(train) {
  # 唯一数値型ではない merchandise を除く
　g_train <- train %>%
    tidyr::gather(variable,value,bids:bids_per_ip_sd,-merchandise)

  tsum <- g_train %>%
    dplyr::group_by(outcome, variable) %>% dplyr::summarise(sum=sum(value))

  tsum <- tsum %>% tidyr::spread(key=outcome,value=sum)

  # outcome の比率に変換する
  table(train$outcome)
  ##    0    1
  ## 1881  103
  tsum[,2] <- tsum[,2]/1881
  tsum[,3] <- tsum[,3]/103

  df <- as.data.frame(tsum)
  df$diff <- abs(df[,2]-df[,3])
  return (df)
}

