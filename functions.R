# mean_resp, sd_resp, meian_resp, min_resp, fast_resp
#
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
    ret <- res %>% group_by(bidder_id) %>%
         summarize(mean_resp=mean(diff(sort(time))), sd_resp=sd(diff(sort(time))),
                   median_resp=median(diff(sort(time))), min_resp=min(diff(sort(time))),
                   fast_resp=fast(diff(sort(time))))
    
    # NA check: Replaced by the value which is the maximum x 2.
    #  最大値の二倍値で欠損値を補完
    ret[is.na(ret$mean_resp),  "mean_resp"]   <- 2*max(ret$mean_resp,na.rm=TRUE)
    ret[is.na(ret$sd_resp),    "sd_resp"]     <- 2*max(ret$sd_resp,na.rm=TRUE)
    ret[is.na(ret$median_resp),"median_resp"] <- 2*max(ret$median_resp,na.rm=TRUE)
    
    # Infinite check: Replaced by the value which is the maximum x 2.
    ret[is.infinite(ret$min_resp), "min_resp"] <- 2*max(ret[!is.infinite(ret$min_resp),"min_resp"],
                                                    na.rm=TRUE)
    
    # instant_resp: the number of responses done on same time
    # Get the difference of NON_DISTINCT and DISTINCT of time.
    query <- "SELECT bidder_id, COUNT(time) AS time FROM bids GROUP BY bidder_id"
    ins <- dbGetQuery(con, query)
    query <- "SELECT bidder_id, COUNT(DISTINCT time) AS time FROM bids GROUP BY bidder_id"
    ins2 <- dbGetQuery(con, query)
    ins$instant_resp <- ins$time - ins2$time
    ins$Per_instant_resp <- ins$instant_resp/ins$time
    ins <- ins[,-2] # delete time column
    ret <- dplyr::left_join(ret, ins, by="bidder_id")
    ret[,2:7] <- log1p(ret[,2:7])
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
                  "COUNT(DISTINCT auction) AS no_auctions,",    
                  "COUNT(DISTINCT device)  AS no_devices,",    
                  "COUNT(DISTINCT country) AS no_countries,",
                  "COUNT(DISTINCT ip)      AS no_ips",    
                  " FROM bids GROUP BY bidder_id")
  res <- dbGetQuery(con, query)
  res[,2:5] <- log1p(res[,2:5])
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
                         bids_per_auction_median=median(no))

  query <- paste0("SELECT bidder_id, device, COUNT(device) AS no",
                   " FROM bids GROUP BY bidder_id, device")
  res <- dbGetQuery(con, query)
  device <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_device_mean=mean(no),
                         bids_per_device_median=median(no))
  ret <- dplyr::left_join(auction, device, by="bidder_id")
  
  query <- paste0("SELECT bidder_id, country, COUNT(country) AS no",
                   " FROM bids GROUP BY bidder_id, country")
  res <- dbGetQuery(con, query)
  country <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_country_mean=mean(no),
                         bids_per_country_median=median(no))
  ret <- dplyr::left_join(ret, country, by="bidder_id")

  query <- paste0("SELECT bidder_id, ip, COUNT(ip) AS no",
                   " FROM bids GROUP BY bidder_id, ip")
  res <- dbGetQuery(con, query)
  ip <- res %>% group_by(bidder_id) %>%
        dplyr::summarise(bids_per_ip_mean=mean(no),
                         bids_per_ip_median=median(no))
  ret <- dplyr::left_join(ret, ip, by="bidder_id")

  ret[,2:9] <- log1p(ret[,2:9])
  return(ret)
}



