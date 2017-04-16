

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
    
    # Iniite check: Replaced by the value which is the maximum x 2.
    ret[is.infinite(ret$min_resp), "min_resp"] <- 2*max(ret[!is.infinite(ret$min_resp),"min_resp"],
                                                    na.rm=TRUE)
    

    # instant_resp: the number of responses done on same time
    query <- "SELECT bidder_id, count(time) AS instant_resp FROM bids GROUP BY bidder_id"
    res2  <- dbGetQuery(con, query)
    ret <- dplyr::left_join(ret, res2, by="bidder_id")

    return(ret)
}
