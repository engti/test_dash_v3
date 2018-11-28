## get data from sitecatalyst
## load it into postgres

## library
  library(RSiteCatalyst)
  library(DBI)  
  library(RPostgres)  
  library(dplyr)
  library(lubridate)
  library(readr)
  library(reshape2)
  library(scales)
  library(stringi)
  
## source the auth
  source("auth.R")
  
  meta_info <- list(
    ecom = GetEcommerce(rsid),
    event = GetSuccessEvents(rsid),
    elements = GetElements(rsid),
    mc_rules = GetMarketingChannelRules(rsid),
    classifications = GetClassifications(rsid),
    segments = GetSegments(rsid)
  )
  
  ## trended channel data ---- 
  df1_today <- QueueTrended(
    reportsuite.id = rsid,
    date.from = Sys.Date(),
    date.to = Sys.Date(),
    metrics = c("visits","orders","revenue"),
    elements = "evar44",
    date.granularity = "hour",
    selected = c("tablet","desktop","ios","android","mobile"),
    top = 1000
  )
  
  df1_lastyear <- QueueTrended(
    reportsuite.id = rsid,
    date.from = Sys.Date() - 365,
    date.to = Sys.Date() - 365,
    metrics = c("visits","orders","revenue"),
    elements = "evar44",
    date.granularity = "hour",
    selected = c("tablet","desktop","ios","android","mobile"),
    top = 1000
  )
  
  ## function to get valid hours
  valid_hour <- function(data_set){
    data_set %>%
      filter(reporting_year == 2018) %>%
      select(hour,value) %>%
      group_by(hour) %>%
      summarise(
        metrics = sum(value)
              ) %>%
      filter(metrics > 0) %>%
      select(-metrics) %>%
      pull(hour)
  }
  
  shaper_trend_data <- function(data_set){
    data_set$datetime <- as.POSIXct(data_set$datetime)
    tmp1 <- data_set %>%
      select(-starts_with("segment"),-url) %>%
      mutate(
        datetime = as.POSIXct(datetime),
        reporting_year = unique(year(datetime))[1]
      ) %>%
      melt(
        id.vars = c("datetime","hour","name","reporting_year"),
        variable.name = "metric",
        variable.value = "value"
      )
    return(tmp1)
  }

  ## function to combine this year and last years data
  shape_kpi_trend <- function(last_year_data,this_year_data){
    ## shape the data
    tmp1 <- shaper_trend_data(last_year_data)
    tmp2 <- shaper_trend_data(this_year_data)
    ## get current hour
    hours_to_filter <- valid_hour(tmp2)
    ## filter todays dataset to only include valid hours 
    tmp3 <- tmp2 %>%
        filter(hour %in% hours_to_filter)
    
    ## merge ly and today data
    tmp4 <- rbind(tmp1,tmp3)
    ## flag status of data
    tmp5 <- tmp4 %>%
      mutate(
        data_status = case_when(
          hour %in% hours_to_filter[1:length(hours_to_filter)-1] ~ "current",
          !hour %in% hours_to_filter ~ "pending",
          hour == max(hours_to_filter) ~ "live"
        )
      )
    
    ## return data
    return(tmp5)
  }
  
  
  df_hour_trend <- shape_kpi_trend(df1_lastyear,df1_today)
  
  ## write the file
   write_csv(df_hour_trend,"hourlydata.csv")
  
  
  ## tablular channel change data from trended dataset ----
  channel_filter_items <- c("ios","android","desktop","mobile","tablet")
  report_filter_period <- "current"
  metric_filter <- "visits" 
  tmp2 <- tmp %>% 
    select(reporting_year,channel = name,data_status,metric,value) %>%
    filter(channel %in% channel_filter_items,
           data_status %in% report_filter_period,
           metric %in% metric_filter) %>%
    select(-data_status) %>%
    group_by(reporting_year,channel,metric) %>%
    summarise(
      value = sum(value)
    ) %>%
    ungroup() %>%
    mutate(
      metric = paste0(reporting_year,"_",metric)
    ) %>%
    dcast(channel ~ metric) %>%
    select(c(1,3,2)) %>%
    mutate(
      change = round((.[[2]] - .[[3]]) / .[[3]],digits = 2)
    )
  
  
  ## aggregate for all channels ----
  write_csv(tmp2,"aggregate_kpi.csv")
  
  
  ## aggregate kpi for top trends
  df_kpi <- df_hour_trend %>%
    select(reporting_year,data_status,metric,value) %>%
    filter(data_status == "current") %>%
    select(-data_status) %>%
    group_by(reporting_year,metric) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    dcast(metric ~ reporting_year,value.var = "value") %>%
    mutate(
      change = (`2018` - `2017`) / `2017`,
      text = case_when(
        change > 0 ~ paste0(percent(change)," from same period last year"),
        change < 0 ~ paste0(percent(change)," from same period last year"),
        change == 0 ~ paste0("unchanged from same period last year")
      )
     )
  
  ## trended channel and marketing channel data ---- 
  ## trended channel data ---- 
  df2_today <- QueueTrended(
    reportsuite.id = rsid,
    date.from = Sys.Date(),
    date.to = Sys.Date(),
    metrics = c("visits","orders","revenue"),
    elements = c("evar44","lasttouchchannel"),
    date.granularity = "hour",
    selected = c("tablet","desktop","ios","android","mobile"),
    top = 1000
  )
  
  df2_lastyear <- QueueTrended(
    reportsuite.id = rsid,
    date.from = Sys.Date() - 365,
    date.to = Sys.Date() - 365,
    metrics = c("visits","orders","revenue"),
    elements = c("evar44","lasttouchchannel"),
    date.granularity = "hour",
    selected = c("tablet","desktop","ios","android","mobile"),
    top = 1000
  )
  
  
  df2_hour_trend <- rbind(df2_lastyear,df2_today) %>%
    select(-starts_with("segment")) 

  df2_hour_trend$datetime <- as.POSIXct(df2_hour_trend$datetime)
  
  df2_hour_trend <- df2_hour_trend %>%
    mutate(reporting_year = unique(year(datetime))[1])
  
  tmp <- valid_hour(df2_hour_trend)
