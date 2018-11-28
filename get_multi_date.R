
## function to get valid hours
valid_hour2 <- function(data_set){
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

shaper_trend_data2 <- function(data_set){
  data_set$datetime <- as.POSIXct(data_set$datetime)
  tmp1 <- data_set %>%
    select(-starts_with("segment")) %>%
    mutate(
      datetime = as.POSIXct(datetime),
      reporting_year = unique(year(datetime))[1]
    ) %>%
    melt(
      id.vars = c("datetime","hour","evar44","lasttouchchannel","reporting_year"),
      variable.name = "metric",
      variable.value = "value"
    )
  return(tmp1)
}

## function to combine this year and last years data
shape_kpi_trend2 <- function(last_year_data,this_year_data){
  ## shape the data
  tmp1 <- shaper_trend_data2(last_year_data)
  tmp2 <- shaper_trend_data2(this_year_data)
  ## get current hour
  hours_to_filter <- valid_hour2(tmp2)
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


df2_today$datetime <- as.POSIXct(df2_today$datetime)


tmp <- shape_kpi_trend2(df2_lastyear,df2_today)


df2_today <- df2_today %>%
  select(-starts_with("segment"))


write_csv(df2_today,"channel_source.csv")
