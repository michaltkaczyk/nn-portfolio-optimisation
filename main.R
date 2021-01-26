library(tidyverse)

source("utils.R")

HISTORICAL_DATA_PATH <- "./historical-fund-data"

funds <- list_nn_funds(HISTORICAL_DATA_PATH)

min_date <- min(c(funds[[1]]$date, funds[[2]]$date))
max_date <- max(c(funds[[1]]$date, funds[[2]]$date))

funds_data <-
    tibble(date = seq(min_date, max_date, "days")) %>% 
    left_join(funds[[1]]) %>% 
    left_join(funds[[2]]) %>%
    gather("fund", "value", -date)

ggplot(funds_data, aes(x = date, y = value, color = fund)) + geom_line()
