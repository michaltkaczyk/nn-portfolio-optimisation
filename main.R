library(tidyverse)
library(lubridate)

source("utils.R")

HISTORICAL_DATA_PATH <- "./historical-fund-data"

START_DAY <- ymd("2005-01-01")
END_DAY <-  ymd("2020-12-31")

funds_data <- HISTORICAL_DATA_PATH %>% 
    list_nn_funds() %>% 
    bind_rows()

funds_old_enough <- funds_data %>%
    group_by(fund) %>%
    summarise(start_day = min(date)) %>% 
    filter(start_day <= START_DAY) %>% 
    pull(fund)

funds_data <- funds_data %>%
    filter(fund %in% funds_old_enough & date >= START_DAY & date <= END_DAY)

ggplot(funds_data, aes(x = date, y = value, color = fund)) +
    geom_line() +
    ggtitle("NN Investment Funds") +
    xlab("Time") +
    ylab("Index")
