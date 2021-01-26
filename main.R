library(tidyverse)

source("utils.R")

funds <- list(
    read_nn_csv("./historical-fund-data/NN_Akcji_(A)_1998-03-11_2021-01-07.txt"),
    read_nn_csv("./historical-fund-data/NN_Obligacji_(A)_1999-02-25_2021-01-07.txt"))

min_date <- min(c(funds[[1]]$date, funds[[2]]$date))
max_date <- max(c(funds[[1]]$date, funds[[2]]$date))

funds_data <-
    tibble(date = seq(min_date, max_date, "days")) %>% 
    left_join(funds[[1]]) %>% 
    left_join(funds[[2]]) %>%
    gather("fund", "value", -date)

ggplot(funds_data, aes(x = date, y = value, color = fund)) + geom_line()
