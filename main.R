library(tidyverse)

source("utils.R")

HISTORICAL_DATA_PATH <- "./historical-fund-data"

funds <- HISTORICAL_DATA_PATH %>% 
    list_nn_funds() %>% 
    bind_rows()

ggplot(funds, aes(x = date, y = value, color = fund)) +
    geom_line() +
    ggtitle("NN Investment Funds") +
    xlab("Time") +
    ylab("Index")
