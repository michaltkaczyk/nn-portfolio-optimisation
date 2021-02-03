read_nn_csv <- function(file) {
    file %>% 
        read_csv() %>% 
        rename(date = Data) %>% 
        select(-Waluta) %>% 
        pivot_longer(!date, "fund") %>% 
        arrange(date) %>% 
        mutate(value_pct_change = value/lag(value) - 1)
}

list_nn_funds <- function(path) {
    path %>% 
        list.files(full.names = TRUE) %>% 
        lapply(read_nn_csv)
}
