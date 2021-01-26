read_nn_csv <- function(file) {
    read_csv(file) %>% 
        rename(date = Data) %>% 
        select(-Waluta)
}

list_nn_funds <- function(path) {
    path %>% 
        list.files(full.names = TRUE) %>% 
        lapply(read_nn_csv)
}
