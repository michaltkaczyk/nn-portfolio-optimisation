read_nn_csv <- function(file) {
    file %>% 
        read_csv() %>% 
        rename(date = Data) %>% 
        select(-Waluta)
}

list_nn_funds <- function(path) {
    path %>% 
        list.files(full.names = TRUE) %>% 
        lapply(read_nn_csv)
}
