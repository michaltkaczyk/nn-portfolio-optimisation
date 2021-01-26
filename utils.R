read_nn_csv <- function(file) {
    read_csv(file) %>% 
        rename(date = Data) %>% 
        select(-Waluta)
}
