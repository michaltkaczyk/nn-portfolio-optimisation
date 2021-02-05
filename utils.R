read_nn_csv <- function(file) {
    file %>% 
        read_csv() %>% 
        rename(date = Data) %>% 
        select(-Waluta) %>% 
        pivot_longer(!date, "fund")
}

list_nn_funds <- function(path) {
    path %>% 
        list.files(full.names = TRUE) %>% 
        lapply(read_nn_csv)
}

generate_random_portfolio_weights <- function(n_weights) {
    random_uniform <- runif(n_weights)
    random_exponential <- -log(random_uniform)
    random_exponential / sum(random_exponential)
}
