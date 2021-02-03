library(timetk)

funds_data_xts <- funds_data %>%
    select(-value) %>% 
    pivot_wider(names_from = fund, values_from = value_pct_change) %>% 
    tk_xts()

manual_portfolios <- rbind(
    c(1, 0, 0, 0, 0),
    c(1, 1, 0, 0, 0),
    c(1, 0, 1, 0, 0),
    c(1, 0, 0, 1, 0),
    c(1, 0, 0, 0, 1),
    c(1, 1, 1, 1, 1)) %>% 
    apply(1, function(x) x / sum(x)) %>% 
    t()

manual_portfolios_results <- list()

for (portfolio in 1:NROW(manual_portfolios)) {
    manual_portfolios_results[[portfolio]] <- 
        Return.portfolio(funds_data_xts, manual_portfolios[portfolio, ]) %>%
        as_tibble() %>% 
        add_column(date = index(funds_data_xts)) %>% 
        mutate(portfolio = paste("portfolio", portfolio)) %>% 
        rename(value = portfolio.returns)
}

portfolio_comparison <- manual_portfolios_results %>%
    bind_rows() %>% 
    group_by(portfolio) %>% 
    mutate(value = cumprod(value + 1) * 100)

ggplot(portfolio_comparison, aes(x = date, y = value, color = portfolio)) +
    geom_line() +
    labs(
        title = paste("Performance of Manually Selected Portfolios Between", START_DAY, "and", END_DAY),
        caption = "Source: NN Investment Partners, michaltkaczyk's estimations") +
    xlab("Time") +
    ylab("Index")
