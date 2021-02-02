library(timetk)

funds_data_xts <- funds_data %>%
    select(-value) %>% 
    pivot_wider(names_from = fund, values_from = value_pct_change) %>% 
    tk_xts()

portfolio_1_returns <- Return.portfolio(funds_data_xts, c(1/3, 1/3, 1/3))
portfolio_2_returns <- Return.portfolio(funds_data_xts, c(1/4, 1/4, 2/4))
portfolio_3_returns <- Return.portfolio(funds_data_xts, c(3/6, 2/6, 1/6))

portfolio_comparison <- data.frame(
    date = index(portfolio_1_returns),
    p1 = as.numeric(portfolio_1_returns),
    p2 = as.numeric(portfolio_2_returns),
    p3 = as.numeric(portfolio_3_returns)) %>%  
    pivot_longer(!date, "portfolio") %>% 
    group_by(portfolio) %>% 
    mutate(value = cumprod(value + 1) * 100)

ggplot(portfolio_comparison, aes(x = date, y = value, color = portfolio)) +
    geom_line() +
    labs(
        title = paste("Performance of Manually Selected Portfolios Between", START_DAY, "and", END_DAY),
        caption = "Source: NN Investment Partners, michaltkaczyk's estimations") +
    xlab("Time") +
    ylab("Index")
