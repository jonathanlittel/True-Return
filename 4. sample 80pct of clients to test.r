demand %>% group_by(Year) %>% summarise(sum(average_cost, na.rm=T), min(average_cost_cum))


# demand2 <- clients %>%
#   group_by(RC.Account.Number) %>%
#   arrange(Year) %>%
#   mutate(sales_lag = lag(sales) ) %>%
#   ungroup() %>%
#   filter(active_year == TRUE, !is.na(producers), !is.na(revenue_less_risk_less_debt_per_year)) %>%
#   mutate(producers = producers_plus) %>%
#   group_by(Year) %>%
#   sample_frac(size = 0.8, replace = FALSE) %>% 
#   arrange(producers) %>%
#   # arrange(-producers) %>%
#   mutate(
#     average_cost = revenue_less_risk_less_debt_per_year - opex,
#     average_cost_per_producer = average_cost / producers,
#     sales_increase = sales - sales_lag,
#     sales_increase_per_cost = sales_increase - average_cost
#     ) %>%
#   arrange(average_cost_per_producer) %>%
#   arrange(desc(average_cost_per_producer)) %>%
#   # arrange(desc(producers)) %>%
#   mutate(
#     average_cost_cum = cumsum(average_cost),
#     producers_cumulative = cumsum(producers),
#     average_cost_per_producer_cumulative = average_cost_cum / producers_cumulative
#     ) %>%
#   arrange(desc(sales_increase_per_cost)) %>%
#   mutate(
#     average_cost_cum = cumsum(average_cost),
#     sales_increase_cum = cumsum(sales_increase),
#     average_cost_per_sales_increase_cum = average_cost_cum / sales_increase_cum
#     )

portfolio_list <- list()
ps <- data.frame(Year = 1,
	average_cost_per_producer_cumulative = 1,
	producers_cumulative = NA
	)
for (i in 1:100) {
seed <- i
set.seed(seed)
demand <- clients %>%
  ungroup() %>%
  filter(active_year == TRUE, !is.na(producers), !is.na(revenue_less_risk_less_debt_per_year), revenue_less_risk_less_debt_per_year > -1e5) %>%
  # filter(producers > 0) %>%       # filter producers of zero
  mutate(producers = producers_plus) %>%
  group_by(Year) %>%
  sample_frac(size = 0.8, replace = FALSE) %>% 
  arrange(producers) %>%
  # arrange(-producers) %>%
  mutate(
  	average_cost = revenue_less_risk_less_debt_per_year - opex,
  	average_cost_per_producer = average_cost / producers,
    sales_increase = sales - sales_in_year_zero,
    sales_increase_per_cost = sales_increase - average_cost
  	) %>%
  arrange(average_cost_per_producer) %>%
  arrange(desc(average_cost_per_producer)) %>%
  # arrange(desc(producers)) %>%
  mutate(
    average_cost_cum = cumsum(average_cost),
    producers_cumulative = cumsum(producers),
    average_cost_per_producer_cumulative = average_cost_cum / producers_cumulative
    )

temp <- demand %>%
	group_by(Year) %>%
	filter(producers_cumulative == max(producers_cumulative), 
    average_cost_per_producer_cumulative == min(average_cost_per_producer_cumulative), revenue_less_risk_less_debt_per_year > -1e5) %>%
	# arrange(Year) %>%
	select(Year, average_cost_per_producer_cumulative, producers_cumulative) %>%
  ungroup() %>%
  arrange(Year) %>%
  filter(Year < 2016, Year > 2010)

# portfolio_list[[seed]] <- temp

ps <- rbind(
	ps, temp
	)

}

ps %>%
  filter(Year > 2010) %>%
  ggplot(aes(x = producers_cumulative, y = -average_cost_per_producer_cumulative, color = as.factor(Year))) +
  geom_point()

# producers + employees supply 
p3 <- demand %>%
  ungroup() %>% # 
  filter(Year < 2016, Year > 2010 | Year == 0) %>% # 
  filter(producers_cumulative > 1.5e4) %>% # zoomed out version
  # filter(producers_cumulative > 1.5e5) %>% # zoomed in version
  arrange(average_cost_per_producer_cumulative) %>%
  full_join(portfolio, by = c('Year', 'producers_cumulative', 'average_cost_per_producer_cumulative')) %>%
  mutate(Year = factor(Year)) %>%
  ggplot(
    aes( x = producers_cumulative, y = -average_cost_per_producer_cumulative, group = Year, color = Year, size = Year)) + 
  geom_point(aes(text = Account.Name), alpha = 0.5) + 
  # geom_text(aes(label = Year), ,hjust=0.75, vjust=1.25) + 
  scale_y_continuous('Average Cost per producer', labels = scales::dollar) +
  scale_x_continuous('Producers & Employees Reached', labels = scales::comma) + 
  # geom_path(alpha = 0.1) +
  # geom_path(portfolio, aes(x = producers_cumulative, y = -average_cost_per_producer_cumulative, group = Year, color = factor(Year))) +
  scale_color_brewer('Year', palette = 'Dark2',
    # name = 'Legend Name',
    # breaks = c(0, 2011:2015, 0, NA),
    labels = c('Portfolio and fit line', 2011:2015)) +
  geom_smooth(aes(group = Year==0), 
    method = lm,  formula = y ~ log(x), size = 1.5,
    fullrange = FALSE, se = FALSE, alpha = 0.05, linetype = 'dashed') + # , linetype = "dashed"
  scale_size_manual(NULL, values=c(4, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)) +   #, guide=FALSE)
  guides(size = FALSE) 
  # geom_point(aes(group = Year==0), size = 3)
  # geom_text(aes(label = Year))
p3
