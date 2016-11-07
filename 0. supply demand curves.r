library(plotly)

source('C:/Box Sync/jlittel/comms/client financials/0. merge and process new.r')

# ----------------------   data prep --------------------------

# cost per year per client for opex
  active_clients <- clients %>% group_by(Year) %>% filter(active_year==TRUE) %>% summarise(n = n())
  temp <- as.vector(active_clients[1:11,2]) * 20000
  temp <- as.vector(temp[['n']])
  opex <- c(temp ,    # 1999 to 2009
  	3599348,	5056206,	6250918,	6409555,	7132026,	7394796,
  	7e6)  # 2016 est

 opex.df <- data.frame(
 	Year = 1999:2016,
 	opex = opex / active_clients[['n']])  

 clients <- clients %>%
 	left_join(opex.df, by = 'Year')

clients$producers[is.na(clients$producers)] <- 0
clients$employees[is.na(clients$employees)] <- 0
clients$employees_part_time[is.na(clients$employees_part_time)] <- 0

clients$producers_plus <- clients$producers + clients$employees + clients$employees_part_time

# ----------------------   graph  --------------------------

demand <- clients %>%
  ungroup() %>%
  filter(active_year == TRUE, !is.na(producers), !is.na(revenue_less_risk_less_debt_per_year), revenue_less_risk_less_debt_per_year > -1e5) %>%
  filter(producers > 0) %>%       # filter producers of zero
  # filter(producers <= 3000) %>%       # filter large producers 95% < 3000
  mutate(producers = producers_plus) %>%
  group_by(Year) %>%
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

p2 <- demand %>%
  filter(Year > 2009, Year < 2016) %>% # 
  filter(producers_cumulative > 1.5e5) %>% # 
  arrange(average_cost_per_producer_cumulative) %>%
  ggplot(
  	aes( x = producers_cumulative, y = -average_cost_per_producer_cumulative, group = Year, color = factor(Year))) + 
  geom_point() + 
  # geom_text(aes(label = Year), ,hjust=0.75, vjust=1.25) + 
  scale_y_continuous('Average Cost per producer', labels = scales::dollar) +
  scale_x_continuous('Producers Reached', labels = scales::comma) + 
  geom_path() +
  scale_color_brewer('Year', palette = 'Dark2') + 
  geom_text(aes(label = Year))
p2

a <- ggplot(demand, aes(x = Year, y = producers)) + geom_line()
b <- ggplot(demand, aes(x = Year, y = revenue_less_risk)) + geom_line()
c <- ggplot(demand, aes(x = Year, y = opex)) + geom_line()
library(gridExtra)
grid.arrange(a, b, c, ncol=3)

portfolio <- demand %>%
	group_by(Year) %>%
	filter(producers_cumulative == max(producers_cumulative), 
    average_cost_per_producer_cumulative == min(average_cost_per_producer_cumulative), revenue_less_risk_less_debt_per_year > -1e5) %>%
	# arrange(Year) %>%
	select(Year, average_cost_per_producer_cumulative, producers_cumulative) %>%
  ungroup() %>%
  arrange(Year) %>%
  filter(Year < 2016, Year > 2010) %>%
  mutate(Year = 0)

lm_supply <- lm(average_cost_per_producer_cumulative ~ producers_cumulative, data = portfolio)
lm_supply <- lm(average_cost_per_producer_cumulative ~ producers_cumulative, data = demand)


# producers + employees supply 
p3 <- demand %>%
  ungroup() %>% # 
  filter(Year < 2016, Year > 2010 | Year == 0) %>% # 
  # filter(producers_cumulative > 1.5e4) %>% # zoomed out version
  filter(producers_cumulative > 1.5e5) %>% # zoomed in version
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

ggsave('supply_curve_draft_wide.png', plot = p3, device = NULL, path = NULL,
  scale = 1, width = 6, height = 10, units = 'in',
  dpi = 300, limitsize = TRUE)



ggplotly()

ggplotly(p3) %>% layout(margin=list(r=120)) %>% layout(title = "Average cost per producer reached, <br> (by annual portfolio)",
         showlegend = TRUE)

ggplotly(p3) %>% 
  config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = TRUE, editable = F, sendData = FALSE, displaylogo = FALSE,
               modeBarButtonsToRemove = list("sendDataToCloud")) %>% layout(margin=list(r=120)) %>% layout(title = "Average cost per producer reached, <br> (by annual portfolio)",
         showlegend = TRUE)

demand %>% group_by(Year) %>% summarise(sum(average_cost, na.rm=T), min(average_cost_cum))

# add points for actual portfolios


# ----------------------   graph  --------------------------

demand2 <- clients %>%
  group_by(RC.Account.Number) %>%
  arrange(Year) %>%
  mutate(sales_lag = lag(sales) ) %>%
  ungroup() %>%
  filter(active_year == TRUE, !is.na(producers), !is.na(revenue_less_risk_less_debt_per_year)) %>%
  mutate(producers = producers_plus) %>%
  group_by(Year) %>%
  arrange(producers) %>%
  # arrange(-producers) %>%
  mutate(
    average_cost = revenue_less_risk_less_debt_per_year - opex,
    average_cost_per_producer = average_cost / producers,
    sales_increase = sales - sales_lag,
    sales_increase_per_cost = sales_increase - average_cost
    ) %>%
  arrange(average_cost_per_producer) %>%
  arrange(desc(average_cost_per_producer)) %>%
  # arrange(desc(producers)) %>%
  mutate(
    average_cost_cum = cumsum(average_cost),
    producers_cumulative = cumsum(producers),
    average_cost_per_producer_cumulative = average_cost_cum / producers_cumulative
    ) %>%
  arrange(desc(sales_increase_per_cost)) %>%
  mutate(
    average_cost_cum = cumsum(average_cost),
    sales_increase_cum = cumsum(sales_increase),
    average_cost_per_sales_increase_cum = average_cost_cum / sales_increase_cum
    )


p4 <- demand2 %>%
  filter() %>% # 
  filter(Year < 2016, Year > 2008 | Year == 0) %>% # 
  # filter(producers_cumulative > 1.5e4) %>% # zoomed out version
  filter(sales_increase_cum > 5e7) %>% # zoomed in version
  arrange(average_cost_per_producer_cumulative) %>%
  # full_join(portfolio, by = c('Year', 'sales_increase_cum', 'average_cost_per_producer_cumulative')) %>%  # adds current portfolio
  ggplot(
    aes( x = sales_increase_cum, y = -average_cost_per_sales_increase_cum, group = Year, color = factor(Year), size = factor(Year))) + 
  geom_point( alpha = 0.5) + 
  # geom_text(aes(label = Year), ,hjust=0.75, vjust=1.25) + 
  scale_y_continuous('Average Cost per cumulative sales increase', labels = scales::dollar) +
  scale_x_continuous('Cumulative Sales Increase', labels = scales::comma) + 
  # geom_path(alpha = 0.1) +
  # geom_path(portfolio, aes(x = sales_increase_cum, y = -average_cost_per_producer_cumulative, group = Year, color = factor(Year))) +
  scale_color_brewer('Year', palette = 'Dark2') +
  geom_smooth(aes(group = Year==0), 
    method = lm,  formula = y ~ log(x), size = 1.5,
    fullrange = FALSE, se = FALSE, alpha = 0.05, linetype = 'dashed') + # , linetype = "dashed"
  scale_size_manual(NULL, values=c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)) +   #, guide=FALSE)
  guides(size = FALSE) 
  # geom_point(aes(group = Year==0), size = 3)
  # geom_text(aes(label = Year))
p4




clients %>% filter(active_year==TRUE) %>% group_by(Year) %>%summarise(n(), sum(is.na(pd)), sum(is.na(producers)), sum(is.na(producers) | is.na(pd)) / `n()`)


# check:
  # single year loans straddling year
  # sanity check = clients match clients reached (yes, though not quite)
  # check that total costs roughly add up to FS