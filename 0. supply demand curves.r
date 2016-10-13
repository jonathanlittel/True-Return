library(plotly)

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
  filter(active_year == TRUE, !is.na(producers), !is.na(revenue_less_risk_less_debt_per_year)) %>%
  mutate(producers = producers_plus) %>%
  group_by(Year) %>%
  arrange(producers) %>%
  # arrange(-producers) %>%
  mutate(
  	average_cost = revenue_less_risk_less_debt_per_year - opex,
  	average_cost_per_producer = average_cost / producers
  	) %>%
  arrange(average_cost_per_producer) %>%
  arrange(desc(average_cost_per_producer)) %>%
  # arrange(desc(producers)) %>%
  mutate(
    average_cost_cum = cumsum(average_cost),
    producers_cum = cumsum(producers),
    average_cost_per_producer_cum = average_cost_cum / producers_cum
    )

p2 <- demand %>%
  filter(Year > 2009, Year < 2016) %>% # 
  filter(producers_cum > 1.5e5) %>% # 
  arrange(average_cost_per_producer_cum) %>%
  ggplot(
  	aes( x = producers_cum, y = -average_cost_per_producer_cum, group = Year, color = factor(Year))) + 
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
	filter(producers_cum == max(producers_cum), average_cost_per_producer_cum == min(average_cost_per_producer_cum)) %>%
	# arrange(Year) %>%
	select(Year, average_cost_per_producer_cum, producers_cum) %>%
  ungroup() %>%
  arrange(Year) %>%
  filter(Year < 2016, Year > 2010) %>%
  mutate(Year = 0)

lm_supply <- lm(average_cost_per_producer_cum ~ producers_cum, data = portfolio)
lm_supply <- lm(average_cost_per_producer_cum ~ producers_cum, data = demand)

p3 <- demand %>%
  filter() %>% # 
  filter(Year < 2016, Year > 2010 | Year == 0) %>% # 
  # filter(producers_cum > 1.5e4) %>% # zoomed out version
  filter(producers_cum > 1.5e5) %>% # zoomed in version
  arrange(average_cost_per_producer_cum) %>%
  full_join(portfolio, by = c('Year', 'producers_cum', 'average_cost_per_producer_cum')) %>%
  ggplot(
    aes( x = producers_cum, y = -average_cost_per_producer_cum, group = Year, color = factor(Year), size = factor(Year))) + 
  geom_point( alpha = 0.5) + 
  # geom_text(aes(label = Year), ,hjust=0.75, vjust=1.25) + 
  scale_y_continuous('Average Cost per producer', labels = scales::dollar) +
  scale_x_continuous('Producers & Employees Reached', labels = scales::comma) + 
  # geom_path(alpha = 0.1) +
  # geom_path(portfolio, aes(x = producers_cum, y = -average_cost_per_producer_cum, group = Year, color = factor(Year))) +
  scale_color_brewer('Year', palette = 'Dark2') +
  geom_smooth(aes(group = Year==0), 
    method = lm,  formula = y ~ log(x), size = 1.5,
    fullrange = FALSE, se = FALSE, alpha = 0.05, linetype = 'dashed') + # , linetype = "dashed"
  scale_size_manual(NULL, values=c(4, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)) +   #, guide=FALSE)
  guides(size = FALSE) 
  # geom_point(aes(group = Year==0), size = 3)
  # geom_text(aes(label = Year))
p3


ggplotly(p3) %>% 
  config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = TRUE, editable = F, sendData = FALSE, displaylogo = FALSE,
               modeBarButtonsToRemove = list("sendDataToCloud")) 

	demand %>% group_by(Year) %>% summarise(sum(average_cost, na.rm=T), min(average_cost_cum))


# add points for actual portfolios



# demand %>%
#   filter(Year > 2009, Year < 2016) %>%
#   arrange(producers_cum) %>%
#   ggplot(
#     aes( x = producers_cum, y = -average_cost_per_producer, color = Year)) + 
#   # geom_point() + 
#   # geom_text(aes(label = Year), hjust=0.75, vjust=1.25) + 
#   scale_y_continuous('Average Cost per producer', labels = scales::dollar) +
#   scale_x_continuous('Producers Reached', labels = scales::comma) + 
#   geom_path()

  # ggplot(
  #   filter(demand, Year > 2009, Year < 2016),
  #   aes( x = producers, y = revenue_less_risk/producers)) + geom_point() + 
  # geom_text(aes(label = Year), hjust=0.75, vjust=1.25) + 
  # scale_y_continuous('Marginal Revenue per producer', labels = scales::dollar) +
  # scale_x_continuous('Producers Reached', labels = scales::comma)

  # ggplot(
  #   filter(demand, Year > 2009, Year < 2016),
  #   aes( x = producers, y = opex/producers)) + geom_point() + 
  # geom_text(aes(label = Year), hjust=0.75, vjust=1.25) + 
  # scale_y_continuous('opex per producer', labels = scales::dollar) +
  # scale_x_continuous('Producers Reached', labels = scales::comma)

  # ggplot(
  #   filter(demand, Year > 2009, Year < 2016),
  #   aes( x = producers, y = opex/payments_to_producers)) + geom_point() + 
  # geom_text(aes(label = Year) ,hjust=0.75, vjust=1.25) + 
  # scale_y_continuous('opex per payment to producer', labels = scales::dollar) +
  # scale_x_continuous('Producers Reached', labels = scales::comma)

