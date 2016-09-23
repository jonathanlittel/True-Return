supply <- arrange(port_sim, desc(profit))

# to add point at 0,0
# supply <- rbind(
# 		data.frame(
# 			profit = 0,
# 			impact_score = 10))

supply$additionality <- cut(supply$impact_score, 
	c(0, 3.25, 6.25, 10),
	labels = c('low', 'medium', 'high')
	)

supply <- supply %>% 
	filter(impact_score > 3.25) %>%
	arrange(desc(profit)) %>%
	mutate(
	profit_cum = cumsum(profit),
	temp = 1,
	impact_score_above_cut = cumsum(temp)
	)

		s <- ggplot(supply, aes(x = impact_score_above_cut, y = -profit_cum/1e6/impact_score_above_cut))
		s + geom_line() + scale_x_continuous('Med additionality loans', limits = c(0, 150))	+ ylab('subsidy ($M)')		

		s + geom_line() + scale_x_continuous('Med additionality loans', limits = c(0, 150))	+ ylab('subsidy per additional unit ($M)')	+ 
			geom_line(aes(x = impact_score_above_cut + 2, y = -profit_cum/1e6/impact_score_above_cut  - .1), linetype = 2)

write.csv(supply, 'supply_curve_example_medium_impact_loans.csv')