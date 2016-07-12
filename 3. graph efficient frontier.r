#---------------------------
# plot 1 - above 3.25
#---------------------------
# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	portfolios_plot1 <- rbind(
		portfolios1,
		portfolios1a,
		portfolios1c,
		data.frame(
			# points:      _upper left_               _lower left_           _lower right_
			total_profit = c(max(portfolios1[,1]), min(portfolios1[,1]), min(portfolios1[,1])),
			impact_score_above_cut = c(125, 125, max(portfolios1[,2])), 
			avg_impact_score = c(min(portfolios1[,2]), min(portfolios1[,2]), max(portfolios1[,2]))
			)
		)
	port_current <- data.frame(
		total_profit = sum(port_sim[,1]), 
		# imp_score_above_cut = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
		impact_score_above_cut = sum(port_sim$impact_score>3.25)
		)
# One curve - loans comm banks won't do
	# port_current <- data.frame(total_profit = sum(port_sim[,1]), 
	# 	# impact_score_above_cut = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
	# 	impact_score_above_cut = sum(port_sim$impact_score>3.25)) # add current # portfolio points

	port_plot <- ggplot(portfolios_plot1, aes(impact_score_above_cut, total_profit/1e6 ))
	# port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + 
	 geom_point(alpha=0.1) +
	 stat_chull(fill = 'brown', alpha=1/5, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='blue', size=3) + 
	 scale_x_continuous('# of loans commercial banks won\'t do', limits=c(145, 170)) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=1, nudge_x =-.75, colour='blue')


org identity when fulfilling an investment policy and goals?
one process for risk assessment? 1) will the market buy that, 2) is that an aggregated risk?

general model is that half of the income comes from loans, half from grants. Who would invest in that?
non-profit?