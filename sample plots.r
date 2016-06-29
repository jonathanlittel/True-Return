# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	portfolios_plot1 <- rbind(portfolios1,
		data.frame(
			total_profit = c(max(portfolios1[,1]), min(portfolios1[,1]), min(portfolios1[,1])),
			# gold_silver_count = c(NA, NA, NA),
			# loan_count = c(NA, NA, NA),
			avg_impact_score = c(min(portfolios1[,2]), min(portfolios1[,2]), max(portfolios1[,2]))
			)
		)

# Some different plots
	port_current <- data.frame(total_profit = sum(port_sim[,1]), avg_impact_score = sum(port_sim[,2] / nrow(port_sim))) # add current portfolio points

	port_plot <- ggplot(portfolios_plot1, aes(avg_impact_score, -total_profit/1e6 ))
	port_plot + geom_point() 

	# port_plot <- port_plot + geom_point(alpha = 1/10) + scale_y_continuous(labels = scales::dollar) + scale_x_continuous(labels = scales::percent) + 
	# 			xlab('Percentage of Loans Meeting High Impact Criteria') + ylab("Portfolio Revenue")
	# port_plot + geom_point(data = port_current, colour = "red", size = 4)

	# port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + geom_point(alpha=0.1) +
	 stat_chull(fill = 'brown', alpha=1/5, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='blue', size=3) + 
	 scale_x_continuous('Average Impact Score') +
	 geom_text(data=port_current, label='Current Portfolio', vjust=, nudge_x =-.75, colour='blue')

	port_plot + 
		# geom_point(alpha=0.1) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('Average Impact Score') +
	 geom_text(data=port_current, label='Current Portfolio', vjust=0.45, nudge_x =0.1, colour='white', size=5)


#-------------------------------------------
# Percent above 3.25

# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	portfolios_plot2 <- rbind(portfolios2,
		data.frame(
			total_profit = c(max(portfolios2[,1]), min(portfolios2[,1]), min(portfolios2[,1])),
			# gold_silver_count = c(NA, NA, NA),
			# loan_count = c(NA, NA, NA),
			imp_score_above_325 = c(min(portfolios2[,2]), min(portfolios2[,2]), max(portfolios2[,2]))
			)
		)

# Some different plots
	port_current <- data.frame(total_profit = sum(port_sim[,1]), 
		imp_score_above_325 = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current portfolio points

	port_plot <- ggplot(portfolios_plot2, aes(imp_score_above_325, -total_profit/1e6 ))
	port_plot + geom_point() 

	port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + geom_point(alpha=0.1) +
	 stat_chull(fill = 'brown', alpha=1/5, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='blue', size=3) + 
	 scale_x_continuous('Average Impact Score', label=scales::percent) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=, nudge_x =-.75, colour='blue')

	port_plot + 
		# geom_point(alpha=0.1) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('% loans that commerical banks won\'t do', label=scales::percent) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=0.45, nudge_x =0.02, colour='white', size=5)

#-------------------------------------------
# Percent above 6.25

# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	portfolios_plot3 <- rbind(portfolios3,
		data.frame(
			total_profit = c(max(portfolios3[,1]), min(portfolios3[,1]), min(portfolios3[,1])),
			# gold_silver_count = c(NA, NA, NA),
			# loan_count = c(NA, NA, NA),
			imp_score_above_325 = c(min(portfolios3[,2]), min(portfolios3[,2]), max(portfolios3[,2]))
			)
		)

# Some different plots
	port_current <- data.frame(total_profit = sum(port_sim[,1]), 
		imp_score_above_325 = sum(port_sim$impact_score>6.25) / nrow(port_sim)) # add current portfolio points

	port_plot <- ggplot(portfolios_plot3, aes(imp_score_above_325, -total_profit/1e6 ))
	port_plot + geom_point() 

	port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + geom_point(alpha=0.1) +
	 stat_chull(fill = 'brown', alpha=1/5, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='blue', size=3) + 
	 scale_x_continuous('Average Impact Score', label=scales::percent) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=, nudge_x =-.75, colour='blue')

	port_plot + 
		# geom_point(alpha=0.1) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('% of loans no one else will do', label=scales::percent) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=0.45, nudge_x =0.01, colour='white', size=5)