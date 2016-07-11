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
		portfolios2$imp_score_above_325 <- 259 * portfolios2$imp_score_above_325
		portfolios_plot2 <- rbind(portfolios2,
		data.frame(
			total_profit = c(max(portfolios2[,1]), -4e6, -4e6), # min of -4e6
			# total_profit = c(max(portfolios2[,1]), min(portfolios2[,1]), min(portfolios2[,1])), # all points
			# imp_score_above_325 = c(min(portfolios2[,2]), min(portfolios2[,2]), max(portfolios2[,2])) # all points
			imp_score_above_325 = c(145, 145, max(portfolios2[,2])) # trim to 145
			)
		)

# One curve - loans comm banks won't do
	port_current <- data.frame(total_profit = sum(port_sim[,1]), 
		# imp_score_above_325 = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
		imp_score_above_325 = sum(port_sim$impact_score>3.25)) # add current # portfolio points

	port_plot <- ggplot(portfolios_plot2, aes(imp_score_above_325, total_profit/1e6 ))
	port_plot + geom_point() 

	port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + geom_point(alpha=0.1) +
	 stat_chull(fill = 'brown', alpha=1/5, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='blue', size=3) + 
	 scale_x_continuous('Average Impact Score', label=scales::percent) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=, nudge_x =-.75, colour='blue')

	port_plot + 
		geom_point(alpha=0.05, size=1,) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = 'white', size=1) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar, limits=c(-4,0)) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('# of loans commercial banks won\'t do', limits=c(145, 170)) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=0.45, hjust=-.25, colour='white', size=5)

#-------------------------------------------
# Percent above 6.25

# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	 # convert back from pct above to absolute #
	 portfolios3$imp_score_above_325 <- 259 * portfolios3$imp_score_above_325
	portfolios_plot3 <- rbind(portfolios3,
		data.frame(
			total_profit = c(max(portfolios3[,1]), min(portfolios3[,1]), min(portfolios3[,1])),
			# imp_score_above_325 = c(29, 29, max(portfolios3[,2]))		
			imp_score_above_325 = c(min(portfolios3[,2]), min(portfolios3[,2]), max(portfolios3[,2]))
			)
		)

# Some different plots
	port_current <- data.frame(total_profit = sum(port_sim[,1]), 
		# imp_score_above_325 = sum(port_sim$impact_score>6.25) / nrow(port_sim)) # add current [ct portfolio 
		imp_score_above_325 = sum(port_sim$impact_score>6.25) ) # add current # portfolio points

	port_plot <- ggplot(portfolios_plot3, aes(imp_score_above_325, total_profit/1e6 ))
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
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar, limits=c(NA, 0)) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('# of loans no one else will do', limits=c(29,55)) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=0.45, hjust=-.25, colour='white', size=5)



#------------------------------
# combining # above 3.25 and 6.25
	 # change left corners to reach 0
	 	portfolios_plot3 <- rbind(portfolios3,
		data.frame(
			total_profit = c(max(portfolios3[,1]), min(portfolios3[,1]), min(portfolios3[,1])),
			# imp_score_above_325 = c(29, 29, max(portfolios3[,2]))		
			imp_score_above_325 = c(0, 0, max(portfolios3[,2]))
			)
		)

		portfolios_plot2 <- rbind(portfolios2,
		data.frame(
			total_profit = c(max(portfolios2[,1]), min(portfolios2[,1]), min(portfolios2[,1])),
			# imp_score_above_325 = c(min(portfolios3[,2]), min(portfolios3[,2]), max(portfolios2[,2]))
			imp_score_above_325 = c(0, 0, max(portfolios2[,2]))
			)
		)


	portfolios_plot3$market <- 'Loans only Root will do'
	portfolios_plot2$market <- 'Loans only Social Lenders will do'
	portfolios_plot4$market <- factor(portfolios_plot4$market)
	portfolios_plot4 <- rbind(portfolios_plot2, portfolios_plot3)
	port_plot4 <- ggplot(portfolios_plot4, aes(imp_score_above_325, total_profit/1e6))
	port_plot4 + 
	# geom_point(alpha=0.1) +
	 stat_chull(aes(colour = market), fill = 'blue', alpha=0.25) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar, limits=c(NA, 0)) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('# of loans') + # , limits=c(29,55)
	 geom_text(data=port_current, label='Current Portfolio', vjust=2, nudge_x=-6, colour='white', size=5) + 
	 theme(legend.position = 'bottom')



# Percent above 3.25, with weighted and unweighted sampling

# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
		portfolios2a$imp_score_above_325 <- 259 * portfolios2a$imp_score_above_325
		portfolios2$imp_score_above_325 <- 259 * portfolios2$imp_score_above_325		
		portfolios_plot2 <- rbind(
			portfolios2,
			portfolios2a,
		data.frame(
			total_profit = c(max(portfolios2[,1]), -4e6, -4e6), # min of -4e6
			# total_profit = c(max(portfolios2[,1]), min(portfolios2[,1]), min(portfolios2[,1])), # all points
			# imp_score_above_325 = c(min(portfolios2[,2]), min(portfolios2[,2]), max(portfolios2[,2])) # all points
			imp_score_above_325 = c(145, 145, max(portfolios2[,2])) # trim to 145
			)
		)

# One curve - loans comm banks won't do
	port_current <- data.frame(total_profit = sum(port_sim[,1]), 
		# imp_score_above_325 = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
		imp_score_above_325 = sum(port_sim$impact_score>3.25)) # add current # portfolio points

	port_plot <- ggplot(portfolios_plot2, aes(imp_score_above_325, total_profit/1e6 ))
	port_plot + geom_point() 

	port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + geom_point(alpha=0.1) +
	 stat_chull(fill = 'brown', alpha=1/5, colour = NA) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar) + 
	 geom_point(data= port_current, colour='blue', size=3) + 
	 scale_x_continuous('Average Impact Score', label=scales::percent) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=, nudge_x =-.75, colour='blue')

	port_plot + 
		geom_point(alpha=0.05, size=1,) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = 'white', size=1) +
	 scale_y_continuous('Philanthropic Support Required ($M)', label=scales::dollar, limits=c(-4,0)) + 
	 geom_point(data= port_current, colour='white', size=3) + 
	 scale_x_continuous('# of loans commercial banks won\'t do', limits=c(145, 170)) +
	 geom_text(data=port_current, label='Current Portfolio', vjust=0.45, hjust=-.25, colour='white', size=6)
