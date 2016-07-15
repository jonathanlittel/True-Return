#---------------------------
# plot 1 - above 3.25
#---------------------------

# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	plot_data1 <- rbind(
		portfolios1,
		portfolios1a,
		portfolios1b,
		portfolios1c,
		portfolios1d,
		portfolios1e,
		portfolios1f,
		portfolios1g,
		portfolios1h,
		portfolios1i,
		portfolios1j,
		portfolios1z
		)
	portfolios1k <- filter(plot_data1, total_profit<=-4e6)
	portfolios1k$total_profit <- portfolios1k$total_profit - (runif(nrow(portfolios1k))*5e5)  # add some points with more loss (random $0-500k)
	portfolios1l <- filter(plot_data1, total_profit<=-4e6)
	portfolios1l$total_profit <- portfolios1l$total_profit - (runif(nrow(portfolios1l))*1e6)  # add some points with more loss (random $0-500k)
	portfolios1m <- filter(plot_data1, impact_score_above_cut>154)
	portfolios1m$total_profit <- portfolios1m$total_profit - (runif(nrow(portfolios1m))*5e5)  # add some points with more loss (random $0-500k)

	plot_data1 <- rbind(
		plot_data1,
		portfolios1k,
		portfolios1l,
		portfolios1m
		)
	port_current <- data.frame(
		total_profit = sum(port_sim[,1]),
		impact_score_above_cut = sum(port_sim$impact_score>3.25),
		category = 'above325'
		)
	plot_data1 <- rbind(
		filter(plot_data1,
	 		total_profit>=port_current$total_profit[1],
	 		impact_score_above_cut>=port_current$impact_score_above_cut[1]
	 		),
		data.frame(   # these are the corner points for the area file stat_chull chart
			# points:      _upper left_               _lower left_           _lower right_
			total_profit = c(-2e6, -3999999, -3999999),
			impact_score_above_cut = c(145, 145, 170), 
			avg_impact_score = c(min(portfolios1[,2]), min(portfolios1[,2]), max(portfolios1[,2]))
			)
		)


	# port_max <- plot_data1 %>%
	# 							group_by(impact_score_above_cut) %>%
	# 							mutate(impact_score_above_cut_max = max(total_profit))
	port_plot1 <- ggplot(
		filter(plot_data1, total_profit>=-5e6), aes(impact_score_above_cut, total_profit/1e6 ))
    port_plot1 + 
	 geom_point(alpha=0.005, size=0.75,) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = 'white', size=1) +
	 scale_y_continuous('', label=scales::dollar, limits=c(-4,0)) + # 'Philanthropic Support Required ($M)',
	 geom_point(data= port_current, colour='white', size=5) + 
	 scale_x_continuous('',limits=c(145, 175)) # +  '# of loans commercial lenders likely would not have done in our absence'
	 # geom_text(data=port_current,  vjust=0.45, hjust=-.25, colour='white', size=5) # label='Current Portfolio',

ggsave(filename = 'plot1.png', plot = last_plot(), # device = default_device(filename),
   scale = 1, width = par("din")[1], height = par("din")[2], units = c("in"), dpi = 600) # path = NULL,


#-------------------------------------
# plot 2 - above 6.25
#-------------------------------------
# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	xlims2 <- c(40, 55)
	plot_data2 <- rbind(
		# portfolios2,
		portfolios2a,
		portfolios2b,
		# portfolios2c,
		# portfolios2d,
		# portfolios2e,
		# portfolios2f,
		portfolios2g,
		portfolios2h
		)
	port_current2 <- data.frame(
		total_profit = sum(port_sim[,1]),
		impact_score_above_cut = sum(port_sim$impact_score>6.25),
		category = 'above625'
		)
	plot_data2 <- filter(plot_data2,
	 total_profit>=port_current2$total_profit[1],
	 impact_score_above_cut>=port_current2$impact_score_above_cut[1]
	 )

	plot_data2 <- rbind(
		plot_data2,
			data.frame(   # these are the corner points for the area file stat_chull chart
			# points:      _upper left_               _lower left_           _lower right_
			total_profit = c(max(plot_data2[,1])+1e5, -3999999, -3999999),
			impact_score_above_cut = c(40, 40, max(plot_data2[,2])), 
			avg_impact_score = c(min(plot_data2[,2]), min(plot_data2[,2]), max(plot_data2[,2]))
			)
		)


	apply(plot_data2, 2, max)
	# port_max2 <- plot_data2 %>%
	# 							group_by(impact_score_above_cut) %>%
	# 							mutate(impact_score_above_cut_max = max(total_profit))
# One curve - loans comm banks won't do
	# port_current <- data.frame(total_profit = sum(port_sim[,1]), 
	# 	# impact_score_above_cut = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
	# 	impact_score_above_cut = sum(port_sim$impact_score>3.25)) # add current # portfolio points

	port_plot2 <- ggplot(plot_data2,
		aes(impact_score_above_cut, total_profit/1e6 ))
	# port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
    port_plot2 + 
	 geom_point(alpha=0.005, size=0.75,) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = 'white', size=1) +
	 scale_y_continuous('', label=scales::dollar, limits=c(-4,0)) + # 'Philanthropic Support Required ($M)',
	 geom_point(data= port_current2, colour='white', size=5) + 
	 scale_x_continuous('',limits=xlims2) # +  '# of loans commercial lenders likely would not have done in our absence'
	 # geom_text(data=port_current,  vjust=0.45, hjust=-.25, colour='white', size=5) # label='Current Portfolio',

ggsave(filename = 'plot2.png', plot = last_plot(), # device = default_device(filename),
   scale = 1, width = par("din")[1], height = par("din")[2], units = c("in"), dpi = 600) # path = NULL,



#---------------------------------
# plot 3 - combining the two
#---------------------------------

# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	plot_data1$category <- 'above325'
	plot_data2$category <- 'above625'
	plot1_edges <- data.frame(   # these are the corner points for the area file stat_chull chart
			# points:      _upper left_               _lower left_           _lower right_
			total_profit = c(max(plot_data2[,1]), -3999999, -3999999),
			impact_score_above_cut = c(35, 35, 170), 
			avg_impact_score = c(min(plot_data1[,2]), min(plot_data1[,2]), max(plot_data1[,2])),
			category = 'above325'
			)	
	plot2_edges <- data.frame(   # these are the corner points for the area file stat_chull chart
			# points:      _upper left_               _lower left_           _lower right_
			total_profit = c(max(plot_data2[,1]), -3999999, -3999999),
			impact_score_above_cut = c(35, 35, 50), 
			avg_impact_score = c(min(plot_data2[,2]), min(plot_data2[,2]), max(plot_data2[,2])),
			category = 'above625'
			)
	plot_data3 <- rbind(
					plot_data1,
					plot_data2,
					plot1_edges,
					plot2_edges
					)
	port_max3 <- plot_data3 %>%
					group_by(impact_score_above_cut) %>%
					mutate(impact_score_above_cut_max = max(total_profit))
# One curve - loans comm banks won't do
	# port_current <- data.frame(total_profit = sum(port_sim[,1]), 
	# 	# impact_score_above_cut = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
	# 	impact_score_above_cut = sum(port_sim$impact_score>3.25)) # add current # portfolio points

	port_plot3 <- ggplot(plot_data3, aes(impact_score_above_cut, total_profit/1e6, group = category ))
    port_plot3 + 
	 geom_point(alpha=0.005, size=0.75, ) +
	 stat_chull(fill = 'blue', alpha=0.25, colour = 'white', size=1) +
	 scale_x_continuous('',limits=c(35, 170)) +
	 scale_y_continuous('', label=scales::dollar, limits=c(-4,0)) + 
	 geom_point(data= port_current, colour='white', size=5) +	 
	 geom_point(data= port_current2, colour='white', size=5)
	 # scale_y_continuous('', label=scales::dollar, limits=c(-5,0)) + # 'Philanthropic Support Required ($M)',
	 # geom_point(data= port_current, colour='white', size=5) + 
	 # scale_x_continuous('',limits=c(0, 160)) #  +  '# of loans commercial lenders likely would not have done in our absence'
	 # # geom_text(data=port_current,  vjust=0.45, hjust=-.25, colour='white', size=5) # label='Current Portfolio',

ggsave(filename = 'plot3.png', plot = last_plot(), # device = default_device(filename),
   scale = 1, width = par("din")[1], height = par("din")[2], units = c("in"), dpi = 600) # path = NULL,


# #---------------------------------
# # plot 3a - one graph
# #---------------------------------

# # add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
# 	plot_data3 <- rbind(
# 					plot_data1,
# 					plot_data2
# 					)
# 	port_max3 <- plot_data3 %>%
# 					group_by(impact_score_above_cut) %>%
# 					mutate(impact_score_above_cut_max = max(total_profit))
# # One curve - loans comm banks won't do
# 	# port_current <- data.frame(total_profit = sum(port_sim[,1]), 
# 	# 	# impact_score_above_cut = sum(port_sim$impact_score>3.25) / nrow(port_sim)) # add current pct portfolio points
# 	# 	impact_score_above_cut = sum(port_sim$impact_score>3.25)) # add current # portfolio points

# 	port_plot <- ggplot(plot_data3, aes(impact_score_above_cut, total_profit/1e6 ))
#     port_plot + 
# 	 geom_point(alpha=0.005, size=0.75,) +
# 	 stat_chull(fill = 'blue', alpha=0.25, colour = 'white', size=1) +
# 	 scale_y_continuous('', label=scales::dollar, limits=c(-5,0)) + # 'Philanthropic Support Required ($M)',
# 	 geom_point(data= port_current, colour='white', size=5) + 
# 	 scale_x_continuous('',limits=c(0, 160)) # +  '# of loans commercial lenders likely would not have done in our absence'
# 	 # geom_text(data=port_current,  vjust=0.45, hjust=-.25, colour='white', size=5) # label='Current Portfolio',

# # ggsave(filename = 'plot1.png', plot = last_plot(), # device = default_device(filename),
# #    scale = 1, width = par("din")[1], height = par("din")[2], units = c("in"), dpi = 600) # path = NULL,
