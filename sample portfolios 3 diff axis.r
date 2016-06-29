# first process and merge data
	source('# merge impact scores.r')

# Functions to plot the convex hull of the points
# per here: http://docs.ggplot2.org/current/vignettes/extending-ggplot2.html
		StatChull <- ggproto("StatChull", Stat,
			  compute_group = function(data, scales) {
			    data[chull(data$x, data$y), , drop = FALSE]
			  },
			  
			  required_aes = c("x", "y")
			)

		stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
		                       position = "identity", na.rm = FALSE, show.legend = NA, 
		                       inherit.aes = TRUE, ...) {
		  layer(
		    stat = StatChull, data = data, mapping = mapping, geom = geom, 
		    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
		    params = list(na.rm = na.rm, ...)
		  )
		}



############################
# require(microbenchmark)
# 0. Sample using Impact.Rating	
############################	
	# Generate portfolio combinations
	port_sim <- select(df.rap.inactive, Marginal_Revenue, impact_score)  # Highest_Impact_Loans
	dim(port_sim)
	port_sim <- port_sim[complete.cases(port_sim),]
	dim(port_sim)
	portfolios <- port_sim[1,] # simple way to create the dataframe - remove this row after loop
	min_loans <- 200 # minimum number of loans in p

	for (i in 1:100) {
		n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio

		portfolios <- rbind(portfolios, 
				port_sim %>% 
				    # group_by(cyl) %>% 
				    sample_n(n) %>%
				    summarise_each(funs(mean))   # need to change this to count of port_sim[,1]==1 / length(port_sim)
				    # summarise(grouped, mean=mean(value), sd=sd(value))
		    )

	}

	portfolios0 <- portfolios[-1,] # remove first row since it was instantiated w sample data
	port_plot1 <- ggplot(portfolios0, aes(impact_score, Marginal_Revenue ))
	port_plot1 <- port_plot1 + geom_point() + stat_chull(fill = NA, colour = "blue")
	port_plot1

########################################################
# 1. average impact score
##########################################################################
	n <- 5000
	# n <- 10
	port_sim <- select(df.rap, profit, impact_score)
	port_sim <- port_sim[complete.cases(port_sim),]
	check <- 1

	loan_count <- rep(NA, n)
	portfolios <- c(0,0) #port_sim[1,] # simple way to create the dataframe - remove this row after loop
	min_loans <- dim(port_sim)[1] # minimum number of loans in p

	for (i in 1:n) {
		set.seed(i)
		add_index <- createDataPartition(port_sim[,1], times=1, p=0.1, list=FALSE)
		check[i] <- sum(add_index)
		port <- rbind(port_sim, port_sim[add_index,])
		# n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio
		loan_count[i] <- min_loans

		portfolios <- rbind(portfolios, 
				port %>% 
					mutate(weight = (i ^ 1.5 ) / i ) %>%
					# create a normalized weight for profit, and add ~ 4.5 to put it around the impact score
					mutate(normalized = 4 + (profit - min(profit)) / (max(profit) - min(profit))) %>%
				    sample_n(min_loans, weight= ( impact_score^weight + normalized^weight)) %>% #
				    # summarise_each(funs(mean)) %>%
				    summarise(
				    	total_profit = sum(profit), 
				    	avg_impact_score = mean(impact_score))
		    )

	}

	portfolios1 <- portfolios[-1,] # remove first row since it was instantiated w sample data




########################################################
# 2.  Sample Highest_Impact_Loans as % of portfolio and 'profit'
##########################################################################
	n <- 5e2
	port_sim <- select(df.rap, profit, impact_score)  # Option 2
	port_sim <- port_sim[complete.cases(port_sim),]
	test <- 1

	loan_count <- rep(NA, n)
	portfolios <- c(0,0) #port_sim[1,] # simple way to create the dataframe - remove this row after loop
	min_loans <- dim(port_sim)[1] # minimum number of loans in p
	for (i in 1:n) {
		set.seed(i)
		add_index <- createDataPartition(port_sim[,1], times=1, p=0.1, list=FALSE)
		test[i] <- sum(add_index)
		port <- rbind(port_sim, port_sim[add_index,])
		# n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio
		loan_count[i] <- min_loans

		portfolios <- rbind(portfolios, 
				port %>%
					mutate(weight = (i ^ 1.5 ) / i ) %>%
					# create a normalized weight for profit, and add ~ 4.5 to put it around the impact score
					mutate(normalized = 4 + (profit - min(profit)) / (max(profit) - min(profit))) %>% 
				    sample_n(min_loans, weight= ( impact_score^weight + normalized^weight)) %>%
				    # summarise_each(funs(mean)) %>%
				    summarise(
				    	total_profit = sum(profit), 
				    	imp_score_above_325 = (sum(impact_score>3.25)/loan_count[i]),
				    	n = n()
				    	)
		    )

	}

portfolios2 <- portfolios[-1,] # remove first row since it was instantiated w sample data


########################################################
# 3.  Sample Highest_Impact_Loans as % of portfolio and 'profit'
##########################################################################
	n <- 1e6
	n <- 10
	port_sim <- select(df.rap, profit, impact_score)  # Option 2
	port_sim <- port_sim[complete.cases(port_sim),]
	test <- 1

	loan_count <- rep(NA, n)
	portfolios <- c(0,0) #port_sim[1,] # simple way to create the dataframe - remove this row after loop
	min_loans <- 315 # minimum number of loans in p
	for (i in 1:n) {
		set.seed(i)
		add_index <- createDataPartition(port_sim[,1], times=1, p=0.1, list=FALSE)
		test[i] <- sum(add_index)
		port <- rbind(port_sim, port_sim[add_index,])
		# n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio
		loan_count[i] <- min_loans

		portfolios <- rbind(portfolios, 
				port %>% 
				    sample_n(min_loans) %>%
				    # summarise_each(funs(mean)) %>%
				    summarise(total_profit = sum(profit), 
				    	imp_score_above_2_pct = (sum(impact_score>2)/loan_count[i]))
		    )

	}

portfolios3 <- portfolios[-1,] # remove first row since it was instantiated w sample data
###################################################################
# portfolios	<- cbind(portfolios, loan_count)
# portfolios$pct_gold_silver <- portfolios[,2] / portfolios$loan_count
# names(portfolios) <- c('profit', 'gold_silver_count', 'loan_count', 'pct_gold_silver')

########################################################
# 4.  weighted sampling
##########################################################################
	n <- 1e6
	n <- 10
	port_sim <- select(df.rap, profit, impact_score)  # Option 2
	port_sim <- port_sim[complete.cases(port_sim),]
	test <- 1

	loan_count <- rep(NA, n)
	portfolios <- c(0,0) #port_sim[1,] # simple way to create the dataframe - remove this row after loop
	min_loans <- 315 # minimum number of loans in p
	for (i in 1:n) {
		set.seed(i)
		add_index <- createDataPartition(port_sim[,1], times=1, p=0.1, list=FALSE)
		test[i] <- sum(add_index)
		port <- rbind(port_sim, port_sim[add_index,])
		# n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio
		loan_count[i] <- min_loans

		portfolios <- rbind(portfolios, 
				port %>% 
				    sample_n(min_loans, weight=impact_score) %>%
				    # summarise_each(funs(mean)) %>%
					# smp_ind <- sample(seq_len(nrow(port), min_loans, prob=weight)
				    summarise(total_profit = sum(profit), 
				    	imp_score_above_2_pct = (sum(impact_score>2)/loan_count[i]))
		    )

	}

portfolios2 <- portfolios[-1,] # remove first row since it was instantiated w sample data
###############################################
# Some different plots
	port_current <- data.frame(profit = sum(port_sim[,1]), pct_gold_silver = sum(port_sim[,2] / nrow(port_sim))) # add current portfolio points

	port_plot <- ggplot(portfolios, aes(pct_gold_silver, profit ))
	port_plot + geom_point() 

	port_plot <- port_plot + geom_point(alpha = 1/10) + scale_y_continuous(labels = scales::dollar) + scale_x_continuous(labels = scales::percent) + 
				xlab('Percentage of Loans Meeting High Impact Criteria') + ylab("Portfolio Revenue")
	port_plot + geom_point(data = port_current, colour = "red", size = 4)

	port_plot + geom_point() + stat_chull(fill = NA, colour = "red")
	port_plot + geom_point() + stat_chull(fill = 'brown', alpha=1/5, colour = NA)

	# add three points, for the stat_chull area plot - the max profit at 0% impact, 0,0, and 100% impact at the min profit
	portfoliosx <- rbind(portfolios,
		data.frame(
			profit = c(max(portfolios[,1]), min(portfolios[,1]), min(portfolios[,1])),
			gold_silver_count = c(NA, NA, NA),
			loan_count = c(NA, NA, NA),
			pct_gold_silver = c(min(portfolios[,4]), min(portfolios[,4]), max(portfolios[,4]))
			)
		)
port_plot <- ggplot(portfoliosx, aes(pct_gold_silver, profit ))
port_plot + geom_point(data = port_current, colour = "red", size = 4) +  stat_chull(fill = 'green', alpha=1/5, colour = NA) + 
			scale_y_continuous(labels = scales::dollar) + scale_x_continuous(labels = scales::percent) + 
			xlab('Percentage of Loans in Tiers 2 and 3 (i.e., highest-impact)') + ylab("Net Portfolio Profit or Loss (i.e., subsidy required)")


#  Redo of above plotting, using different 
# portfoliosx <- portfolios  # saving 'portfolios' for posterity, since I keep running/tweaking the loop that generates it
	portfoliosy <- rbind(portfoliosx,
		data.frame(    # Top left, bottom left, bottom right
			profit = c(max(portfoliosx[,1]), min(portfoliosx[,1])-5e5, min(portfoliosx[,1])-5e5),  # y axis
			gold_silver_count = c(NA, NA, NA),
			loan_count = c(NA, NA, NA),
			pct_gold_silver = c(0, 0, max(portfoliosx[,4]))            # x axis
			)
		)

	portfoliosy$profit <- portfoliosy$profit*1e6
	port_currenty <- port_current
	port_currenty$profit <- port_currenty$profit*1e6

	port_plot <- ggplot(portfoliosy, aes(pct_gold_silver, profit ))
	port_plot + geom_jitter(alpha=1/10) + geom_point(data = port_currenty, colour = "red", size = 4) +  stat_chull(fill = 'green', alpha=1/5, colour = 'red') + 
				scale_y_continuous(labels = scales::dollar, limits = c(-5e6, 0)) + scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
				xlab('Percentage of Loans in Tiers 2 and 3 (i.e., highest-impact)') + ylab("Net Portfolio Profit or Loss (i.e., subsidy required) (millions)") +
				theme_grey(base_size = 10) 

# # Remove the last three added rows in portfoliosy (these three rows were added to get the area chart to go to the axis intersection and origin, to fill out the graph)
# 		portfoliosz <- portfoliosy[1:685281,]

# # Replot with tigher focus on points
# 	port_plot <- ggplot(portfoliosz, aes(pct_gold_silver, profit ))
# 	port_plot + geom_jitter(alpha=1/10, width=0.01) + geom_point(data = port_currenty, colour = "red", size = 4) +  stat_chull(fill = 'green', alpha=1/5, colour = 'red') + 
# 				scale_y_continuous(labels = scales::dollar, limits = c(-5, 0)) + scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
# 				xlab('Percentage of Loans in Tiers 2 and 3 (i.e., highest-impact)') + ylab("Net Portfolio Profit or Loss (i.e., subsidy required) (millions)") +
# 				theme_grey(base_size = 10)


