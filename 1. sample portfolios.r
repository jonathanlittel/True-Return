# first process and merge data
	source('0. merge impact scores.r')

# or use these two columns
  wd <- paste("C:/Box Sync/jlittel/",
          "True Return/data",
          sep="")
  	setwd(wd)
  	filename <- 'True Return dataset for Jon 07.11.16.csv'
  	filename <- 'True Return dataset for Jon.csv'
	df <- read.csv(filename)
	port_sim <- select(df, 
			profit = Expected.Value.of.Net.Loan.Income,
			impact_score = Impact.Rating...10.Point.Scale)
	# using older dataset
	port_sim <- select(df, 
			profit = profit,
			impact_score = Final.Impact.Score)

	# port_sim <- port_sim[,-1]
	# drop NA rows
	port_sim <- port_sim[complete.cases(port_sim),]
	# port_sim <- data.frame(
	# 	profit = port_sim$profit,
	# 	impact_score = port_sim$impact_score)
	# names(port_sim) <- c('profit', 'impact_score')
# Function to plot the convex hull of a set of points
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

# function to simulate portfolios
#-----------------------------------------------
simulate_portfolio <- function(x, y, n = 50, p = 0.1, cut = 3.25, weighted = TRUE, x_weight = 1, y_weight = 1) {
	n <- n
	df <- cbind(x, y)
	df <- df[complete.cases(df),]
	df <- data.frame(df)
	names(df) <- c('x', 'y')
	df <- df %>%
			# mutate(weight = log(i) ^ 2 ) %>% # option tweak to weighting
			# create a normalized val for weight for x and y
			mutate(
				normalized_x = (x - min(x)) / (max(x) - min(x)),
				normalized_y = (y - min(y)) / (max(y) - min(y))
				) 
	loan_count <- rep(NA, n)
	portfolios <- c(0,0)
	min_loans <- dim(df)[1]
	cut <- cut
	# begin loop
	for (i in 1:n) {
		set.seed(i)
		# add_index <- createDataPartition(df[,1], times=1, p= p , list=FALSE)
		# check[i] <- sum(add_index)
		# df_plus <- rbind(df, df[add_index,])
		df_plus <- rbind(
			df,
			sample_n(df,
				size = 25,
				weight = if (weighted) {
					    		(1 + normalized_x)^x_weight + (1 + normalized_y)^y_weight
					    		} else {
					    			NULL 
					    		})
					    )	
		# n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio
		loan_count[i] <- min_loans

		portfolios <- rbind(portfolios, 
				df_plus %>% 
					# # mutate(weight = log(i) ^ 2 ) %>% # option tweak to weighting
					# # create a normalized val for weight for x and y
					# mutate(
					# 	normalized_x = (x - min(x)) / (max(x) - min(x)),
					# 	normalized_y = (y - min(y)) / (max(y) - min(y))
					# 	) %>%
				    sample_n(min_loans, 
				    	weight = if (weighted) {
				    		(1 + normalized_x)^x_weight + (1 + normalized_y)^y_weight
				    		} else {
				    			NULL 
				    		}) %>% 
				    # summarise_each(funs(mean)) %>%
				    summarise(
				    	total_profit = sum(x),
				    	impact_score_above_cut = sum(y>cut),
				    	avg_impact_score = mean(y))
		    )
	}
	portfolios[-1,]
}	

# create portfolios 
  # cut above 3.25
num_sims <- 0
  portfolios1  <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 1e4, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 1.1, y_weight = 2)
  portfolios1a <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 1e4, p = 0.101, cut = 3.25, weighted = FALSE)
  portfolios1b <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 1e4, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 2.5, y_weight = 1.2)
  portfolios1c <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 1e4, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 1.2, y_weight = 3.5)
  portfolios1d <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 3, y_weight = 3)
  portfolios1e <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 3, y_weight = 3)
  portfolios1f <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 10, y_weight = 10)
  portfolios1g <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 15, y_weight = 10)
  portfolios1h <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 15, y_weight = 10)
  portfolios1i <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 15, y_weight = 1)
  portfolios1j <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 2, y_weight = 4)
  portfolios1z <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 1e4, p = 0.101, cut = 3.25, weighted = TRUE, x_weight = 4, y_weight = 2)


num_sims <- 1e4
  # cut above 6.25
  # portfolios2  <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 5e3, p = 0.101, cut = 6.25, weighted = TRUE)
  portfolios2a <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = FALSE)
  portfolios2b <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = TRUE, x_weight = 2, y_weight = 2)
  # portfolios2c <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = TRUE, x_weight = 1, y_weight = 3)
  # portfolios2d <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = TRUE, x_weight = 1, y_weight = 4)
  # portfolios2e <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = TRUE, x_weight = 2, y_weight = 1)
  # portfolios2f <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = FALSE)
  portfolios2g <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = TRUE, x_weight = 1.5, y_weight = 2)
  portfolios2h <- simulate_portfolio(port_sim[,1], port_sim[,2], n = num_sims, p = 0.101, cut = 6.25, weighted = TRUE, x_weight = 3, y_weight = 2)

quantile(port_sim[,1])