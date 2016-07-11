# first process and merge data
	source('0. merge impact scores.r')

# or use these two columns
  wd <- paste("C:/Box Sync/jlittel/",
          "True Return/data",
          sep="")
  	setwd(wd)
  	filename <- 'True Return dataset for Jon 07.11.16.csv'
	df <- read.csv(filename)
	port_sim <- df


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
simulate_portfolio <- function(x, y, n = 50, p = 0.1, cut = 3.25, weighted = TRUE) {
	n <- n
	df <- cbind(x, y)
	df <- df[complete.cases(df),]
	df <- data.frame(df)
	names(df) <- c('x', 'y')
	loan_count <- rep(NA, n)
	portfolios <- c(0,0)
	min_loans <- dim(df)[1]
	cut <- cut
	# begin loop
	for (i in 1:n) {
		set.seed(i)
		add_index <- createDataPartition(df[,1], times=1, p= p , list=FALSE)
		check[i] <- sum(add_index)
		df_plus <- rbind(df, df[add_index,])
		# n <- sample(min_loans:nrow(port_sim),1,replace=FALSE) # generate single random n for number of rows in portfolio
		loan_count[i] <- min_loans

		portfolios <- rbind(portfolios, 
				df_plus %>% 
					mutate(weight = (i ^ 1.5 ) / i ) %>%
					# create a normalized weight for x, and add ~ 4.5 to put it around the impact score
					mutate(normalized = (x - min(x)) / (max(x) - min(x))) %>%
				    sample_n(min_loans, 
				    	weight = if (weighted) {
				    		( y^weight + (normalized * 10 )^weight)
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
portfolios1 <- simulate_portfolio(port_sim[,1], port_sim[,2], n = 50, p = 0.101, cut = 3.25, weighted = TRUE)