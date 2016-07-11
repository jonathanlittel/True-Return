# 1. Find efficient frontier

        d <- select(df.rap.inactive, Marginal_Revenue, Final.Impact.Rating, weight=balance_1215+1)
        d <- d[complete.cases(d),]
        # create y, the distance from the origin
 		d$y <- sqrt(d[,1]^2 + d[,2]^2)
 		s <- sum(d$weight)
 		d$y_weighted <- d$y * d$weight / s

 		# sort descending on the weighted y, for incremental portfolio building 
 		d <- arrange(d, desc(y_weighted), weight)
 		d$y_weighted <-NULL
 		rm(s)
 		# portfolios_2 <- data.frame(imp=NULL, mr=NULL)
 		l <- 1 # rep(1, nrow(d))
 		# make placeholder df 
 		p <- data.frame(weighted_y=l, weighted_imp=l, weighted_mr=l)

 		# Create a data frame with a row for each combo, with the weighted y, impact score, and marginal revenue
 		for (i in 1:nrow(d)) {  
 			d$w <- NULL
 			d$weight_ratio <- 0
 			d$weighted_y <- 0
 			d$w <- sum(d$weight[1:i]) # sum the weights for i rows
 			d$weight_ratio[i] <- d$weight[i] / d$w[i]
 			d$weighted_y[i] <- d$y[i] * d$weight_ratio[i]
 			d$weighted_imp <- d$Final.Impact.Rating * d$weight_ratio
 			d$weighted_mr <- d$Marginal_Revenue * d$weight_ratio

 			weighted_y <- sum(d$weighted_y[1:i])
 			weighted_imp <- sum(d$weighted_imp[1:i])
 			weighted_mr <- sum(d$weighted_mr[1:i])
 			r <- cbind(weighted_y, weighted_imp, weighted_mr)
 			p <- rbind(p, r, make.row.names=FALSE)
 			# p$weighted_y[i] <- sum(d$weighted_y[1:i])
 			# # p$weighted_imp[i] <- sum(d$Final.Impact.Rating[1:i] *  d$weight_ratio[1:i])
 			# # p$weighted_mr[i] <- sum(d$Marginal_Revenue[1:i] *  d$weight_ratio[1:i])
 		}
 		p <- data.frame(p[-1,])

plot(p$weighted_imp,p$weighted_mr, type='l')

# This is a plot of each possible portfolio, where you incrementally add the next loan that is closest to the efficient frontier.
# One problem is that the scale of the impact score is different, so it gets more weight in choosing the 'next optimal' loan.


# 1. Find efficient frontier

        d <- select(df.rap, profit, Final.Impact.Rating, weight=rep(1,nrow(df.rap)))
        d <- d[complete.cases(d),]
        # create y, the distance from the origin
 		d$y <- sqrt(d[,1]^2 + d[,2]^2)
 		s <- sum(d$weight)
 		d$y_weighted <- d$y * d$weight / w

 		# sort descending on the weighted y, for incremental portfolio building 
 		d <- arrange(d, desc(y_weighted), weight)
 		d$y_weighted <-NULL
 		rm(s)
 		# portfolios_2 <- data.frame(imp=NULL, mr=NULL)
 		l <- 1 # rep(1, nrow(d))
 		# make placeholder df 
 		p <- data.frame(weighted_y=l, weighted_imp=l, weighted_mr=l)

 		# Create a data frame with a row for each combo, with the weighted y, impact score, and marginal revenue
 		for (i in 1:nrow(d)) {  
 			d$w <- NULL
 			d$weight_ratio <- 0
 			d$weighted_y <- 0
 			d$w <- sum(d$weight[1:i]) # sum the weights for i rows
 			d$weight_ratio[i] <- d$weight[i] / d$w[i]
 			d$weighted_y[i] <- d$y[i] * d$weight_ratio[i]
 			d$weighted_imp <- d$Final.Impact.Rating * d$weight_ratio
 			d$weighted_mr <- d$Marginal_Revenue * d$weight_ratio

 			weighted_y <- sum(d$weighted_y[1:i])
 			weighted_imp <- sum(d$weighted_imp[1:i])
 			weighted_mr <- sum(d$weighted_mr[1:i])
 			r <- cbind(weighted_y, weighted_imp, weighted_mr)
 			p <- rbind(p, r, make.row.names=FALSE)
 			# p$weighted_y[i] <- sum(d$weighted_y[1:i])
 			# # p$weighted_imp[i] <- sum(d$Final.Impact.Rating[1:i] *  d$weight_ratio[1:i])
 			# # p$weighted_mr[i] <- sum(d$Marginal_Revenue[1:i] *  d$weight_ratio[1:i])
 		}
 		p <- data.frame(p[-1,])

plot(p$weighted_imp,p$weighted_mr, type='l')
