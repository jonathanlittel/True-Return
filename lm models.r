
source('# merge impact scores.r')

	# create a dataframe to work with
		# impact <- as.data.frame(cbind(df.rap.inactive$Final.Impact.Rating,df.rap.inactive$Marginal_Revenue,
		# 	df.rap.inactive$TotalAssets_log, df.rap.inactive$pd))
		impact_all <- select(df.rap.inactive, Final.Impact.Rating, Marginal_Revenue, TotalAssets_log, pd,					# Everything
			Loan.Additionality.POINT, Average.Poverty.Level.of.Country.or.Region.POINT, Env.Degradation.Hotspot.POINT,
			Climate.Change.Hotspot.POINT, Climate.Change.Mitigation, Climate.Change.Adaptation.Support,
			Social.Impact.POINT..Max.2.,   Total.Producers.Artisans.Employees.POINT, Highest_Impact_Loans)   # , Highest_Impact_Loans

		to_faina <- select(df.rap, LoanID, Final.Impact.Rating, 				# Everything
			Loan.Additionality.POINT, Average.Poverty.Level.of.Country.or.Region.POINT, Env.Degradation.Hotspot.POINT,
			Climate.Change.Hotspot.POINT, Climate.Change.Mitigation, Climate.Change.Adaptation.Support,
			Social.Impact.POINT..Max.2.,   Total.Producers.Artisans.Employees.POINT,
			Highest_Impact_Loans, Marginal_Revenue, TotalAssets_log, pd, LGD, Average_Balance, Months_outstanding_to_date, 
			Yield, balance_1215, Marginal_Revenue, Interest_Expense,
			Risk.Rating.Category, Highest_Impact_Loans)
			write.csv(to_faina, 'marginal_revenue_Q4_2015.csv')


		impact.inactive.reg.factor <- select(df.rap.inactive, Final.Impact.Rating, Marginal_Revenue, TotalAssets_log, pd,
			Yield,
			Loan.Additionality.POINT, Average.Poverty.Level.of.Country.or.Region.POINT, Env.Degradation.Hotspot.POINT,
			Climate.Change.Hotspot.POINT, Climate.Change.Mitigation, Climate.Change.Adaptation.Support,
			Social.Impact.POINT..Max.2.,   Total.Producers.Artisans.Employees.POINT,
			Highest_Impact_Loans_factor)

		impact.inactive <- select(df.rap.inactive, Final.Impact.Rating, Marginal_Revenue, TotalAssets_log,										# All impact
			Loan.Additionality.POINT, Average.Poverty.Level.of.Country.or.Region.POINT, Env.Degradation.Hotspot.POINT,			# inactive loans only
			Climate.Change.Hotspot.POINT, Climate.Change.Mitigation, Climate.Change.Adaptation.Support,
			Social.Impact.POINT..Max.2.,   Total.Producers.Artisans.Employees.POINT,
			Highest_Impact_Loans)


		impact_complete <- impact_all[complete.cases(impact_all),]

		# colnames(impact) <- c('Impact_Rating', 'Marginal Revenue','TotalAssets_log', 'pd')





	# Write output correlation matrix
		cor <- cor(impact_complete, method='spearman')
		write.csv(cor, 'correlation_matrix_rap_data.csv')
		file.show('correlation_matrix_rap_data.csv')
		cor.matrix <- stargazer(cor, title='Correlation Matrix', type='html')

		write(cor.matrix, file = "rap_impact_score_models.html",  append = FALSE)

	# Produce a few models and save an output file
		# Note that models 1 and 2 only have about 175 obs, because they exclude active loans
		# Model 1 
			lm <- lm(Marginal_Revenue ~ Final.Impact.Rating + TotalAssets_log, data=impact.inactive.reg.factor)
			summary(lm)

			model1 <- stargazer(lm, type = "html", title='Impact Rating on Profitability',
                     ci = TRUE, keep.stat=c('n',"adj.rsq"))

			write(model1, file = "rap_impact_score_models.html",  append = FALSE)
		# Model 2
			lm2 <- lm(Marginal_Revenue ~ Highest_Impact_Loans_factor + TotalAssets_log, data=impact.inactive.reg.factor)
			summary(lm2)

			model2 <- stargazer(lm2, type = "html", title='High Impact on Profitability',
                     ci = TRUE, keep.stat=c('n',"adj.rsq"))

			write(model2, file = "rap_impact_score_models.html",  append = TRUE)
		# Model 3
			lm3 <- lm(pd ~ Highest_Impact_Loans_factor + TotalAssets_log, data=df.rap)
			summary(lm3)
			model3 <- stargazer(lm3, type = "html", title='High Impact on probability of default',
                     ci = TRUE, keep.stat=c('n',"adj.rsq"))

			write(model3, file = "rap_impact_score_models.html",  append = TRUE)			

		# Model 4
			lm4 <- lm(Yield ~ Highest_Impact_Loans_factor + TotalAssets_log, data=impact.inactive.reg.factor)
			summary(lm4)
		




	plot(impact)
	plot(df.rap.inactive$Yield - df.rap.inactive$Interest_Expense - (LGD_no_guarantee * df.rap.inactive$pd))						#
		abline(h=0, col='green')
	plot(( df.rap.inactive$Yield - df.rap.inactive$Interest_Expense - (LGD_no_guarantee * df.rap.inactive$pd)) * df.rap.inactive$Average_Balance, type='p' )   #
	abline(h=0, col='green')
	plot(df.rap.inactive$Yield - (df.rap.inactive$pd * 0.9))

	# example - splom for impact sub things

												# 1a
	splom(impact, lm=TRUE)

		corr_1 <- select(df.rap, Final.Impact.Rating, Marginal_Revenue, pd,					# Everything
			Loan.Additionality.POINT, Average.Poverty.Level.of.Country.or.Region.POINT, Env.Degradation.Hotspot.POINT,
			Climate.Change.Hotspot.POINT, Climate.Change.Mitigation, Climate.Change.Adaptation.Support,
			Social.Impact.POINT..Max.2.,   Total.Producers.Artisans.Employees.POINT,
			Highest_Impact_Loans)

		corr_2 <- select(df.rap, 				# Just impact
			Loan.Additionality.POINT, Average.Poverty.Level.of.Country.or.Region.POINT, Env.Degradation.Hotspot.POINT,
			Climate.Change.Hotspot.POINT, Climate.Change.Mitigation, Climate.Change.Adaptation.Support,
			Social.Impact.POINT..Max.2.,   Total.Producers.Artisans.Employees.POINT,
			Highest_Impact_Loans)




	chart.Correlation(corr_2, cex.axis=3, cex.labels=4, cex.main=4, method='spearman')											# 1b
	#c
	lm1 <- lm(df.rap$Final.Impact.Rating ~ df.rap$Marginal_Revenue)

	lm2 <- lm(df.rap$Final.Impact.Rating ~ df.rap$Marginal_Revenue + df.rap$TotalAssets_log)

	regCols <- c("Marginal_Revenue", "Yield", "pd", 'Final.Impact.Rating', 'TotalAssets_log', 'Sales_log', 'Months_outstanding_to_date',
		'Average_Balance', 'Internal.Interest.Rate....')
	df.reg <- df.rap[,names(df.rap) %in% regCols]  # can subset for active / non WO here
	chart.Correlation(df.reg)												# 2

# Predict marginal revenue
	lm3 <- lm(df.reg$Marginal_Revenue ~ df.reg$Average_Balance + df.reg$Months_outstanding_to_date 
		+ df.reg$Internal.Interest.Rate.... )

# Break down Marginal revenue		
	lm4 <- lm(df.reg$Final.Impact.Rating <- df.reg$pd + df.reg$Yield)
	summary(lm4)

	lm5 <- lm(df.reg$Final.Impact.Rating ~ df.reg$pd + df.reg$Yield + df.reg$TotalAssets_log))

# Plot  
	plot( df.rap$Average_Balance, df.rap$Yield - df.rap$Interest_Expense - (LGD_no_guarantee * df.rap$pd))	#
	abline(h=0, col='green')


mean(na.omit(df.rap$Final.Impact.Rating))
mean(na.omit(df.rap$Marginal_Revenue))

plot(sort(df.rap$Yield - df.rap$pd * LGD_no_guarantee) )									# 3



# Always profitable to do a loan if marginal revenue exceeds marginal cost
# need to do a certain amount of volume to cover fixed costs


		df.reg <- df.reg[complete.cases(df.reg),]
		cor(df.reg)
		lm.a <- lm(df.reg$pd ~ df.reg$Final.Impact.Rating + df.reg$Sales_log))

summary(lm.a)

	regCols <- c("Marginal_Revenue", "Yield", "pd", 'Final.Impact.Rating', 'TotalAssets_log', 'Sales_log', 'Months_outstanding_to_date',
		'Average_Balance', 'Internal.Interest.Rate....')
	df.reg.inactive <- count[,names(count) %in% regCols]  # can subset for active / non WO here

		df.reg.inactive <- df.reg.inactive[complete.cases(df.reg),]


		dim(df.reg.inactive)

		# get 462 loans with complete cases
		# play


# Box plot of marginal revenue vs impact score
	library(ggplot2)
	df.boxplot <- impact.inactive.reg.factor[!is.na(impact.inactive.reg.factor$Highest_Impact_Loans),]
	p <- ggplot(df.boxplot, aes(factor(Highest_Impact_Loans_factor), Marginal_Revenue))
	p + geom_boxplot()