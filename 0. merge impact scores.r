library(dplyr)  require(dplyr)
library(tidyr)
library(ggplot2)
library(caret) # for createDataPartition and createResample
library(scales) # for $ in plot
options (scipen = 99, digits = 2)

	# Read and merge data
		  wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
		            "Working Folders for RAP Modules/Risk Profile/PD Model/",
		            "3.Outputs",
		            sep="")
		  setwd(wd)
		  filename <-  "pds_06.22.16.csv"                       ###### <--- update with next quarter
		  df.rap <- read.csv(filename, header=TRUE, sep=",")
		  # rap <- subset(df.rap, last_year==1)
		  wd <- paste("C:/Box Sync/jlittel/",
		              "True Return/data",
		              sep="")
		  setwd(wd)

	# # Read and merge impact data and portfolio revenue (option 1/2)
		# filename <- 'impact_scores.csv'
		# impact_data <- read.csv(filename, header=TRUE, sep=",")
		# names(impact_data)[1] <- "LoanID"
		# impact_data <- rename(impact_data, Highest_Impact_Loans_factor = X.Highest.impact..loans)

	# Use rev data from Adaptive 
	  filename <- 'portfolio_revenue_rap_Q4_2015.csv'
		rev_data <- read.csv(filename, head=TRUE, sep=",")
		keep_cols <- c('LoanID', 'Average_Balance', 'Portfolio_Revenue', 'Months_outstanding_to_date', 'Yield', 'Annualized_Yield')
		rev_data <- rev_data[,names(rev_data) %in% keep_cols]
		# rev_data <- apply(rev_data, 2, function(x)  as.numeric(x))
		rev_data$Average_Balance <- as.numeric(as.character(rev_data$Average_Balance))
		rev_data$Portfolio_Revenue <- as.numeric(as.character(rev_data$Portfolio_Revenue))

	# Use rev / Profitability (option 2/2)
		filename <- 'True Return dataset for Jon.csv'
		imp_data <- read.csv(filename, header=TRUE, sep=",")
		names(imp_data) <- c('LoanID', 'Final.Impact.Score', 'profit')
		# rename
		imp_data <- rename(imp_data, impact_score = Final.Impact.Score)
		# # imp_data <- imp_data %>%
		# #   mutate(profit=replace(profit, profit=="#N/A", NA))
		# imp_data$profit <- as.numeric(imp_data$profit)


	# Merge
		df.rap <- merge(x=df.rap, y=rev_data, by=c("LoanID"), all.x=TRUE)
		df.rap <- merge(x=df.rap, y=imp_data, by=c("LoanID"), all.x=TRUE)

		  wd <- paste("C:/Box Sync/jlittel/",
		              "True Return/",
		              sep="")
		  setwd(wd)


	# Data cleaning
		# names(df.rap)[names(df.rap=='Internal.Interest.Rate....')] <- "Internal_Interest_Rate"
		# Rename some names with operators in name
		# df.rap <- rename(df.rap, Internal_Interest_Rate = Internal.Interest.Rate....)
	# # Change Highest_Impact_Loans to numeric
	# 	# df.rap$Highest_Impact_Loans <- as.numeric(levels(df.rap$Highest_Impact_Loans))[df.rap$Highest_Impact_Loans]
	# 	# df.rap$Highest_Impact_Loans <- as.numeric(as.character(df.rap$X.Highest.impact..loans))
	# 	df.rap$Highest_Impact_Loans <- ifelse(df.rap$Highest_Impact_Loans_factor=="Bronze", 3, 
	# 		ifelse(df.rap$Highest_Impact_Loans_factor=="Silver", 2, 
	# 			ifelse(df.rap$Highest_Impact_Loans_factor=="Gold", 1,
	# 				NA)))
	# # Make dummy for 'gold' loans	
	# 	df.rap$gold_silver <- ifelse(df.rap$Highest_Impact_Loans==3, 0, 1)
	# 	df.rap$gold <- ifelse(df.rap$Highest_Impact_Loans==1, 1, 0)


    #############################################################
	# Add interest expense, marginal opex, and loss given default
	# calculate marginal revenue
	# Interest expense is 2.4% over life of loan, opex is flat $XXX per loan,  yield is actual yield over actual average balance 
												# note, should subset for loans that went to nonaccrual
												# df.rap$Internal.Interest.Rate.... != 0
		df.rap$interest_expense <- df.rap$tenor_years_min1 * 0.025
		# df.rap$Interest_Expense <- df.rap$interest_exp_assum * df.rap$Months_outstanding_to_date / 12
		df.rap$Marginal_Opex <- 0 / df.rap$Average_Balance   # somewhere in the range of $1-2k, could be relative to pd (workouts)
		df.rap$LGD <- 0.90
		# optional: add assumption if loans are guaranteed
		# rap.active$LGD <- 0
		# rap.active$LGD <- ifelse(rap.active$Guarantee_pct<.01, LGD_no_guarantee, (1 - rap.active$Guarantee_pct))
		df.rap$Marginal_Revenue <- df.rap$Yield - df.rap$interest_expense - df.rap$Marginal_Opex - (df.rap$LGD * df.rap$pd)
		plot(sort(df.rap$Marginal_Revenue))

	# Subset to exclude active loans and those that went on non-accrual
		df.rap.inactive <- filter(df.rap, active==0)  # , Internal_Interest_Rate >0
		prop.table(table(df.rap.inactive$Marginal_Revenue>0))
		prop.table(table((df.rap.inactive$Yield - (df.rap.inactive$pd* 0.9))>0))


# compare short term and long term net_rev (actual revenue / average balance, less pd)
# note that whill underestimate, since actual rev will include loss
	df.rap.inactive$net_rev <- df.rap.inactive$Yield - df.rap.inactive$pd
	df.rap.inactive$st <- df.rap.inactive$Tenor_years<=1
	g <- ggplot(df.rap.inactive, aes(x=st, y=net_rev))
	g + geom_boxplot()