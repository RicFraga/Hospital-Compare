# This file contains functions to analyze and plot data from the
# care measures.csv

# This function plots an histogram of the 30-day death rates from heart attack
# (column 11 in the outcome dataset)

plot_30_day_mr <- function(data) {
     
     # Getting the data to plot
     day_30 <- as.numeric(data[, 11])
     
     # Plotting the data
     hist(day_30, xlab = 'Day', main = '30-day mortality rates for heart attack')
}