install.packages("MonteCarlo")
library(MonteCarlo)
library(dplyr)
library(ggplot2)
#defining the inventory function
inventory <- function(...) {
  #sample from the pdfs
  ...
  #do the calculations
  ...
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_list = list(...) #number of arguments matches that of func
#run the actual Monte Carlo simulations
MC_result <- MonteCarlo(func = inventory, nrep = 1000, param_list = param_list)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df <- MakeFrame(MC_result)
head(df)
tbl <- tbl_df(df)
p <- ggplot(tbl, aes(x=sum)) 
  + geom_histogram(binwidth = 1, color = "black", fill="white")
  + labs(title = "Histogram of inventory sums",x="emissions (KT)",y="Frequency")
p
#calculate mean of sum using the plyr package
library(plyr)
mu <- ddply(df, summarise, grp.mean = mean(sum))
head(mu)
p + geom_vline(data=mu, aes(xintercept=grp.mean), linetype = "dashed") #Add mean lines
  + theme(legend.position="top") #change legend position

#calculating 95% CIs for the histogram
intervals <- t.test(df$sum,conf.level = 0.95) #stats package
low <- results$conf.int[1]
high <- results$conf.int[2]

