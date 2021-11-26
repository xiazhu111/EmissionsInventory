install.packages("MonteCarlo")
install.packages("EnvStats")
library(MonteCarlo)
library(EnvStats)
library(dplyr)
library(ggplot2)
#used value from Steph's code 
set.seed(1421) 
#defining the inventory function
inventory <- function(n) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n,2530,4382,3578)
  house_shedding <- runif(n,0.01,0.04)
  littering_rate <- rnorm(n,12656,14271) 
  turf_shedding <- runif(n,0.01,0.04)
  proportion_foam <- runif(n,0.058,0.15)
  pellet_loss <- runif(n,0.0001,0.0004)
  loadperwash <- runif(n,3,4)
  shedding_laundry <- runif(n,124,308)
  loadperdry <- rnorm(n,0.438,0.017)
  shedding_dryer <- rnorm(n,18,8)
  tire_shedding <- rtri(n,0.05,0.25,0.1)
  fishinggear_loss <- runif(n,0.01,0.1)
  litres_paint <- runif(n,2,3) 
  #do the calculations
  sum <- (1112929*exterior_surface_area/100*0.45359237*0.2*house_shedding) #source 1: house paint shedding
    + (littering_rate) #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
    + (235.9202662*0.835*0.50) #source 3: road paint shedding
    + (54*27000*(6.5/1000)*turf_shedding) #source 4: shedding from artificial turf
    + (95.9971663589714*proportion_foam*3.4) #source 5: foam snow during construction 
    + (4667*0.0357*pellet_loss*1000) #source 6: pellet spills from plastic industry
    + (135233*278/10^9) #source 7: shedding of tire dust from airplanes
    + (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
    + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
    + (1179057*1.3*16000*tire_shedding)/10^9 #source 10: wearing of vehicle tires
    + (109668*1435.974459546*fishinggear_loss/10^6) #source 11: derelict fishing gear
    + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_list = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
MC_result <- MonteCarlo(func = inventory, nrep = 100000, param_list = param_list)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df <- MakeFrame(MC_result)
head(df)
tbl <- tbl_df(df) #a subclass of data.frame; tibbles are the central data structure for the set of packages known as the tidyverse
p <- ggplot(tbl, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of inventory sums",x="emissions (T)",y="Frequency")
p
#calculate mean of sum using the plyr package
library(plyr)
mu <- ddply(df, summarise, grp.mean = mean(sum))
head(mu)

#calculating 95% CIs for the histogram
intervals <- t.test(df$sum,conf.level = 0.95) #stats package
low <- results$conf.int[1]
high <- results$conf.int[2]
low
high

#making histogram of sum, and adding vertical means for mean and 95% CI
p + geom_vline(xintercept=c(mean(sum),low,high),color="blue",linetype="dashed",size=1) + theme(legend.position="top") #change legend position
summary(df$sum)
summary(tbl$sum)
