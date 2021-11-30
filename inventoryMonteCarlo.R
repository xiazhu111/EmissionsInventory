install.packages("MonteCarlo")
install.packages("EnvStats")
library(MonteCarlo)
library(EnvStats)
library(dplyr)
library(ggplot2)
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
#used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
set.seed(1234)
#run the actual Monte Carlo simulations
MC_result <- MonteCarlo(func = inventory, nrep = 100000, param_list = param_list)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df <- MakeFrame(MC_result)
head(df)
#tbl <- tbl_df(df) #a subclass of data.frame; tibbles are the central data structure for the set of packages known as the tidyverse
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of inventory sums",x="emissions (T)",y="Frequency")
#making histogram of sum, and adding vertical means for mean and 95% CI
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
#+ theme(legend.position="top") #change legend position

#calculating 95% CIs for the histogram
#intervals <- t.test(df$sum,conf.level = 0.95) #stats package
#low <- results$conf.int[1]
#high <- results$conf.int[2]
#low
#high #I keep getting NULL from these
#Try other code from http://pages.stat.wisc.edu/~yandell/st571/R/append7.pdf 
norm.interval = function(data, variance = var(data), conf.level = 0.95)
{
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z*sdx, xbar + z*sdx)
}
norm.interval(df$sum)
summary(df$sum)
#summary(tbl$sum) #should be same as above

#calculate mean of sum using the plyr package; didn't use
#library(plyr)
#mu <- ddply(df, summarise, grp.mean = mean(sum))
#head(mu)
#
#
#
#
#
#Monte Carlo simulations for subgroups of sources
###########################################1. terrestrial#############################################
#defining the terrestrial function
terrestrial <- function(n_terrestrial) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n_terrestrial,2530,4382,3578)
  house_shedding <- runif(n_terrestrial,0.01,0.04)
  littering_rate <- rnorm(n_terrestrial,12656,14271) 
  turf_shedding <- runif(n_terrestrial,0.01,0.04)
  proportion_foam <- runif(n_terrestrial,0.058,0.15)
  pellet_loss <- runif(n_terrestrial,0.0001,0.0004)
  loadperwash <- runif(n_terrestrial,3,4)
  shedding_laundry <- runif(n_terrestrial,124,308)
  loadperdry <- rnorm(n_terrestrial,0.438,0.017)
  shedding_dryer <- rnorm(n_terrestrial,18,8)
  tire_shedding <- rtri(n_terrestrial,0.05,0.25,0.1)
  #do the calculations
  sum_terrestrial <- (1112929*exterior_surface_area/100*0.45359237*0.2*house_shedding) #source 1: house paint shedding
  + (littering_rate) #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
  + (235.9202662*0.835*0.50) #source 3: road paint shedding
  + (54*27000*(6.5/1000)*turf_shedding) #source 4: shedding from artificial turf
  + (95.9971663589714*proportion_foam*3.4) #source 5: foam snow during construction 
  + (4667*0.0357*pellet_loss*1000) #source 6: pellet spills from plastic industry
  + (135233*278/10^9) #source 7: shedding of tire dust from airplanes
  + (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
  + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
  + (1179057*1.3*16000*tire_shedding)/10^9 #source 10: wearing of vehicle tires
  #return result
  return(list("sum"=sum_terrestrial))
}
#add uncertainty pdf parameters to the list
param_terrestrial = list("n_terrestrial" = 1) #number of arguments matches that of func
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
#run the actual Monte Carlo simulations
MC_terrestrial <- MonteCarlo(func = terrestrial, nrep = 1000, param_list = param_terrestrial)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_terrestrial <- MakeFrame(MC_terrestrial)
head(df_terrestrial)
p <- ggplot(df_terrestrial, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of terrestrial sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_terrestrial$sum),color="blue",linetype="dashed",size=1) 
summary(df_terrestrial$sum)
norm.interval(df_terrestrial$sum)
#
#
#
#
#
#
###########################################2. maritime#############################################
#defining the maritime function
maritime <- function(n) {
  #sample from the pdfs
  fishinggear_loss <- runif(n,0.01,0.1)
  litres_paint <- runif(n,2,3) 
  #do the calculations
  sum <- (109668*1435.974459546*fishinggear_loss/10^6) #source 11: derelict fishing gear
  + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_maritime = list("n" = 1) #number of arguments matches that of func
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
#run the actual Monte Carlo simulations
MC_maritime <- MonteCarlo(func = maritime, nrep = 1000, param_list = param_maritime)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_maritime <- MakeFrame(MC_maritime)
head(df_maritime)
p <- ggplot(df_maritime, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of maritime sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_maritime$sum),color="blue",linetype="dashed",size=1) 
summary(df_maritime$sum)
norm.interval(df_maritime$sum)
#
#
#
#
#
#
#
###########################################3. paint#############################################
#defining the paint function
paint <- function(n) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n,2530,4382,3578)
  house_shedding <- runif(n,0.01,0.04)
  litres_paint <- runif(n,2,3) 
  #do the calculations
  sum <- (1112929*exterior_surface_area/100*0.45359237*0.2*house_shedding) #source 1: house paint shedding
  + (235.9202662*0.835*0.50) #source 3: road paint shedding
  + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_paint = list("n" = 1) #number of arguments matches that of func
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
#run the actual Monte Carlo simulations
MC_paint <- MonteCarlo(func = paint, nrep = 1000, param_list = param_paint)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_paint <- MakeFrame(MC_paint)
head(df_paint)
p <- ggplot(df_paint, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of paint sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_paint$sum),color="blue",linetype="dashed",size=1) 
summary(df_paint$sum)
norm.interval(df_paint$sum)
#
#
#
#
#
#
#
###########################################4. tires#############################################
#defining the tires function
tires <- function(n) {
  #sample from the pdfs
  tire_shedding <- rtri(n,0.05,0.25,0.1)
  #do the calculations
  sum <- (135233*278/(10^9)) #source 7: shedding of tire dust from airplanes
  + (1179057*1.3*16000*tire_shedding)/(10^9) #source 10: wearing of vehicle tires
  #return result
  return(list("sum"=sum))
}
rtri(1,0.05,0.25,0.1)
#add uncertainty pdf parameters to the list
param_tires = list("n" = 1) #number of arguments matches that of func
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
#run the actual Monte Carlo simulations
MC_tires <- MonteCarlo(func = tires, nrep = 1000, param_list = param_tires)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_tires <- MakeFrame(MC_tires)
head(df_tires)
p <- ggplot(df_tires, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_tires$sum),color="blue",linetype="dashed",size=1) 
summary(df_tires$sum)
norm.interval(df_tires$sum)
#
#
#
#
#
#
###########################################5. clothes#############################################
#defining the clothes function
clothes <- function(n) {
  #sample from the pdfs
  loadperwash <- runif(n,3,4)
  shedding_laundry <- runif(n,124,308)
  loadperdry <- rnorm(n,0.438,0.017)
  shedding_dryer <- rnorm(n,18,8)
  #do the calculations
  sum <- (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
  + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_clothes = list("n" = 1) #number of arguments matches that of func
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
#run the actual Monte Carlo simulations
MC_clothes <- MonteCarlo(func = clothes, nrep = 1000, param_list = param_clothes)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_clothes <- MakeFrame(MC_clothes)
head(df_clothes)
p <- ggplot(df_clothes, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_clothes$sum),color="blue",linetype="dashed",size=1) 
summary(df_clothes$sum)
norm.interval(df_clothes$sum)
#end