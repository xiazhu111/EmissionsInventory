#Monte Carlo Simulation in R Example
#from https://www.programmingr.com/monte-carlo-simulation-in-r/

min(runif(1,3000,5000),runif(1,2000,4000),runif(1,150,200)*30)
min(runif(1,3000,5000),runif(1,2000,4000),runif(1,150,200)*30)
min(runif(1,3000,5000),runif(1,2000,4000),runif(1,150,200)*30)

#if we want to run more iterations, e.g. 1000 iterations
#to build a larger vector of results
#full Monte Carlo Simulator in R
results = NULL
for (k in 1:1000) #iterate 1000 times 
{
  rolls = runif(1,3000,5000)
  bags = runif(1,2000,4000)
  cases = runif(1,150,200)*30
  total = min(rolls, bags, cases)
  results = rbind(results, data.frame(rolls, bags, cases, total))
}
head(results)
summary(results) # from the summary, we can see that the bagger is the constraint (least rolls per hour)
#The speed of the overall manufacturing line is limited to the speed of putting the rolls into bags
summary(results$total) #prints horizontally
summary(results['total']) #prints vertically 

#the triangular distribution
install.packages("EnvStats")
library(EnvStats)
rtri(50, 0, 1, 1/2) #'50' is the number of samples drawn from the distribution
d <- rnorm(1,2,3)
d
