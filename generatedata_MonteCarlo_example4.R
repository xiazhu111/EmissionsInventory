install.packages("irtoys")
library(irtoys)

generate_data <- function(nitem, nexaminee, seed) {
  #Set the seed
  set.seed(seed)
  
  #generate item parameters
  itempar <- cbind(
  rnorm(nitem, mean = 1.13, sd = 0.25)*1.702, #a
  rnorm(nitem, mean = 0.21, sd = 0.51)*1.702, #b
  rnorm(nitem, mean = 0.16, sd = 0.05))       #c
  
  #Generate ability parameters
  ability <- rnorm(nexaminee, mean = 0, sd = 1)
  
  #Generate response data according to the 3PL model
  respdata <- irtoys::sim(ip = itempar, x = ability)
  colnames(respdata) <- paste0("item", 1:nitem) 
  
  #Combine the generated values in a list
  data <- list(itempar = itempar, 
               ability = ability, 
               seed = seed, 
               respdata = respdata) 
  
  #Return the simulated data
  return(data)
}
