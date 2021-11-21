set.seed(1234)
rnorm(1)

x = rnorm(30, 500, 30)
#let’s sample randomly from the distribution, calculate the mean, and then repeat this process many times
means = replicate(n = 1000, expr = { 
  x_i = sample(x, length(x), replace = T)
  mean(x_i)
})
#what happens after this? ... 