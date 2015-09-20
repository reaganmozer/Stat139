#########################
#  Section 3, Stat 139  #
#  Confidence Intervals #
#       9/23/15         #
#     Reagan Rose       #
#########################

# Simulate 1,000 samples of a n=25 dataset from N(0,1)
nsim = 1000
nsamp = 25

lower_bound = c()
upper_bound = c()
xbar = c()

for (i in 1:nsim){
  data = rnorm(nsamp)
  xbar[i] = mean(data)
  s = sum((data-xbar[i])^2)/(nsamp-1)
  critical_val = qnorm(c(0.975)) # should be about 1.96
  lower_bound[i] = xbar[i] - critical_val * (s/sqrt(nsamp))  
  upper_bound[i] = xbar[i] + critical_val * (s/sqrt(nsamp)) 
}

lengthCI = upper_bound-lower_bound
library(plotrix)
plotCI(1:nsim, xbar, lengthCI)

num_missed_zero = length(which(lower_bound > 0 | upper_bound < 0))
1 - (num_missed_zero/nsim) # Percentage of intervals that captured the true mean 
