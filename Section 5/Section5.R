#######################
# Section 5, Stat 139 #
#       Power         #
#         +           #
# Multiple Comparison #
#      10/7/15        #
#    Reagan Rose      #
#######################

# Exercise 1
mu0 = 3.35
sigma = 0.15
n = 25
alpha = 0.05
z_alpha = abs(qnorm(1-(alpha/2)))
rr_low = mu0 - z_alpha*(sigma/sqrt(n)) #rejection region low
rr_high = mu0 + z_alpha*(sigma/sqrt(n))

mu_true = 3.3
power = pnorm(rr_low, mu_true, (sigma/sqrt(n))) + (1-pnorm(rr_high, mu_true, (sigma/sqrt(n))))
power

alpha = 0.05
beta = 0.2
z_alpha = abs(qnorm(1-(alpha/2)))
z_beta = abs(qnorm(1-beta))

n = ((sigma*(z_alpha+z_beta))/(mu_true-mu0))^2
n


# Exercise 2

# Part 1 
# Probability of at least one significant
alpha = 0.05
K = 20

1 - (1-alpha)^K

# Part 2
# Bonferroni correction value
correction = choose(20, 1)
alpha_star = alpha / correction

# Part 3 (i)
# Simulate 100 samples from N(0,1)
# Test mu = 0 independently

pvals = c()
nsim = 1000

for (i in 1:nsim){
  x = rnorm(100)
  pvals[i] = t.test(x=x, mu=0)$p.value
}

mean(pvals < 0.05) # Proportion that were flagged as significant
#should look like alpha

# Part 3 (ii)
# Simulate for all 20 genes at once

n = K
pvals = matrix(NA, nrow=nsim, ncol = n)

for (i in 1:nsim){
  X = replicate(n=n, expr=rnorm(100))
  pvals[i,] = apply(X=X, MARGIN=2, FUN = function(x){t.test(x=x,mu=0)$p.value})
}

any_significant = c()
for (i in 1:nsim){
  any_significant[i] = any(pvals[i,] < 0.05)
}
mean(any_significant) #should look like Part (i)

# Part 3 (iii)
# Now use the Bonferroni correction

any_significant_corrected = c()
for (i in 1:nsim){
  any_significant_corrected[i] = any(pvals[i,] < alpha_star)
}
mean(any_significant_corrected) # should look like alpha
