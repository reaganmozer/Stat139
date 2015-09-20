# Suppose we are looking at whether being a carrier of a certain
# gene is associated with an outcome (like height, weight, etc.)

# Let's look at two cases. One where it IS associated and one where its NOT.

# Simulate data for both cases with N=300
carrier <- rep(c(0,1), c(100,200))
y.null <- rnorm(300)  # Assuming NO association
y.alt <- rnorm(300, mean=carrier/2) # Assuming association

# Test whether there is a difference between carriers and non-carriers in both cases
t.test(y.null~carrier, var.equal=T)
t.test(y.alt~carrier, var.equal=T)

# Look at difference in means between carriers and non-carriers in both cases
null.diff <- mean(y.null[carrier==1]) - mean(y.null[carrier==0])
alt.diff <- mean(y.alt[carrier==1]) - mean(y.alt[carrier==0])

nsim = 1000
ybardiff_null.sim = rep(NA, nsim)
ybardiff_alt.sim = rep(NA, nsim)

for (i in 1:nsim){
  carrier.sim = sample(carrier)
  ybardiff_null.sim[i] = mean(y.null[carrier.sim==1]) - mean(y.null[carrier.sim==0])
  ybardiff_alt.sim[i] = mean(y.alt[carrier.sim==1]) - mean(y.alt[carrier.sim==0])
}

# Look at no association case
hist(ybardiff_null.sim)
abline(v=null.diff, lwd=2, col="red")
mean(abs(ybardiff_null.sim) > abs(null.diff))

# Look at association case
hist(ybardiff_alt.sim, xlim=c(-0.6,0.6))
abline(v=alt.diff, lwd=2, col="red")
mean(abs(ybardiff_alt.sim) > abs(alt.diff))
