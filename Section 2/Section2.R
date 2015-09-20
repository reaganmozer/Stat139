#######################
# Section 2, Stat 139 #
#  Hypothesis Tests   #
#         +           #
#  Permutation Tests  #
#      9/17/15        #
#    Reagan Rose      #
#######################

# Question 2: Crabs data #
setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code and Data")
crabs <- read.csv("crabs.csv")
head(crabs)

# Part a: compute summary statistics
summary(crabs$weight)

# Part b: boxplots
# Let's change the names so the plot is pretty
crabs[crabs$col==0,]$col = "Light"
crabs[crabs$col==1,]$col = "Dark"

# Now make the plot
boxplot(crabs$weight~crabs$col, 
        main="Weight of Light v. Dark Crabs", xlab="Color", ylab="Weight")

# Part c: correlation between color and weight
crabs <- read.csv("crabs.csv")
cor(crabs$weight, crabs$col)

# Part d: Permutation test

# First lets look at the difference in means
attach(crabs)

ybardiff.obs <- mean(crabs[crabs$col==1,]$weight) - mean(crabs[crabs$col==0,]$weight)
t.test(crabs$weight ~ crabs$col, var.equal=T)

# Now do the permutation test
nsim = 10000
ybardiff.sim = rep(NA, nsim)

for (i in 1:nsim){
  col.sim <- sample(crabs$col)
  ybardiff.sim[i] <- mean(weight[col.sim==1]) - mean(weight[col.sim==0])
}

hist(ybardiff.sim)
abline(v=ybardiff.obs, col="red")

# one sided p-value
mean(ybardiff.sim >= ybardiff.obs)
length(which(ybardiff.sim >= ybardiff.obs )) / nsim

# two sided p-value
mean(abs(ybardiff.sim) >= abs(ybardiff.obs))
length(which(abs(ybardiff.sim) >= abs(ybardiff.obs))) / nsim


# Question 3: Reddit Data#
setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code and Data")
reddit <- read.csv("reddit_sub.csv")
attach(reddit)
head(reddit)

summary(number_of_upvotes[sub==0])
summary(number_of_upvotes[sub==1])
boxplot(number_of_upvotes~sub) #this doesn't look so good

# Hypothesis test
t.test(number_of_upvotes~sub)
t.test(number_of_upvotes~sub, alternative=c("less"))

# Permutation test
ybardiff.obs = mean(number_of_upvotes[sub==1]) - mean(number_of_upvotes[sub==0])

nsim = 1000
ybardiff.sim = rep(NA, nsim)

for (i in 1:nsim){
  sub.sim = sample(sub)
  ybardiff.sim[i] = mean(number_of_upvotes[sub.sim==1]) - mean(number_of_upvotes[sub.sim==0])
}

hist(ybardiff.sim)
abline(v=ybardiff.obs, col="red")
