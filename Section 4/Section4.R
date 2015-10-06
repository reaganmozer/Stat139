#######################
# Section 4, Stat 139 #
#    Assumptions      #
#         +           #
#   Transformations   #
#         +           #
#   Rank-sum test     #
#      9/30/15        #
#    Reagan Rose      #
#######################

# Let's simulate some data to see how the t-test
# fails when we violate the normality assumption
carrier <- rep(c(0,1), c(1000,1000))
y = c()
y[carrier==0] <- 100 + 0.01*rnorm(1000) # this one is normal
shift.down <- seq(-10,0, by = 10/499)
shift.up <- seq(0,100, by = 100/499)
y[carrier==1] <- 100 + c(shift.down, shift.up) # this one is not

hist(y)
plot(density(y))

par(mfrow=c(1,2))
plot(density(y[carrier==0]))
plot(density(y[carrier==1]))
par(mfrow=c(1,1))

t.test(y[carrier==0], y[carrier==1]) #t-test flags as significant

# Let's see if the ranked-sum test can get it right
t.obs <- sum(rank(y)[carrier==0])

nsim = 1000
t.sim = rep(NA, nsim)

for (i in 1:nsim){
  carrier.sim <- sample(carrier)
  t.sim[i] = sum(rank(y)[carrier.sim==0])
}

hist(t.sim)
abline(v=t.obs, col="red")
mean(abs(t.sim) <= abs(t.obs))

wilcox.test(y[carrier==0], y[carrier==1])

# Another example where the data is skewed by outliers
carrier <- rep(c(0,1), c(10,20))
y = c(rnorm(25), 3.9, 7.1, 5.6, 3.1, 7.5)
boxplot(y~carrier)

t.test(y[carrier==0], y[carrier==1])
wilcox.test(y[carrier==0], y[carrier==1])

# Coffee data
starbucks <- c(8.5,9.48,8.65,8.16,8.83,7.76,8.63)
dd <- c(8.27, 8.20, 8.25, 8.14, 9, 8.10, 7.20, 8.32, 7.70)
data <- c(starbucks, dd)
label <- c(rep(0, length(starbucks)), rep(1, length(dd)))

t.obs <- sum(rank(data)[label==0])
wilcox.test(data[label==0], data[label==1])


# Reddit data
data = read.csv("reddit_sub.csv")
names(data)
attach(data)

t.test(number_of_upvotes[sub==0], number_of_upvotes[sub==1])

summary(number_of_upvotes[sub==0])
summary(number_of_upvotes[sub==1])

hist(number_of_upvotes[sub==0])
hist(number_of_upvotes[sub==1])

qqnorm(number_of_upvotes[sub==0])
qqnorm(number_of_upvotes[sub==1])

hist(sqrt(number_of_upvotes[sub==0]))
hist(sqrt(number_of_upvotes[sub==1]))

hist(log(number_of_upvotes[sub==0]+1))
hist(log(number_of_upvotes[sub==1]+1))

qqnorm(log(number_of_upvotes[sub==0]+1))
qqnorm(log(number_of_upvotes[sub==1]+1))

t.test(log(number_of_upvotes[sub==0]+1), log(number_of_upvotes[sub==1]+1))

# Wine data 
data = read.csv("winedata.csv")
data = data[1:100,]
names(data)
attach(data)

t.obs <- sum(rank(alcohol)[quality=="good"])

n.sim = 1000
t.sim = rep(NA, n.sim)
for (i in 1:n.sim){
  quality.sim = sample(data$quality)
  t.sim[i] = sum(rank(alcohol)[quality.sim=="good"])
}

hist(t.sim)
abline(v=t.obs, col="red", lwd=2)
mean(t.sim >= t.obs) # one-sided p-value

t.test(data[data$quality=="good", ]$alcohol, data[data$quality=="not good",]$alcohol)



