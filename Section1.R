#######################
# Section 1, Stat 139 #
#     Intro to R      #
#      9/9/15         #
#    Reagan Rose      #
#######################

# This is a comment!

### Common mathematical operations ###
exp(2)

sin(pi/6)

log(10); log(10, base=10) 

sqrt(16)

abs(3-7)

round(pi,0); round(pi, 1); round(pi,4)

### Matrices ###
matrix(c(1,2,3,4,5,6), byrow=T, nrow=3)
A <- matrix(c(1,2,3,4,5,6), byrow=T, nrow=3) # name the matrix A
B <- matrix(c(7,8,9,10,11,12), byrow=T, ncol=2)
t(A) # transpose
A*B # multiply the numbers in the matrix
t(A) %*% B # multiply the actual matrices
A[1:2,]
solve(A[1:2,])

### Syntax ###

# Variables and computation
A <- matrix(c(1,2,3,4,5,6), byrow=T, nrow=3)
A
x = c(1,0,0)
x

t(A) %*% x

y <- c(2,3,3)
x + y

x <- 1:10
y <- seq(1,10)
z <- seq(1,10,0.1)
w <- rep(1,10)

# Control Flow and Loops

for (i in 1:10){
  print (i)
}

x <- 9001
if (x > 9000){
  print("It's over 9,000!")
} else{ 
  print("Nope!")
}

if (x > 10000){
  x * 2
} else if (x > 9000 & x <= 10000) {
  print ("But it's not over 10,000!")
} else {
  print("Weak!")
}

### Inputting Data ###
setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code")
crabs <- read.csv("crabs.csv")

head(crabs) # check out the dataset, default is 6 obs
head(crabs, n=10)
tail(crabs)

crabs <- read.table("crabs.dat", header=T)
names(crabs)
summary(crabs)

### Summary Statistics and Regression ###
crabs$width
crabs[,3]

x <- crabs$width

mean(x)
sd(x)
var(x)
length(x)
median(x)

summary(x)

y <- crabs$satell
cor(x,y)

# We can subset to crabs of one color
crabs3 <- subset(crabs, crabs$color == 3)
head(crabs3)

crabs3.2 <- crabs[crabs$color == 3,]


### Regression ###
fit <- lm(y ~ x)
summary(fit)

### Graphics and Plotting ###
hist(crabs$width)
hist(crabs$width, breaks=25)

plot(crabs$width, crabs$satell)
plot(crabs$satell~crabs$width)

# Adding lines to your plot
fit <- lm(crabs$satell, crabs$width)
plot(crabs$width, crabs$satell)
abline(fit)

# the "v" means vertical line
# lty indicates the texture of the line
# lwd is the weight of the line (higher numbers = wider line)
plot(crabs$width, crabs$satell, main="Our First Plot")
abline(v=mean(crabs$width), lty=2) 
abline(v=mean(crabs$width) + 2 * sd(crabs$width), lty=3) 
abline(v=mean(crabs$width) - 2 * sd(crabs$width), lty=3)
abline(h=max(crabs$satell), lty=3, col="blue", lwd=2)

# Want to see what colors are available?
colors()


# Tables and Barplots
table(crabs$color)
table(crabs$color, crabs$spine)
barplot(table(crabs$satell), name="$ of Satellites")

# Scatterplot and Correlation Matrix
attach(crabs) 
pairs(~width+satell+weight)

cor(crabs)
cor(crabs[,3:5])

install.packages("corrplot")
library("corrplot")

M <- cor(crabs[,3:5])
corrplot(M)


### More Advanced Examples ###
data <- read.csv("winedata.csv", header=T)
head(data)
names(data)
summary(data)

attach(data)

# Calculate the mean of all continuous variables at once
lapply(data[,1:11], mean)

# Let's do some exploratory analysis
cor(alcohol, as.numeric(quality))

M <- cor(data[,c(1, 4, 5, 10:11)])
corrplot(M)

# Let's run a logistic regression to see which variables
# are predictors of wine quality
is.factor(data$quality) # this should be TRUE

lr <- glm(quality~., data=data, family="binomial")
summary(lr)

confint(lr)

data$quality = as.numeric(data$quality)
attach(data)
goodwines = data[quality==1,]
badwines = data[quality==2,]

# Is there significantly different alcohol content in the good wines?
t.test(goodwines$alcohol, badwines$alcohol, alternative=c("two.sided"))

# Is there significantly more alcohol in the good wines?
t.test(goodwines$alcohol, badwines$alcohol, alternative=c("greater"))
