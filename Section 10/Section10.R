# Create the data
Y = c(1,2)
X = matrix(c(1,1,2.5, 1.1), byrow=F, ncol=2)
data = data.frame(y=Y, x=X)
names(data) = c("y", "x0", "x1")
attach(data)

# Solve using univariate formulas
beta_1_hat = sum((x1-mean(x1))*(y-mean(y))) / sum((x1-mean(x1))^2)
beta_0_hat = mean(y)-beta_1_hat*mean(x1)

# Solve using LM
model = lm(y~x1, data)
summary(model)

# Solve using matrix multiplication
beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y


# Question 3
setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code and Data/")
data = read.csv("height.csv")
data = data[,-c(1)] # just getting rid of the annoying X column
attach(data)

n = dim(data)[1]
X = data.frame(x0=rep(1,n), x1=data$mom_height, x2=data$dad_height,x3=data$female)
Y = data.frame(y=data$child_height)

X = as.matrix(X)
Y = as.matrix(Y)

beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y

model2 = lm(data$child_height~.*., data)
sigma_2_hat = 1.007
vcov(model2)
sigma_2_hat * solve(t(X)%*%X)

std_errors = sqrt(diag(vcov(model2)))
t_stats = as.vector(beta_hat) / std_errors

p_vals = 2*(1-pt(abs(t_stats), 162))
