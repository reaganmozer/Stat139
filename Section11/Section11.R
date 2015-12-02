setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code and Data/")
data = read.csv("insurance_claims.csv")
attach(data)

#------- PRELIMINARY CHECKS (EDA) -------#
# Check to see which variables are "factors" (categorical)
# This is REALLY important
lapply(data, is.factor)

# check the outcome for normality
hist(claim_amount)
hist(log(claim_amount)) # this looks better
plot(log(claim_amount)~coverage) #but this doesn't look right

hist(coverage)
hist(log(coverage))
plot(log(claim_amount)~log(coverage)) #this is more linear (notice the pseudo-Simpsons?)

#------- QUESTION 1 -------#
# Run the full interaction model
model1 = lm(log(claim_amount)~log(coverage)*claim_type, data=data)
summary(model1)

# Calculate the slopes for each of the groups using Model 1
slope_contamination = model1$coefficients[2]
slope_fire = sum(model1$coefficients[c(2,7)])
slope_theft = sum(model1$coefficients[c(2,8)])
slope_water = sum(model1$coefficients[c(2,9)])
slope_wind = sum(model1$coefficients[c(2,10)])

c(slope_contamination, slope_fire, slope_theft, slope_water,slope_wind)

# Test slope_fire vs. slope_theft
# H0: b1+b6 = b1+b7
n = dim(data)[1]
k = length(model1$coefficients)
C = rep(0,k)
C[c(2,7)]=1
betas = model1$coefficients
gamma_hat = t(C)%*%betas
gamma = slope_theft
sigma_hat = 0.4229
claim_type=as.integer(claim_type)
X = as.matrix(data.frame(x0=rep(1,n),log(coverage),(claim_type==2), (claim_type==3),(claim_type==4), (claim_type==5),
                         (claim_type==2)*log(coverage), (claim_type==3)*log(coverage),(claim_type==4)*log(coverage), (claim_type==5)*log(coverage)))

T_obs = (gamma_hat-gamma) / (sigma_hat*sqrt(t(C)%*%solve(t(X)%*%X)%*%C))


# An easier way to do this is using the vcov function!
cov = vcov(model1)
T_obs = (gamma_hat-gamma) / sqrt(t(C)%*%cov%*%C)
k = length(betas)
df = n-(k+1)
2*(1-pt(abs(T_obs), df)) #not significant


#Extra sum of squares
# Part (c)
claim_type=as.factor(claim_type)
model2 = lm(log(claim_amount)~log(coverage), data=data)
anova(model1,model2) #so Model 1 is significantly better than Model 2

# Part (d)
model3 = lm(log(claim_amount)~log(coverage)+claim_type,data=data)
anova(model1,model3) # Model 1 is NOT significantly better than Model 3

# Make an intercept-only model
model0 = lm(log(claim_amount)~1, data=data)

# AIC
SSR = c()
SSR[1] = anova(model0)$Sum
SSR[2] = anova(model1)$Sum[4]
SSR[3] = anova(model2)$Sum[2]
SSR[4] = anova(model3)$Sum[3]

p = c(1,10,2,6) #number of parameters in each model
AIC_vals = n*log(SSR/n) + 2*p

# Let's check
extractAIC(model0)
extractAIC(model1)
extractAIC(model2)
extractAIC(model3)

#------- QUESTION 2 -------#
# Stepwise Regression and Cross Validation
library(MASS)

# Forward
step_forward = step(model0, scope=list(upper=model1), direction="forward")
summary(step_forward)
step_forward$anova

# Backward
step_backward = step(model1, scope=list(lower=model0), direction="backward")
summary(step_backward)
step_backward$anova

# Stepwise
step_both = step(model1, scope=list(lower=model0, upper=model1), direction="both")
summary(step_both)
step_both$anova

# Cross Validation
library(cvTools)
set.seed(1234)
cv_assignment = cvFolds(n, K=5, type="random")
cvFit(model0,data=data,K=5,R=1,y=log(data$claim_amount),foldtype=c("random"))
cvFit(model1,data=data,K=5,R=1,y=log(data$claim_amount),foldtype=c("random"))
cvFit(model2,data=data,K=5,R=1,y=log(data$claim_amount),foldtype=c("random"))
cvFit(model3,data=data,K=5,R=1,y=log(data$claim_amount),foldtype=c("random"))

#Conclusion: All of these methods suggest that Model 3 is the best choice.

#------- QUESTION 3 -------#
# Dataset 1 - perfectly uncorrelated
x1 = c(2,2,2,2,4,4,4,4)
x2 = c(5,5,7,7,5,5,7,7)
y = c(52,43,49,46,50,48,44,40)
data1 = data.frame(y,x1,x2)
cor(data1$x1,data1$x2)

model.11 = lm(y~x1, data=data1)
model.12 = lm(y~x2, data=data1)
model.13 = lm(y~., data=data1) #Notice the coefficents stay EXACTLY the same


# Dataset 2 - nearly uncorrelated (the usual case)
data2 = read.table("bloodpress.txt", header=T)
cor(data2)
# Stress and BSA are basically uncorrelated (r=0.018)

model.21 = lm(BP~Stress, data=data2)
model.22 = lm(BP~BSA, data=data2)
model.23 = lm(BP~Stress+BSA, data=data2) #Coefficients change a LITTLE

# Dataset 3 - almost perfectly correlated
cor(data2) 
# weight and BSA are strongly correlated

model.31 = lm(BP~Weight, data=data2)
model.32 = lm(BP~BSA, data=data2)
model.33 = lm(BP~Weight+BSA, data=data2) #Coefficient for BSA is DRASTICALLY different


