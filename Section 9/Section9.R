setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code and Data/")


# Problem 1
data = read.csv("varieties.csv")
attach(data)

# For y1
plot(x,y1)
hist(x)
hist(y1)
plot(log(x), y1
fit1 = lm(y1~x,data)
fit1a = lm(y1~log(x), data)

plot(x, log(y1))
fit1b = lm(log(y1)~x, data)

xaxis = seq(0,50,0.1)
line1a = (fit1a$coefficients[1] + fit1a$coefficients[2] * log(xaxis))
line1b = exp(fit1b$coefficients[1] + fit1b$coefficients[2] * xaxis)
plot(x,y1)
abline(fit1, col="green", lwd=2)
lines(xaxis, line1a, col="blue", lwd=2)
lines(xaxis, line1b, col="red", lwd=2)

# The blue line is best, so we'll use model fit1a
plot(fit1a$fitted.values, fit1a$residuals)
abline(h=0, lty=2, col="red", lwd=2)

qqnorm(fit1a$residuals)
qqline(fit1a$residuals)

# For y2
plot(x,y2)
fit2 = lm(y2~x, data)
fit2a = lm((1/y2)~x, data)
xnew = 1/x
fit2b = lm(y2~xnew, data)

xaxis = seq(0,50,0.1)
line2a = 1/(fit2a$coefficients[1] + fit2a$coefficients[2] * xaxis)
line2b = fit2b$coefficients[1] + fit2b$coefficients[2] * (1/xaxis)
plot(x,y2)
abline(fit2, col="green", lwd=2)
lines(xaxis, line2a, col="blue", lwd=2)
lines(xaxis, line2b, col="red", lwd=2)

# Red line is best so we'll use fit2b
plot(fit2b$fitted.values, fit2b$residuals)
abline(h=0, lwd=2, lty=2, col="red")

# For y3
plot(x, y3)
fit3 = lm(y3~x, data)
fit3a = lm(sqrt(y3)~x, data)
fit3b = lm(y3~sqrt(x),data)

xaxis = seq(0,50,0.1)
line3a = (fit3a$coefficients[1] + fit3a$coefficients[2]*xaxis)^2
line3b = fit3a$coefficients[1] + fit3a$coefficients[2]*(xaxis^2)

abline(fit3, col="green", lwd=2)
lines(xaxis, line3a, col="blue", lwd=2)
lines(xaxis, line3b, col="red", lwd=2)

# blue line is best so we'll use fit3a
plot(fit3a$fitted.values, fit3a$residuals)
abline(h=0, col="red", lwd=2, lty=2)

qqnorm(fit3a$residuals)
qqline(fit3a$residuals)

# Problem 2
data = read.csv("height.csv")
data = data[,-c(1)] # just getting rid of the annoying X column
attach(data)

# First we should check out whether there's any relationships here
plot(data)
# Based on these plots, what do you expect to find?

# Now let's assess the assumptions
# First we should check whether the outcome is normal
hist(child_height)

# Now we should check whether the predictors are normal
hist(dad_height)
hist(mom_height)
par(mfrow=c(1,2))
hist(child_height[female==0])
hist(child_height[female==1])
par(mfrow=c(1,1))

# Check for outliers
boxplot(mom_height) # should we remove these outliers?
max(mom_height)
c(floor(max(mom_height)/12), max(mom_height)%%12)
boxplot(dad_height) # what about this one?
min(dad_height)
c(floor(min(dad_height)/12), min(dad_height)%%12)
boxplot(child_height~female)
min(child_height[female==0])

# A dad who is 4 foot 1 inch is probably not generalizable - let's remove him
outlier_index = which(dad_height == min(dad_height))
data = data[-c(outlier_index),]
attach(data)

# And check the linear relationship assumption (though we kind of already did)
plot(child_height~mom_height)
plot(child_height~dad_height)
plot(child_height~female) #does this make sense?

# Now lets go ahead and run the regression
model = lm(child_height~., data)
summary(model)

# Create the residual plots
plot(model$residuals~model$fitted.values)
abline(h=0, col="red", lwd=2)

qqnorm(model$residuals)
qqline(model$residuals)

# Predict height for a female with 5'0 mom and 6'0 dad
newdata = data.frame(mom_height=60, dad_height=72, female=1)
predict(model, newdata, interval=c("confidence"))
predict(model, newdata, interval=c("prediction"))


# Problem 3
# Let's first make up an outcome variable we're interested in
n = 100
age = rnorm(n,mean=20, sd=1)
gpa = rnorm(n, mean = 3.2, sd = .25) + .1*age

# Now let's make up predictor variables that have NOTHING to do with GPA
cash_in_pocket = rnegbin(n, mu=20, theta=1/2)
cowboys_fan = c(rep(1,30), rep(0, 70))
fav_number = floor(runif(n,0,100))
phone_charge = round(runif(n,0,1),2)

fakedata = data.frame(gpa,age,cash_in_pocket,cowboys_fan,fav_number, phone_charge)
attach(fakedata)
head(fakedata)
# Run with just one predictorr
model1 = lm(gpa~age, data=fakedata)
summary(model1)

# Two predictors
model2 = lm(gpa~age+cash_in_pocket, data=fakedata)
summary(model2)

# All predictors
fakemodel = lm(gpa~., data=fakedata)
summary(fakemodel)

# Notice that the R^2 goes up every time. Adjusted R^2 is better.


### APPENDIX ###
# This is how the VARIETIES.CSV dataset was created
n = 300
x = runif(n,1,50)
y1 = 3 + log(x) + rnorm(n)
plot(x,y1)

y2 = 3 + 13/x + rnorm(n)
plot(x, y2)

y3 = 5 + 2*x^2 + rnorm(n,0,200)
plot(x, y3)

y4 = x + rnorm(n)
plot(x, y4)

y5 = 7 + 20/x + rnorm(n)
plot(x,y5)

data <- data.frame(x,y1,y2,y3,y4,y5)
write.csv(data, "varieties.csv")

# This is how the HEIGHT.CSV dataset was created
n = 166
mom_height = rnorm(n, mean=64, sd=3)
dad_height = rnorm(n, mean=70, sd=5)
female = c(rep(1,83), rep(0,83))
child_height = 17 + 0.3*mom_height + 0.5*dad_height - 5*female + rnorm(166, sd=1)

height_data = data.frame(child_height,mom_height,dad_height,female)
write.csv(height_data, "height.csv")





