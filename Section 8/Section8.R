#######################
# Section 8, Stat 139 #
#     Regression      #
#      11/04/15       #
#    Reagan Rose      #
#######################

#---------------#
# A Toy Example #
#---------------#

# If we simulate some data that is linearly dependent, will regression pick it up?
# Let's say that age and income are perfectly related
n = 100
age_years = rnorm(n, mean=50, sd=15)
income = 1000 + 50*age_years

plot(age_years, income)
model = lm(income~age_years)
abline(model, col="red")
summary(model)

# In real life, things are never "perfectly related", so let's add some noise
income_noise = 1000 + 50*age_years +rnorm(100, mean=100, sd=100)
plot(age_years, income_noise)
model2 = lm(income_noise~age_years)
abline(model2, col="red")
summary(model2)

#----------------#
# A Real Example #
#----------------#

setwd("~/Desktop/Harvard/Teaching/Stat 139/R Code and Data")
data = read.csv("eharmony.csv")
attach(data)

head(data)
plot(data) # this will show you all possible scatterplots

plot(matches~attractiveness)
model1 = lm(matches~attractiveness)
abline(model1)
summary(model1)

plot(matches~photos)
model2 = lm(matches~photos)
abline(model2)
summary(model2)

plot(matches~age)
model3 = lm(matches~age)
abline(model3)
summary(model3)

plot(ss_agerangemax~ss_agerangemin)

# Let's just work with matches and age
hist(age)
hist(matches) # doesn't look normal, could transform, but we wont for this example
              # so we should be aware results may be invalid


model4 = lm(matches~age)
summary(model4)

plot(matches~age)
abline(model4, col="red")

beta0 = model4$coefficients[1] #use this to pull out the coefficients from your model
beta1 = model4$coefficients[2]

# What is the effect of age on number of matches
beta1

# Find a 95% confidence interval for beta1
confint(model4)

# Using our model, how many matches do you think a 60 year old will get?
beta0 + beta1*60

# Find a 95% confidence interval for number of matches given age = 60
new = data.frame(age=60)
predict(model4, new, interval="confidence")

# Find a 95% prediction interval for number of matches for a 60 year old
predict(model4, new, interval="prediction")
