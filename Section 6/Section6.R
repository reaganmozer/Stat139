#######################
# Section 6, Stat 139 #
#       ANOVA         #
#      10/14/15       #
#    Reagan Rose      #
#######################

# Exercise 1
n = c(31,31,14)
N = sum(n)
mean = c(25.8,22.68,21.29)
sd = c(2.56, 3.67, 2.76)

allmean = mean(mean)
SSB = sum(n*(mean-allmean)^2)
SSW = sum((n-1)*sd^2)
dfB = length(n)-1
dfW = N - length(n)
F_obs = (SSB/dfB)/(SSW/dfW)

1 - pf(F_obs, dfB, dfW) #p-value

# Contrasts for midsize versus big
a = c(1,-.5,-.5)
T_num = sum(a*mean)
Sp2 = SSW/ (sum(n)-length(n))
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))

T = T_num / T_denom
df = sum(n)-length(n)
p.val = 2*(1-pt(T,df))

# Contrasts for SUV versus pickup
a = c(0,1,-1)
T_num = sum(a*mean)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))

T = T_num / T_denom
df = sum(n)-length(n)
p.val = 2*(1-pt(T,df))

# t-test
var.pooled = ((n[2]-1)*sd[2]^2 + (n[3]-1)*sd[3]^2)/(n[2]+n[3]-2)
sd.pooled = sqrt(var.pooled)
t.obs = (mean[2]-mean[3])/(sd.pooled*sqrt((1/n[2])+(1/n[3])))

df.t = (n[2]+n[3]-2)
2*(1-pt(t.obs,df.t))


# Exercise 2
setwd("~/Desktop/Stat139/Section 6")
data = read.csv("insurance_claims.csv")
head(data)
attach(data)

summary(claim_type)
summary(claim_amount)

# Figure out the correct way to transform the data
hist(claim_amount)
hist(log(claim_amount))
boxplot(log(claim_amount)~fraudulent)
boxplot(log(claim_amount)~claim_type)

# Prove to yourself the the ANOVA for two groups is equivalent to the t-test
model0= aov(log(claim_amount)~fraudulent)
summary(model0)

t.test(log(claim_amount)~fraudulent, var.equal=T)
t.test(log(claim_amount)~fraudulent, var.equal=T)$statistic^2

# Conduct one-way ANOVA using the different claim types as groups
model1 = aov(log(claim_amount)~claim_type)
summary(model1)

# Two way ANOVA with claim types and fraudulence as groups
model2 = aov(log(claim_amount)~claim_type+fraudulent)
summary(model2)


# Something cool we'll talk about next week
model3 = aov(log(claim_amount)~townsize)
summary(model3)

model4 = aov(log(claim_amount)~claim_type+townsize)
summary(model4)

model5 = aov(log(claim_amount)~claim_type*townsize)
summary(model5)
