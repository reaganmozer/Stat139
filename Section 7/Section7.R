#######################
# Section 7, Stat 139 #
#       ANOVA II      #
#      10/22/15       #
#    Reagan Rose      #
#######################

#### EXERCISE 1 ####
group1 = c(4,3.5,5,3,2)
group2 = c(3,2,0,2,2)
group3 = c(1,0,1,2,1)
means = c(mean(group1), mean(group2), mean(group3))
sds = c(sd(group1),sd(group2),sd(group3))
n=c(5,5,5)

vals = c(group1,group2,group3)
groups = c(rep(1,5), rep(2,5), rep(3,5))

# Run the ANOVA
data = data.frame(vals,groups)
model = aov(data$vals~factor(data$groups))
summary(model)

# Pairwise t-tests
t.test(group1,group2, pooled.var=T)$p.value*3
t.test(group1,group3, pooled.var=T)$p.value*3
t.test(group2,group3, pooled.var=T)$p.value*3

# Contrast tests
Sp2 = sum((n-1)*sds^2)/ (sum(n)-length(n))
df = sum(n)-length(n)

#Testing group 1 versus group 2
a = c(1,-1,0)
T_num = sum(a*means)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))
T = T_num / T_denom
p.val = 2*(1-pt(T,df))

p.val*3

#Testing group 1 versus group 3
a = c(1,0,-1)
T_num = sum(a*means)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))
T = T_num / T_denom
p.val = 2*(1-pt(T,df))

p.val*3

#Testing group 2 versus group 3
a = c(0,1,-1)
T_num = sum(a*means)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))
T = T_num / T_denom
p.val = 2*(1-pt(T,df))

p.val*3

# Tukey's method
TukeyHSD(model) # the easiest way

# the harder way
numgroups = 3
nobs = 15
nbar = mean(n)
alpha = 0.05

q = qtukey(1-alpha,numgroups,nobs - numgroups)/sqrt(2)
alpha_tukey=2*(1-pt(q,df=nobs-numgroups))

# APPROXIMATION to the Tukey
t.test(group1,group2,pooled.var=T)$p.value
t.test(group1,group3,pooled.var=T)$p.value
t.test(group2,group3,pooled.var=T)$p.value

# the hardest way
nbar = mean(n)
nobs = 15
I = 3

t_obs1 = (means[1]-means[2])/sqrt(Sp2/nbar)
t_obs2 = (means[1]-means[3])/sqrt(Sp2/nbar)
t_obs3 = (means[2]-means[3])/sqrt(Sp2/nbar)

1-ptukey(t_obs1, I, nobs-I)
1-ptukey(t_obs2, I, nobs-I)
1-ptukey(t_obs3, I, nobs-I)



#### EXERCISE 2 ####

# Simulate some data

n.sim = 100

# Initalize some vectors where we'll put our simulation values
pval_12.t = pval_13.t = pval_23.t = c() #t-tests
pval_12.c = pval_13.c = pval_23.c = c() #contrast tests
pval_12.HSD = pval_13.HSD = pval_23.HSD = c() #Tukey's HSD

# Initialize all of our given numbers
alpha = 0.05
n = 15
numgroups = 3
numtests = choose(numgroups,2)
alpha_bonferroni = alpha / numtests

q = qtukey(1-alpha,numgroups,n - numgroups)/sqrt(2)
alpha_tukey=2*(1-pt(q,df=n-numgroups))

for (i in 1:n.sim){
# First we generate data from 3 groups
x1 = rnorm(5, mean=1, sd=1)
x2 = rnorm(5, mean=1, sd=1)
x3 = rnorm(5, mean=1, sd=1)

x = c(x1,x2,x3)
groups = rep(1:3, each=5)
k = 3

# Pairwise t-tests
alpha = 0.05
pval_12.t[i]=t.test(x1,x2,var.equal=T)$p.value #multiply by k to correct for multiple comparisons
pval_13.t[i]=t.test(x1,x3,var.equal=T)$p.value
pval_23.t[i]=t.test(x2,x3, var.equal=T)$p.value

# Contrast tests
means = c(mean(x1), mean(x2), mean(x3))
sds = c(sd(x1), sd(x2), sd(x3))
n = c(5,5,5)
Sp2 = sum((n-1)*sds^2)/ (sum(n)-length(n))
df = sum(n)-length(n)

a = c(1,-1,0)
T_num = sum(a*means)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))
T = abs(T_num / T_denom)
pval_12.c[i] = 2*(1-pt(T,df))


a = c(1,0,-1)
T_num = sum(a*means)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))
T = abs(T_num / T_denom)
pval_13.c[i] = 2*(1-pt(T,df))


a = c(0,1,-1)
T_num = sum(a*means)
T_denom = sqrt(Sp2) * sqrt(sum(a^2/n))
T = abs(T_num / T_denom)
pval_23.c[i] = 2*(1-pt(T,df))

# Tukey's HSD
model = aov(x~factor(groups))
tukeytest = TukeyHSD(model)
tukeytest_pvals = as.vector(tukeytest[[1]][,4])
pval_12.HSD[i]=tukeytest_pvals[1]
pval_13.HSD[i]=tukeytest_pvals[2]
pval_23.HSD[i]=tukeytest_pvals[3]
}

par(mfrow=c(1,3))
hist(pval_12.t)
abline(v=0.05, col="red")
hist(pval_12.c)
abline(v=0.05, col="red")
hist(pval_12.HSD)
abline(v=0.05, col="red")

# Compares Type I Error rate for mean difference between groups 1 and 2
mean(pval_12.t <= alpha_bonferroni)
mean(pval_12.c <= alpha_bonferroni)
mean(pval_12.HSD <= alpha)

par(mfrow=c(1,3))
hist(pval_13.t)
abline(v=0.05, col="red")
hist(pval_13.c)
abline(v=0.05, col="red")
hist(pval_13.HSD)
abline(v=0.05, col="red")

# Compares Type I Error rate for mean difference between groups 1 and 3
mean(pval_13.t <= alpha_bonferroni)
mean(pval_13.c <= alpha_bonferroni)
mean(pval_13.HSD <= alpha)



par(mfrow=c(1,3))
hist(pval_23.t)
abline(v=0.05, col="red")
hist(pval_23.c)
abline(v=0.05, col="red")
hist(pval_23.HSD)
abline(v=0.05, col="red")

# Compares Type I Error rate for mean difference between groups 2 and 3
mean(pval_23.t <= alpha_bonferroni)
mean(pval_23.c <= alpha_bonferroni)
mean(pval_23.HSD <= alpha)
par(mfrow=c(1,1))
