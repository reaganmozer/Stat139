#######################
# Section 1, Stat 139 #
#   More R Practice   #
#      9/9/15         #
#    Reagan Rose      #
#######################
# read in the data from the file "swans.dat", which has a header row, so
# the first 5 rows look like:
#
#   circum   weight   wingspan   cwidth   cheight   spec     age     sex
#   75.5      9.6      228.0     25.5      20.5       0       1       M
#   72.0     10.8      233.0     24.5      19.5       0       1       M
#   69.5      5.6      218.5     24.0      17.5       0       1       F
#   72.0      9.8      218.0     24.5      19.0       0       1       F
#
swans=read.table("swans.dat",header=T)
#
# we can look at the data using
swans
# subset it using
swans[2,] # the second row
swans[,3] # the third column
swans[2,3] # the entry in the second row and third column
swans[swans[,6]==1,] # all swans with 6th col==1
swans[swans$sex=="F",] # all female swans
swans[swans$weight>10,] # all swans weighing more than 10 kg
#
# we can copy:
swanscopy = swans
#
# and transform:
swanscopy$cheight = log(swanscopy$cheight)
#
# we can compute summary statistics:
summary(swans)
mean(swans)
var(swans)
# OK, so it does not like the categorical variable, and we need to do:
var(swans[,1:5])
# the above matrix is a COVARIANCE matrix, the one below is a CORRELATION matrix:
cor(swans[,1:5])
#
# we can summarize the categorical variables:
table(swans$sex)
table(swans$sex,swans$age)
# 
# we can do normal probability plots:
qqnorm(swans$circum)
qqnorm(swans$weight)
qqnorm(swans$wingspan)
#
# do stem-and-leaf plots:
stem(swans$weight)
#
# and histograms:
hist(swans$weight)
hist(swans$weight,breaks=20)
#
# barcharts for categorical variables:
barplot(table(swans$sex))
barplot(table(swans$sex,swans$age))
#
# we can do bivariate plots, e.g. plot weight versus circumference:
plot(swans[,"circum"],swans[,"weight"])
#
# or:
plot(swans$circum,swans$weight)
#
# plot only the female swans:
plot(swans[swans$sex=="F",]$circum,swans[swans$sex=="F",]$weight)
#
# plot all the swans but use M or F for the symbol:
plot(swans$circum,swans$weight,type="n")
text(swans$circum,swans$weight,swans$sex)
#
# and we can even use color:
plot(swans$circum,swans$weight,col=as.numeric(swans$sex))
#
# note that as.numeric(swans$sex) assigns numbers to the levels of 
# the categorical variable in alphabetical order, so that
# "M" is converted to 2 and "F" is converted to 1.
# We can change the colors to be whatever we like, in the following way:
#
plot(swans$circum,swans$weight,col=c("magenta","blue")[as.numeric(swans$sex)])
#
# and the available color names can be seen using the command:
# 
colors()
# 
# to compare male and female swans we can do a side-by-side boxplot:
boxplot(circum~sex,data=swans)
# to look at all variables we can do a pairs plot:
pairs(swans)
# which is silly for the categoricals, so just do:
#
pairs(~circum+weight+wingspan+cwidth+cheight,data=swans)
#
# and we can color this one too:
pairs(~circum+weight+wingspan+cwidth+cheight,data=swans,
	col=c("magenta","blue")[as.numeric(swans$sex)])
#
# We can fit a linear model:
swans.reg1 = lm(weight~circum,data=swans)
#
# do the scatterplot and superimpose the regression line
plot(swans$circum,swans$weight,col=c("magenta","blue")[as.numeric(swans$sex)])
abline(swans.reg1)
#
# we could do the regression separately for boys and girls:
swans.reg1F = lm(weight~circum,data=swans[swans$sex=="F",])
swans.reg1M = lm(weight~circum,data=swans[swans$sex=="M",])
plot(swans$circum,swans$weight,col=c("magenta","blue")[as.numeric(swans$sex)])
abline(swans.reg1F,col="magenta")
abline(swans.reg1M,col="blue")
# 
# or fit a multiple regression:
swans.reg = lm(weight~circum+wingspan+cwidth+cheight+sex,data=swans)
#
# and we can plot weight versus wingspan and put the regression line on the plot:
summary(swans.reg)
plot(swans.reg)

