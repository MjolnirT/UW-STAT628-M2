rm(list=ls())
setwd('/Users/apple/Documents/Wisc/study/2019Fall/STAT 628/Module2')
bodyfat=read.csv('BodyFat.csv')
head(bodyfat)

summary(bodyfat)
which.min(bodyfat$DENSITY)

bodyfat[216,]#this guy has a density<1 
bodyfat[182,]#this has bodyfat=0
#these two guy we need to check

##########################################
#######detect outlier and clean data######
##########################################

# 1. use siri equation to find outlier

#according to siri function we know that there is a  relationship between 1/density and bodyfat
#siri=495/bodyfat$DENSITY - 450
reverse.density=1/bodyfat$DENSITY
plot(bodyfat$BODYFAT ~ reverse.density,main="Siri's equation",xlab="1/Density",ylab="Body fat",pch=19,col="red")
text(reverse.density[c(96, 182 ,48 ,76)],bodyfat$BODYFAT[c(96, 182 ,48 ,76)] ,c(96, 182 ,48 ,76),p=4) #96 182 48 76
#From the plot above ,there are some points need to check
bodyfat[c(48 ,76,96, 182 ),]

#1)
#for the 48 its bodyfat is 6.4 which shows something wrong
#then use the siri function to compute the bodyfat
a=495/bodyfat$DENSITY[48]- 450
a#14.13502 which is much more reliable
bodyfat$BODYFAT[48]=a
#2)
#for the 96
bodyfat[c(96,97,98),]#we can find from 97 and 98 that,when density increase ,the bodyfat should decrease,however this time when density goes up the bodyfat goes up . 
#if we use the density to compute the bodyfat then it should be
495/bodyfat$DENSITY[96]- 450#0.3684833 which is impossible compare with the bodyfat with others
#hence this time we use bodyfat to compute density
bodyfat$DENSITY[96]=495/(bodyfat$BODYFAT[96]+450) #the new density
#3)
#for the 182 
bodyfat[c(182,183,184),]#the bodyfat is 0 so we should use density to compute
495/bodyfat$DENSITY[182]- 450#after applying siri's equation,the bodyfat is negative ,which is impossible
#hence we should delect 182,neither the bodyfat nor density  are correct.
#4)
#for the 76
bodyfat[c(72,76),]
#compare these two ,we can see that the bodyfat has a large diff
#however the bmi not shows much diff and the other variables shows the 76 should not be fat
#hence the bodyfat of 76 might be wrong 
fat=495/bodyfat$DENSITY[76]- 450
bodyfat$BODYFAT[76]=fat
#check again
reverse.density=1/bodyfat$DENSITY
plot(bodyfat$BODYFAT[-182] ~ reverse.density[-182],main="Siri's equation",xlab="1/Density",ylab="Body fat",pch=19,col="red")

#from above,we know that 182 is the one we need to delet 
#and for 216,it's 1/density and bodyfat is on the line,
#hence the density is wrong and the bodyfat is computed by siri's equation
#thus the 216 should also delet.

#2. use BMI to find outliers bmi=weight/(height)^2
bmi=bodyfat$WEIGHT/(bodyfat$HEIGHT)^2
plot(bodyfat$ADIPOSITY[c(-182,-216)] ~ bmi[c(-182,-216)],main="BMI equation",xlab=expression(WEIGHT/(HEIGHT)^2),ylab="ADIPOSITY",pch=19,col="red")
text(bmi[c(42,163)],bodyfat$ADIPOSITY[c(42,163)]  ,c(42,163),p=3) #42,163,221
text(bmi[c(221)],bodyfat$ADIPOSITY[c(221)]  ,c(221),p=1) #42,163,221

#1)check what's wrong with 42
bodyfat[c(41,42,43),]#the heigth of the 42 is only 29.5 which might be mistake
sqrt(bodyfat$WEIGHT[42]/bodyfat$ADIPOSITY[42])
#construct a model so that we can use bmi to compute ADIPOSITY
model.bmi=lm(bodyfat$ADIPOSITY[-42]~bmi[-42])
summary(model.bmi)
#then ADIPOSITY=0.1942+698.6151*bmi
bmi.42=(bodyfat$ADIPOSITY[42]-0.1942)/698.6151
height=sqrt(bodyfat$WEIGHT[42]/bmi.42)
#hence the height of the 42 guy is 69.4345 much reliable
bodyfat$HEIGHT[42]=height
bmi=bodyfat$WEIGHT/(bodyfat$HEIGHT)^2
plot(bodyfat$ADIPOSITY ~ bmi,main="BMI equation",xlab=expression(WEIGHT/(HEIGHT)^2),ylab="ADIPOSITY",pch=19,col="red")
text(bmi[c(163,221)],bodyfat$ADIPOSITY [c(163,221)] ,c(163,221),p=2) #96 182 48 76
#2)check 221
bodyfat[c(116,123,218,221),]
#we see that for 221 ,the weight and height are similar with the 218 however their ADIPOSITY are diff
#there is one mistake among these three,we can compare the rest variables 221's are larger than 218's
#but the weight is smaller,and then compare 211 with 116,123...
#hence the weight of 221 might be wrong
bmi.221=(bodyfat$ADIPOSITY[221]-0.1942)/698.6151
weight=bmi.221*(bodyfat$HEIGHT[221])^2
bodyfat$WEIGHT[221]=weight
#3)check 163
bodyfat[c(158,163,167,170),]
#compare 163 and 167 the value of 163 are larger than 167 and thier height are the same
#it is reasonable to say 163 is fatter is larger than 167 and the the weight also shows is
#but the ADIPOSITY of 163 is smaller
#hence we guess there is a mistake when we compute the ADIPOSITY.
#What's more weight 163>158,height:163<158,the ADIPOSITY should be 163>158,but the data is not
bmi.163=bodyfat$WEIGHT[163]/(bodyfat$HEIGHT[163])^2
adi=0.1942+698.6151*bmi.163
bodyfat$ADIPOSITY[163]=adi

#check the bmi equation again
bmi=bodyfat$WEIGHT/(bodyfat$HEIGHT)^2
plot(bodyfat$ADIPOSITY ~ bmi,main="BMI equation",xlab=expression(WEIGHT/(HEIGHT)^2),ylab="ADIPOSITY",pch=19,col="red")
text(bmi[c(42,163,221)],bodyfat$ADIPOSITY[c(42,163,221)]  ,c(42,163,221),p=2) #96 182 48 76
#it seems good

#using these two euqations we only delet 182,216 and change some values 


# 3. use the Cook's distance to detect outliers

#Cook's distance or Cook's D is a commonly used estimate of
#the influence of a data point when performing a least-squares regression
#analysis
#Rule of thumb: classify as leverages anything above 4/(n-p).
model=lm(BODYFAT~ ., data=bodyfat[-c(182,216 ),c(-1,-3)])
plot(model,which = 4)
n=nrow(bodyfat[-c(182 ),c(-1,-3)])
p=ncol(bodyfat[-c(182 ),c(-1,-3)])
abline(h = 4/(n-p),lty=2,col='red')
#library(car)
#influencePlot(model)#
#outlierTest(model,cutoff=Inf, n.max=Inf, order=TRUE)
#Bonferroni Outlier Test
#Description
#Reports the Bonferroni p-values for testing each observation 
#in turn to be a mean-shift outlier, based Studentized residuals 
#in linear (t-tests), generalized linear models (normal tests), 
#and linear mixed models.

#we can see the 39 is a influntial point
outlier=bodyfat[39,]#only  29.5 height outlier
bodyfat[39,]#weight  363.15 pounds
#since we'd like to find a way to predict the bodyfat for most people
#39 as a very influntial point may affect our result and the it's not common
#hence we decide to delect 39

## delect number 39 and fit the model again
model_1=lm(BODYFAT~ ., data=bodyfat[-c(39,182,216 ),c(-1,-3)])
summary(model_1)
layout(matrix(1:4, ncol=2))
plot(model_1)
layout(matrix(1:1, ncol=1))
plot(model_1,which = 4)
n=n-1
abline(h = 4/(n-p),lty=2,col='red')
influencePlot(model_1)
outlierTest(model_1)
bodyfat[86,]
## delect number 86 and fit the model again
model_2=lm(BODYFAT~ ., data=bodyfat[-c(39,86,182,216 ),c(-1,-3)])
summary(model_2)
layout(matrix(1:4, ncol=2))
plot(model_2)
layout(matrix(1:1, ncol=1))
plot(model_2,which = 4)
n=n-1
abline(h = 4/(n-p),lty=2,col='red')
influencePlot(model_2)
outlierTest(model_2)

# after cleaning the data the new dataset
bodyfat_new=bodyfat[-c(39,86,182,216),c(-1)]
write.csv(bodyfat_new,"clean dataset.csv")


