##########################################
############select variables##############
##########################################
rm(list=ls())
setwd('/Users/apple/Documents/Wisc/study/2019Fall/STAT 628/Module2')
bodyfat_new=read.csv("clean dataset.csv")
#3. use vif check multicollinear
library(car)
bodyfat_new=bodyfat_new[,-1]
model.new=lm(BODYFAT ~ . , data=bodyfat_new[,-2])#delete density and a column represent the order
vif(model.new)
#there is multicollinear

#4. aic bic to find model

##AIC BIC
#both direction AIC
model.aic.both=step(model.new,direction = "both",k=2)
summary(model.aic.both)
#678.82,BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST
n=nrow(bodyfat_new)
model.bic.both=step(model.new,direction = "both",k=log(n))
summary(model.bic.both)
#695.84 BODYFAT ~ WEIGHT + ABDOMEN + WRIST

###A rule of thumb: Good models are those that are within 2 AIC units of the lowest AIC value. Models with more than 10 AIC units above the lowest AIC value are generally not considered.
###AIC can result in overfitting.
###AIC and most other criteria do not have measure of variation and thus the rules are generally approximations.
##AIC和BIC均引入了与模型参数个数相关的惩罚项，BIC的惩罚项比AIC的大，考虑了样本数量，样本数量过多时，可有效防止模型精度过高造成的模型复杂度过高。

#Hence choose the model lm(BODYFAT ~ WEIGHT + ABDOMEN + WRIST)

## WRIST may not change too much we could delete
##########################################
#############fit models###################
##########################################

#Cross validation group
source("cv-mse.R")

mygroup=CVgroup(5,nrow(bodyfat_new))



##From the BIC method
model.new=lm(BODYFAT~ WEIGHT + ABDOMEN + WRIST,data=bodyfat_new)
summary(model.new)# the R^2 is 0.7251
anova(model.new)#参数显著
model.bic1=BODYFAT ~ WEIGHT + ABDOMEN + WRIST
bic_cv1 = CVMSE(bodyfat_new, model.bic1, 5,100)
bic_cv1#15.69449

#Since we want to find the simple and accurate model,and we know that the 
#WRIST doesnot different too much.Just try to delete it.
model.new1=lm(BODYFAT~ WEIGHT + ABDOMEN,data=bodyfat_new)
summary(model.new1)#the R^2 is 0.7133 compare with the full modle  the R^2 not change much
model.bic2=BODYFAT ~ WEIGHT + ABDOMEN
bic_cv2 = CVMSE(bodyfat_new, model.bic2, 5,100)
bic_cv2#16.20634 # the mse not change too much

##What if we only use one variable?

model.new2=lm(BODYFAT~ WEIGHT,data=bodyfat_new)
summary(model.new2)#the R^2 is 0.3721 decrease too much
model.bic3=BODYFAT ~ WEIGHT
bic_cv3 = CVMSE(bodyfat_new, model.bic3, 5,100)
bic_cv3#35.17816


model.new3=lm(BODYFAT~  ABDOMEN ,data=bodyfat_new)
summary(model.new3)#the R^2 is 0.6672 
model.bic3=BODYFAT ~ ABDOMEN 
bic_cv4 = CVMSE(bodyfat_new, model.bic3, 5,100)
bic_cv4#18.62439
#only one variable seems too easy to get an error

#BODYFAT=-37.26486+ 0.60769 ABDOMEN
#slope is constant may lead a problem that the bodyfat increase 
#the same when circumference of ABDOMEN increase from 70-75 and 85-90,

##########################################
####check model assumptions###############
##########################################
model.new1=lm(BODYFAT~ WEIGHT + ABDOMEN,data=bodyfat_new)
#test slope whether is 0
summary(model.new1) #参数显著
#standadized residual plot 
plot(model.new1)
#normality
shapiro.test(model.new1$residuals)#p= 0.2336>0.05 it is normal
#collinearity?
vif(model.new1)# both <10 no collinearity problem

#strength simple & linearity
#weakness not a high acc
