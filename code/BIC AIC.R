##########################################
############select variables##############
##########################################

#3. use vif check multicollinear
library(car)
model.new=lm(BODYFAT~ ., data=bodyfat_new[,-2])
vif(model.new)
#there is multicollinear

#4. aic bic to find model

##AIC BIC
#both direction AIC
model.aic.both=step(model.new,direction = "both",k=2)
summary(model.aic.both)
#670.89,BODYFAT ~ AGE + WEIGHT + HEIGHT + ADIPOSITY + ABDOMEN + THIGH + FOREARM + WRIST
model.bic.both=step(model.new,direction = "both",scope=list(lower=~1,upper=model),k=log(n))
summary(model.bic.both)
#685.87 BODYFAT ~ WEIGHT + ABDOMEN + WRIST

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
library(plyr)
CVgroup=function(k,Ndata){
  cvlist=list()
  allseq=rep(1:k,ceiling(Ndata/k))[1:Ndata]   
  temp=sample(allseq,Ndata)   
  x=1:k
  dataseq=1:Ndata
  cvlist=lapply(x,function(x) {dataseq[temp==x]})  
  return(cvlist)
}

mygroup=CVgroup(5,nrow(bodyfat_new))


#calculate the mean mse after cross validation
DoCV=function(k,insertdata,model){
  mygroup=CVgroup(k,nrow(insertdata))
  datacol=ncol(insertdata)
  MSEs=1:k
  for(i in 1:5) {
    Ntraining = setdiff(1:5, i)
    trainingINDEX = numeric()
    
    for (t in Ntraining) {
      trainingINDEX = c(trainingINDEX, unlist(mygroup[[t]]))
    }
    
    testINDEX = mygroup[[i]]
    
    m = lm(model, data  = insertdata[trainingINDEX, ])
    
    test_X = insertdata[testINDEX, 2:datacol]
    test_Y = insertdata[testINDEX, 1]
    
    predicted_Y = predict(m, test_X)
    MSEs[i] = sum((test_Y - predicted_Y)^2) / length(test_Y)
  }
  return(mean(MSEs))
}

#calculate the mse after s times
CVMSE=function(insertdata,model,k,s){
  sumMES = 0
  
  for (i in 1:s) {
    sumMES = sumMES + DoCV(k,insertdata, model)
  }
  
  return(sumMES / s)
}


##From the BIC method
model.new=lm(BODYFAT~ WEIGHT + ABDOMEN + WRIST,data=bodyfat_new)
summary(model.new)# the R^2 is 0.7251
anova(model.new)#参数显著
model.bic1=BODYFAT ~ WEIGHT + ABDOMEN + WRIST
bic_cv1 = CVMSE(bodyfat_new, model.bic1, 5,100)
bic_cv1#15.6832
#Since we want to find the simple and accurate model,and we know that the 
#WRIST doesnot different too much.Just try to delete it.
model.new1=lm(BODYFAT~ WEIGHT + ABDOMEN,data=bodyfat_new)
summary(model.new1)#the R^2 is 0.7133 compare with the full modle  the R^2 not change much
model.bic2=BODYFAT ~ WEIGHT + ABDOMEN
bic_cv2 = CVMSE(bodyfat_new, model.bic2, 5,100)
bic_cv2#16.19427 # the mse not change too much

##What if we only use one variable?

model.new2=lm(BODYFAT~ WEIGHT,data=bodyfat_new)
summary(model.new2)#the R^2 is 0.3721 decrease too much
model.bic3=BODYFAT ~ WEIGHT
bic_cv3 = CVMSE(bodyfat_new, model.bic3, 5,100)
bic_cv3#35.15616 


model.new3=lm(BODYFAT~  ABDOMEN ,data=bodyfat_new)
summary(model.new3)#the R^2 is 0.6672 
model.bic3=BODYFAT ~ ABDOMEN 
bic_cv4 = CVMSE(bodyfat_new, model.bic3, 5,100)
bic_cv4#18.67761
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