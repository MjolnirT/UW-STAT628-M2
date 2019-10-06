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
mygroup=CVgroup(5,nrow(cleanbodyfat))

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