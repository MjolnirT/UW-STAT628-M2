#Adjusted R square
g2=leaps(X, Y, nbest = 1,method = "adjr2")
Cpplot(g2)
order(g2$adjr2,decreasing = TRUE)[1] #10
as.numeric(which(g2$which[10,]==TRUE))
print(colnames(cleanbodyfat)[as.numeric(which(g2$which[10,]==TRUE)) + 1])
model_adjr21 = DENSITY ~ AGE + WEIGHT + NECK + ABDOMEN +  HIP + THIGH + ANKLE +BICEPS+FOREARM + WRIST
adjr21_cv = CVMSE(cleanbodyfat, model_adjr21, 5,100)   # number of variables is 10