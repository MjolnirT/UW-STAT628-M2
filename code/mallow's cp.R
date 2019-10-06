#mallow's cp
library(leaps)
library(faraway)
n=ncol(cleanbodyfat)
X = cleanbodyfat[, 2:n]
Y = cleanbodyfat$DENSITY
g1=leaps(X, Y, nbest = 1)
Cpplot(g1)
order(g1$Cp)[1] #8
as.numeric(which(g1$which[8,]==TRUE))
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[8,]==TRUE)) + 1])
model_cp1 = DENSITY ~ AGE + WEIGHT + NECK + ABDOMEN +  HIP + THIGH + FOREARM + WRIST
cp1_cv = CVMSE(cleanbodyfat, model_cp1, 5,100)   # number of variables is 8
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[7,]==TRUE)) + 1])  
model_cp2 = DENSITY ~ AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST
cp2_cv = CVMSE(cleanbodyfat, model_cp2, 5,100)   # number of variables is 7
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[6,]==TRUE)) + 1])  
model_cp3 = DENSITY ~  WEIGHT + NECK + ABDOMEN + BICEPS + FOREARM + WRIST
cp3_cv = CVMSE(cleanbodyfat, model_cp3, 5,100)   # number of variables is 6
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[5,]==TRUE)) + 1])  
model_cp4 = DENSITY ~  WEIGHT + ABDOMEN + BICEPS + FOREARM + WRIST
cp4_cv = CVMSE(cleanbodyfat, model_cp4, 5,100)   # number of variables is 5
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[4,]==TRUE)) + 1])  
model_cp5 = DENSITY ~  WEIGHT + ABDOMEN  + FOREARM + WRIST
cp5_cv = CVMSE(cleanbodyfat, model_cp5, 5,100)   # number of variables is 4
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[3,]==TRUE)) + 1])  
model_cp6 = DENSITY ~  WEIGHT + ABDOMEN + WRIST
cp6_cv = CVMSE(cleanbodyfat, model_cp6, 5,100)   # number of variables is 3
print(colnames(cleanbodyfat)[as.numeric(which(g1$which[2,]==TRUE)) + 1])  
model_cp7 = DENSITY ~  WEIGHT + ABDOMEN 
cp7_cv = CVMSE(cleanbodyfat, model_cp7, 5,100)   # number of variables is 2
c(cp1_cv,cp2_cv,cp3_cv,cp4_cv,cp5_cv,cp6_cv,cp7_cv)