###
### Lasso in this model

library(glmnet)

### Data Path
data = read.csv("C:/Users/Tansu/Documents/GitHub/UW-STAT628-M2/dataset/clean dataset.csv")

### Lasso
lasso_m = glmnet(as.matrix(data[,c(-1,-2,-3)]),data[,2],alpha = 1)

lasso_m$lambda
coef(lasso_m,xvar="lambda")
plot(lasso_m, label = T, xvar="lambda")

cvlassofit = cv.glmnet(as.matrix(data[,c(-1,-2,-3)]),data[,2],alpha = 1)
plot(cvlassofit)
### The variable we selected
coef(cvlassofit, s = "lambda.1se")
## The lambda we are choosing
cvlassofit$lambda.1se

### Get the R^2
cvlassofit$glmnet.fit$dev.ratio[which(cvlassofit$glmnet.fit$lambda == cvlassofit$lambda.1se)]
cvlassofit$cvm[which(cvlassofit$glmnet.fit$lambda == cvlassofit$lambda.1se)]



### Elastic net
elastic_net_m = glmnet(as.matrix(data[,c(-1,-2,-3)]),data[,2],alpha = 0.5)
elastic_net_m$lambda
plot(elastic_net_m, label = T, xvar="lambda")

cv_elastic_netfit = cv.glmnet(as.matrix(data[,c(-1,-2,-3)]),data[,2],alpha = 1)
plot(cv_elastic_netfit )
### The variable we selected
coef(cv_elastic_netfit , s = "lambda.1se")
## The lambda we are choosing
cv_elastic_netfit$lambda.1se


### Get the R^2
cv_elastic_netfit$glmnet.fit$dev.ratio[which(cv_elastic_netfit$glmnet.fit$lambda == cv_elastic_netfit$lambda.1se)]
cv_elastic_netfit$cvm[which(cv_elastic_netfit$glmnet.fit$lambda == cv_elastic_netfit$lambda.1se)]


