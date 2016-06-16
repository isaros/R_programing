setwd("~/Desktop/big-data-exam")

library(MASS)
library(kknn)

Xtrain <- read.table("gisette_train.data.txt")
Ytrain <- factor(read.table("gisette_train.labels.txt")[,1])
Xtest <- read.table("gisette_valid.data.txt")
Ytest <- factor(read.table("gisette_valid.labels.txt")[,1])


train <- data.frame(Xtrain,class=Ytrain)
train$class
test <- data.frame(Xtest,class=Ytest)

##################################################################
### PCA projection
##################################################################
plot(cmdscale(dist(Xtrain)),
     col = as.integer(Ytrain))


kknnfit <- kknn(class~.,train,test,kernel = "rectangular")
summary(kknnfit)
fit <- fitted(kknnfit)
table(test$class,fit)


trifit <- kknn(class~.,train,test,kernel = "triangular")
summary(trifit)
fit <- fitted(trifit)
table(test$class,fit)

epafit <- kknn(class~.,train,test,kernel = "epanechnikov")
summary(epafit)
fit <- fitted(epafit)
table(test$class,fit)


bifit <- kknn(class~.,train,test,kernel = "biweight")
summary(bifit)
fit <- fitted(bifit)
table(test$class,fit)



triweightfit <- kknn(class~.,train,test,kernel = "triweight" )
summary(triweightfit)
fit <- fitted(triweightfit)
table(test$class,fit)


gausfit <- kknn(class~.,train,test,kernel = "gaussian" )
summary(gausfit)
fit <- fitted(gausfit)
table(test$class,fit)




fit1 <- kknn(class~.,train,test,kernel = "rank" )
fit11 <- fitted(fit1)
table(test$class,fit11)
fit2 <- kknn(class~.,train,test,kernel = "optimal" )
fit22 <- fitted(fit2)
table(test$class,fit22)
fit3 <- kknn(class~.,train,test,kernel = "cos" )
fit33 <- fitted(fit3)
table(test$class,fit33)
fit4 <- kknn(class~.,train,test,kernel = "inv" )
fit44 <- fitted(fit4)
table(test$class,fit44)








