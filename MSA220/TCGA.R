setwd("~/Desktop/big-data-exam")

load("TCGAdata.RData")

# The TCGA data comprising 6 types of cancers with 
# measured gene expression for 20530 genes is posted on the class home page.
# Compare one-against-all, pairwise-classification and
# multi-class classification techniques. 
# Discuss the results - are some cancers easy to tell 
# from other cancers using one method over another? Which
# cancers are difficult to tell apart and why?
# How do the methods compare with respect to selected 
# features? Are the same features selected? 
# Why/Why not? Roughly same number of features selected? Why/Why not?

data <- data.frame(TCGA,TCGAclassstr=TCGAclassstr)
n <- length(TCGAclassstr)
d <- dim(TCGA)[2]



sample.size <- 500
sample.idx <- sample(1:n,sample.size)

#feature sampling 
feature.sample <- sample(1:d,5000)
data.sample <- data[sample.idx,c(feature.sample,d+1)]

# training and  testing set 
groups <- sample(rep(1:2,times = ceiling(n / 2)),size = n, replace = F)
data.train <- data[!groups%in%1,c(feature.sample,d+1)]
data.test <- data[groups%in%1,c(feature.sample,d+1)]

##################################################################
### PCA projection
##################################################################
plot(cmdscale(dist(data[sample.idx,-(d+1)])),
     col = as.integer(data[sample.idx,]$TCGAclassstr))

##################################################################
#### KKNN
##################################################################
library(MASS)
library(kknn)
kknnfit <- kknn(TCGAclassstr~.,data.sample,data[-sample.idx,c(feature.sample,d+1)])
summary(kknnfit)
fit <- fitted(knnfit)
table(data[-sample.idx,c(feature.sample,d+1)]$TCGAclassstr,fit)

##################################################################
#### SVM ONE-AGAINST-ONE
##################################################################
library(klaR)

# sample.size <- 50
# sample.idx2 <- sample(1:n,sample.size)
# feature.sample <- sample(1:d,5000)
# data.sample2 <- data[sample.idx2,c(feature.sample,d+1)]
svmmodel <- svmlight(TCGAclassstr~.,data=data.train,class.type = "oao" ,pathsvm='./svm_ligth')
svmpred <- predict(svmmodel,data.test)
svmtable <-table(data.test$TCGAclassstr,svmpred$class)
errorTable <- matrix(rep(0,6),6,1)
for (i in 1:6) {
  errorTable[i] <- sum(svmtable[-i,i])/sum(svmtable[,i])
}
print(errorTable)

##################################################################
#### SVM ONE-AGAINST-ALL
##################################################################

oaa <- svmlight(TCGAclassstr~.,data=data.train,class.type = "oaa",pathsvm='./svm_ligth')
oaasvmpred <- predict(oaa,data.test)
oaasvmtable <-table(data.test$TCGAclassstr,svmpred$class)
print(oaasvmtable)
errorTable <- matrix(rep(0,6),6,1)
for (i in 1:6) {
  errorTable[i] <- sum(svmtable[-i,i])
}
print(errorTable)

####### var imp from ggRF run for ever without beging able to find a result.
library(ggRandomForests)
library(randomForestSRC)
rfsrc_fit <- rfsrc(TCGAclassstr~., data=data.train)
gg_e <- gg_error(rfsrc_fit)
plot(gg_e)
plot.variable(rfsrc_fit)
# plot(gg_vimp(rfsrc_fit))
