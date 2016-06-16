setwd("~/Desktop/big-data-exam")

load("SpamContaminated.RData")

X <- SpamContaminated[,1:57]
Y <- SpamContaminated[,58]

SpamContaminated[1,]$type == 'spam'

plot(cmdscale(dist(X)),
     col = as.integer(Y))

##################################################################
#### SVM
##################################################################
library(e1071)

model <- svm(X,Y,scale = TRUE)
print(model)
summary(model)
pred <- predict(model, X)
# Check accuracy:
(table(pred, Y)[1,1] + table(pred, Y)[2,2])/length(Y)
miss.classed <- which(((pred==Y)%in% FALSE))
print(length(miss.classed)/length(Y))


##################################################################
####  Random forest 
##################################################################

library(randomForest)
library(plotmo)
data <- data.frame(X,class=Y)
rf<-randomForest(class~.,data=data,importance=TRUE,proximity=TRUE)
plotmo(rf)
print(rf$confusion)
miss.classed1 <- which(((rf$predicted==Y)%in% FALSE))
print(length(miss.classed1)/length(Y))

##################################################################
#### R PART 
##################################################################
library(rpart)
library(rpart.plot)
ctrl<-trainControl(method="repeatedcv",repeats=3,classProbs=TRUE,summaryFunction=twoClassSummary)
rpartfit<-train(class~.,data=data,method="rpart",metric="ROC",trControl=ctrl)
pred2<-predict(rpartfit,newdata=data)
miss.classed2 <- which(((pred2==Y)%in% FALSE))
print(length(miss.classed2)/length(Y))

##################################################################
#### KNN 
##################################################################
knnfit<-train(class~.,data=data,method="knn",metric="ROC",trControl=ctrl)
pred3<-predict(knnfit,newdata=data)
miss.classed3 <- which(((pred3==Y)%in% FALSE))
print(length(miss.classed3)/length(Y))
common.miss.classed0123<- which(miss.classed3 %in% miss.classed1)
length(common.miss.classed0123)/length(Y)

##################################################################
#### Naive Bayes
##################################################################
nbfit<-train(class~.,data=data,method="nb",tuneLength=15,metric="ROC",trControl=ctrl)
pred4<-predict(nbfit,newdata=data)
miss.classed4 <- which(((pred4==Y)%in% FALSE))
print(length(miss.classed4)/length(Y))
common.miss.classed01234<- which(common.miss.classed0123 %in% miss.classed4)
length(common.miss.classed01234)/length(Y)

common.miss.classed01 <- miss.classed[which(miss.classed %in% miss.classed1)]
length(common.miss.classed01)/length(Y)
common.miss.classed012<- common.miss.classed01[which(common.miss.classed01 %in% miss.classed2)]
length(common.miss.classed012)/length(Y)
common.miss.classed0123 <- common.miss.classed012[which(common.miss.classed012 %in% miss.classed3)]
length(common.miss.classed0123)/length(Y)
common.miss.classed01234 <- common.miss.classed0123[which(common.miss.classed0123 %in% miss.classed4)]
length(common.miss.classed01234)/length(Y)

print(common.miss.classed01234)
Y2<-as.numeric(Y)*0+4
Y2[common.miss.classed01234]=1

pca_proj <- cmdscale(dist(X))
plot(pca_proj,
     col = as.integer(Y2), pch=as.character(Y),
     main = "PCA projection of email (in black the missclassified 4%)",
     sub = NULL,
     xlab = "PCA1",
     ylab = "PCA2")
identify(pca_proj)
plot(pca_proj[common.miss.classed01234,],
     col = as.integer(Y[common.miss.classed01234]),
     main = "PCA projection of miss-classified email (about 4%)",
     sub = NULL,
     xlab = "PCA1",
     ylab = "PCA2")


