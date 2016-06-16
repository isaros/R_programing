

setwd("~/Desktop/big-data-exam")

load("CATSnDOGS.RData")
# 99 cats 99 dogs ; 4096 feature (ie 64*64 pixels)
n <- dim(CATSnDOGS)[1]
# On Cat
# image(matrix(CATSnDOGS[50,4096:1],64,64,byrow=T),col=gray.colors(256,0,1))
# The average Cat
image(matrix(colMeans(CATSnDOGS[1:99,4096:1]),64,64,byrow=T),col=gray.colors(256,0,1))

# One Dog
# image(matrix(CATSnDOGS[150,4096:1],64,64,byrow=T),col=gray.colors(256,0,1))
# The average dog
image(matrix(colMeans(CATSnDOGS[100:198,4096:1]),64,64,byrow=T),col=gray.colors(256,0,1))



##################################################################
######################## Classification ##########################
##################################################################
# (A) Classification: Can you tell cats from dogs based on low-resolution images?
# Provide reliable estimates for the misclassification error rate on new data
# - discuss how you estimate this and why you think it’s a reliable estimate for future data.
# How many features do you need? How do you decide this?
# Which classification technique do you prefer and why.
# (In image classification there are more complex methods 
# for extracting features than we have discussed -
# you don’t need to use these but work with the methods we’ve discussed in class).


# Data
X <- CATSnDOGS
# Labels
Y = matrix(,0,n)
Y[1:99] <- 0
Y[100:n] <- 1
# Transfor Y in a factor for caret
Y <- factor(x= Y)

n.randomtest <- 10


n.groups <- 10 #number groups cross validation
Accuracy <- 1:(n.randomtest*n.groups)*0


##################################################################
#### Caret :
##################################################################
library(caret)
# Train with SVM (caret)

# Transfor Y in a factor for caret
Y <- factor(x= Y)

for (i in 1:n.groups) {
  X.train <- X[!groups%in%i,]
  X.test <- X[groups%in%i,]
  Y.train <- Y[!groups%in%i]
  Y.test <- Y[groups%in%i] 
  svmT <- train(x = X.train, y = Y.train, method="ranger", scale = FALSE)
  svmClass <- predict.train(svmT, newdata = X.test , testX = X.test, testY = Y.test)
  Accuracy[i] <- confusionMatrix(data = svmClass, Y.test)$overall[1]
  
}

plot(Accuracy)
mean(Accuracy)

importance <- varImp(svmT)
importance
plot(svmClass)
svmClass
confusionMatrix(data = svmClass, Y.test)
##################################################################
#### # Train with SVM from e1071 package
##################################################################

library(e1071)

# Do n.randomtest times a n.group cross validation and save the accuracy
for (j in 1:n.randomtest){
  # Change the random seed
  set.seed(10+2*j)
  
  # Generate the cross val group 
  groups <- sample(rep(1:n.groups,times = ceiling(nrow(X) / n.groups)),size = nrow(X), replace = F)
  
  # Operate Cross Val
  for (i in 1:n.groups) {
    X.train <- X[!groups%in%i,]
    X.test <- X[groups%in%i,]
    Y.train <- Y[!groups%in%i]
    Y.test <- Y[groups%in%i] 
    
    
    ## classification mode
    # default with factor response:
    model <- svm(X.train,Y.train, kernel = "radial")
    
    print(model)
    summary(model)
    
    # test with test data
    pred <- predict(model, X.test)
    
    # Check accuracy:
    Accuracy[i+n.groups*(j-1)]<- (table(pred, Y.test)[1,1] + table(pred, Y.test)[2,2])/length(Y.test)
  }
}

plot(Accuracy)
# boxplot(Accuracy)
mean(Accuracy)

# Try visualize with PCA projection (classes by color, SV by crosses):
plot(cmdscale(dist(X.train)),
     col = as.integer(Y.train),
     pch = c("o","+")[1:length(Y.train) %in% model$index + 1])

##################################################################
#### Result of SVM radial and linear kernel :
##################################################################

###### Parameters of the svm model:
# SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.0002441406 

###### Result -> 
# mean accuracy for 10 fold cross validation 0.8381579 with svm method
# 0.8615873 for 30 fold cross validation. 
# 0.867885 on average for 10 time a 10 fold CV

###### Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1 
# gamma:  0.0002441406 
###### Result -> 
#  0.8134211 mean accuracy for 10 times 10 fold cross validation with svm method
#  0.8134211 mean accuracy for 1 times 10 fold cross validation with svm method


##################################################################
#### VAR IMP FROM CARET :
##################################################################

# Importance
# V2386     100.00
# V2387      99.61
# V2130      98.97
# V1938      98.79
# V2194      98.67
# V2002      98.61
# V1937      98.31
# V1679      98.04
# V2323      98.01
# V2385      97.21
# V2322      97.10
# V2321      96.98
# V2451      96.85
# V2066      96.69
# V1680      96.49
# V2195      96.44
# V1683      96.42
# V1810      96.37
# V2449      96.33
# V2003      96.09
library(caret)
varReduce <- c(2386,2387,2130,1938,2194,2002,1937,1679,2323,2385,2322,2321,2451,2066,1680 ,2195,1683,1810,2449,2003)
Xreduce <- X[,varReduce]
featurePlot(x = Xreduce[,1:5],
            y = Y,
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 2)
)
library(rgl)
plot3d(Xreduce[,1:3],col = as.integer(Y))
#### Interpretation 
# From the most important variables the two classes are still overlaping a lot.
# Both classes seems to have a common structure from those variables, only translated.
# That could be partially explained by the fact that the feature, which are pixels,
# do not make sence by own, in image classification, the important feature for classication
# rely on mean and variance of line and column or even gradients.
# Therefore a transformation of the data could be interesting. 


##################################################################
#### Classification boundaries with other method.
##################################################################
library(klaR)
reduceCatNdog <- data.frame(Xreduce[,1:5],class=Y)
#classscatter(class~.,data=catNdog, method = "lda")
newfac<-rep(0,dim(reduceCatNdog)[1])
newfac[reduceCatNdog$class==-1]<-"Cat"
newfac[reduceCatNdog$class==1]<-"Dog"
newfac<-as.factor(newfac)
reduceCatNdog$class<-newfac
### Try to plot classification boundaries from first 5 component from varImp
partimat(class~.,data=reduceCatNdog, method = "lda")
partimat(class~.,data=reduceCatNdog, method = "qda")
partimat(class~.,data=reduceCatNdog, method = "naiveBayes")
partimat(class~.,data=reduceCatNdog, method = "rpart")


##################################################################
######## Dimension reduction and data representation #############
##################################################################

##################################################################
#### SVD 
##################################################################
# ssn<-svd(X)
# plot(ssn$d)
# # most of variance explained by 20 components
# plot(ssn$u[,1:2])
# identify(ssn$u[,1:2])
# # [1]  40  47  55 179
# image(matrix(CATSnDOGS[47,4096:1],64,64,byrow=T),col=gray.colors(256,0,1))
# # highligthing the structure that the 1st components identify and separate
# 
# plot(ssn$u[,1:2],pch=as.character(Y),col=as.numeric(Y)+1)
# plot(ssn$u[,2:3],pch=as.character(Y),col=as.numeric(Y)+1)

# #######
# par(mfrow=c(2,2))
# for (zz in (0:1)) {
#   iz<-sample(seq(1,dim(X)[1])[Y==zz],1)
#   image(matrix(CATSnDOGS[iz,4096:1],64,64,byrow=T),col=gray.colors(256,0,1))
# }
##################################################################
#### Decomposition with SVD
##################################################################

library(svdvis)
svd.obj <- svd(X)
svd.scree(svd.obj, subr=5)
#### Plot in 3D the component  
library(plot3D)
library(rgl)
library(irlba)

# Even if there is a lot of overlapping the first 3 component
# it is much better than the plot with the last component 
# or a random sample of feature of X
plot3d(svd.obj$u[,1:3],col=as.numeric(Y)+1)
plot3d(svd.obj$u[,190:193],col=as.numeric(Y)+1)
plot3d(X[,sample(seq(1,dim(X)[2]),3)],col=as.numeric(Y)+1)

##################################################################
#### Image compression using SVD 
##################################################################

flipedMat <- X[,4096:1]
example<-sample(seq(1,n),1)
ss<-svd(matrix(flipedMat[example,],64,64,byrow=T))
par(mfrow=c(3,3),pty="s")
par(cex = 0.6)
par(mar = c(1, 1, 0, 0), oma = c(1, 1, 1, 1))
# first one is the full image then using the 9,8,..,2 singular value.
image(t(matrix(flipedMat[example,],64,64)),col=gray.colors(256,0,1))
for (nSVD in 9:2) {
  s_value <- 1:nSVD # sample(seq(1,16),nSVD)
  image(ss$u[,s_value]%*%diag(ss$d[s_value])%*%t(ss$v[,s_value]),col=gray.colors(256,0,1))
}



##################################################################
####  Random forest 
##################################################################

# library(randomForest)
# library(plotmo)
# CatNdog <- data.frame(X[1:2,],class=Y[1:2])
# SA.rf<-randomForest(class~.,data=CatNdog,importance=TRUE,proximity=TRUE)
# plotmo(SA.rf)

# #### Classification
# library(rpart)
# library(rpart.plot)
# 
# ##########
# library(randomForest)
# library(plotmo)
# CatNdog <- data.frame(X,class=Y)
# Xsample<-sample(seq(1,dim(X)[1]),170)
# SA.rf<-randomForest(class~.,data=CatNdog[Xsample,])
# 
# plot(SA.rf)
# print(SA.rf$confusion)



##################################################################
#### SOMS
##################################################################

library(kohonen)

som_model <- som(X, 
                 grid=somgrid(5,5), 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood="circular" )
plot(som_model,type="mapping",col=as.numeric(Y),pch=as.numeric(Y))
plot(som_model, type="changes")
plot(som_model,type="count")
plot(som_model,type="dist.neighbours")
plot(som_model,type="codes")

som_cluster <- cutree(hclust(dist(som_model$codes)), 3)
plot(som_model, type="mapping", bgcol = som_cluster, main = "Clusters")


##################################################################
######################    Clustering     #########################
##################################################################


##################################################################
#### Kmeans
##################################################################
groups <- sample(rep(1:2,times = ceiling(nrow(X) / 2)),size = nrow(X), replace = F)
A <- X[groups%in%1,]
B <- X[groups%in%2,] 
kA<-kmeans(A,2)
kB<-kmeans(B,2)
cA <- factor(kA$cluster)
cB <- factor(kB$cluster)
modelA <- svm(A,cA)
modelB <- svm(B,cB)
# test with group A on cB and group B on cA.
predA <- predict(modelA,B)
predB <- predict(modelB,A)
acc.f <- function(pred,label) {
  length((pred==label)[(pred==label)==T])/length(label) 
}
cluster.accuracy <- mean(c(acc.f(predA, cB),acc.f(predB,cA)))
print(cluster.accuracy)


### Test the stability for the clusters
n.kmeans.test <- 20
assigment <- matrix(,nrow = n, ncol = n.kmeans.test)

firstIndex <- sample(1:n, 1)
image(matrix(CATSnDOGS[firstIndex,4096:1],64,64,byrow=T),col=gray.colors(256,0,1))
for (i in 1:n.kmeans.test) {
  km<-kmeans(X,2)
  firstvalue <- km$cluster[1]
  c1<-km$cluster==firstvalue
  km$cluster[c1==T]=0
  km$cluster[c1==F]=1
  
  assigment[,i]= km$cluster
}
par(mfrow=c(1,1))
print(assigment)
# boxplot(t(assigment))
plot(colMeans(t(assigment)))

## print outlier 183
outlier <- identify(colMeans(t(assigment)))
for (i in outlier) {
  image(matrix(CATSnDOGS[i,4096:1],64,64,byrow=T),col=gray.colors(256,0,1))
}

for (i in 1:10) {
  kk<-kmeans(X,2)
  a1 <- acc.f(kk$cluster-1,Y)
  kk$cluster[kk$cluster==2]=0
  a2 <- acc.f(kk$cluster,Y)
  print(max(c(a1,a2)))
}

### Assess cluster stability 
n.kmeans.test <- 20
assigment <- matrix(,nrow = n, ncol = n.kmeans.test)
for (i in 1:n.kmeans.test) {
  km<-kmeans(X,2)
  assigment[,i]=as.numeric(km$cluster-1==Y)
}
par(mfrow=c(1,1))
print(assigment)
plot(colMeans(t(assigment)))

##################################################################
#### PAM method 
##################################################################
pam_model<-pam(dist(X),2)
table(pam_model$cluster,Y)
W<-rep(0,10)
for (k in 2:20) {
  pam_model<-pam(dist(X),k)
  W[k-1]<-pam_model$sil$avg
}
plot(2:10, W[2:10], type= "l", main = "pam() clustering assessment",
     xlab= "k  (# clusters)", ylab = "average silhouette width")

##################################################################
#### Hierarchical clustering method 
##################################################################
hh<-hclust(dist(X))
plot(hh,label=Y)






