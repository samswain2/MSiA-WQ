####Illustration of K-NN for Gas Mileage data
library(scatterplot3d)
library(rgl)
GAS<-read.csv("Gas_Mileage.csv",header=TRUE)
GAS1<-GAS
GAS1[,2:12]<-sapply(GAS1[,2:12], function(x) (x-mean(x[!is.na(x)]))/sd(x[!is.na(x)]))
GAS[1:10,]
attach(GAS1)
GAS1[c(1,2,6)]
plot3d(Displacement,Rear_axle_ratio,Mpg)
###
plot(Displacement,Rear_axle_ratio,type="p")
###
identify(Displacement,Rear_axle_ratio)

####K-NN for Concrete data
library(yaImpute)
CRT<-read.csv("concrete.csv", header=TRUE)
CRT1<-CRT
CRT1[1:8]<-sapply(CRT1[1:8], function(x) (x-mean(x))/sd(x)) #standardize predictors
CRT1[9]<-(CRT1[9]-min(CRT1[9]))/(max(CRT1[9])-min(CRT1[9]))
train<-as.matrix(CRT1[,1:8])
test<-as.matrix(CRT1[,1:8])
ytrain<-CRT1[,9]
ytest<-CRT1[,9]
K=3
out<-ann(train,test,K)
ind<-as.matrix(out$knnIndexDist[,1:K])
D<-as.matrix(out$knnIndexDist[,(1+K):(2*K)])
fit<-apply(ind,1,function(x) mean(ytrain[x]))
plot(fit,ytest)
1-var(ytest-fit)/var(ytest)

####CV to choose the best K
Nrep<-50 #number of replicates of CV
K<-10  #K-fold CV on each replicate
n.models = 2 #number of different models to fit
n=nrow(CRT1)
y<-CRT1$Strength
yhat=matrix(0,n,n.models) 
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K)
  for (k in 1:K) {
    train<-as.matrix(CRT1[-Ind[[k]],1:8])
    test<-as.matrix(CRT1[Ind[[k]],1:8])
    ytrain<-CRT1[-Ind[[k]],9]
    K1=3;K2=4
    out<-ann(train,test,K1,verbose=F)
    ind<-as.matrix(out$knnIndexDist[,1:K1])
    yhat[Ind[[k]],1]<-apply(ind,1,function(x) mean(ytrain[x]))
    out<-ann(train,test,K2,verbose=F)
    ind<-as.matrix(out$knnIndexDist[,1:K2])
    yhat[Ind[[k]],2]<-apply(ind,1,function(x) mean(ytrain[x]))
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSEAve <- apply(MSE,2,mean); MSEAve #averaged mean square CV error
MSEsd <- apply(MSE,2,sd); MSEsd   #SD of mean square CV error
r2<-1-MSEAve/var(y); r2  #CV r^2
plot(yhat[,2],y)


####K-NN for FGL Data
FGL<-read.table("fgl.txt",sep="\t")
z<-(FGL$type == "WinF") | (FGL$type == "WinNF")
y<-as.character(FGL$type)
y[z]<-"Win"; y[!z]<-"Other"
FGL<-data.frame(FGL,"type_bin"=as.factor(y))  #add a binary factor response column
y[y == "Win"]<-1;y[y == "Other"]<-0;
FGL<-data.frame(FGL,"type01"=as.numeric(y))  #also add a binary numeric response column
FGL1<-FGL
FGL1[1:9]<-sapply(FGL1[1:9], function(x) (x-mean(x))/sd(x)) #standardize predictors
train<-as.matrix(FGL1[,1:9]); test<-as.matrix(FGL1[,1:9])
ytrain<-FGL1[,11]; ytest<-FGL1[,11]
K=5
out<-ann(train,test,K)
ind<-as.matrix(out$knnIndexDist[,1:K])
phat<-apply(ind,1,function(x) sum(ytrain[x]=="Win")/length(ytrain[x]))
plot(phat,jitter(as.numeric(ytest=="Win"),amount=.05))
1-sum((ytrain=="Win") == (phat > 0.5))/nrow(FGL) #training misclass rate
####can alternatively use the following
library(class)
out<-knn(train, test, ytrain, k = 10, prob = T)

####Local linear regression for concrete data
CRT<-read.csv("concrete.csv", header=TRUE)
CRT1<-CRT
CRT1[1:8]<-sapply(CRT1[1:8], function(x) (x-mean(x))/sd(x)) #standardize predictors
CRT1[9]<-(CRT1[9]-min(CRT1[9]))/(max(CRT1[9])-min(CRT1[9]))
out<-loess(Strength~., CRT1[,c(1,2,4,8,9)],degree=1,span=.3)
summary(out)
names(out)
y<-CRT1[[9]]
yhat<-predict(out)
SSE<-sum((y-yhat)^2);SSE
1-var(y-yhat)/var(y)
plot(yhat,y)

####Using C_p to choose lambda for concrete example
##first find sigma_hat for a low-bias model###
for (lambda in seq(.02,.2,.02)) {out<-loess(Strength ~., CRT1[, c(1,2,4,8,9)],degree=1, span=lambda); print(c(lambda,out$s))}
sig_hat<-0.1

##now find Cp for various lambda###
for (lambda in c(seq(.01,.05,.01), seq(.1,1,.2))) {out<-loess(Strength ~., CRT1[, c(1,2,4,8,9)],degree=1, span=lambda); SSE<-sum((CRT1[,9]-out$fitted)^2); Cp <- (SSE+2*out$trace.hat*sig_hat^2)/nrow(CRT1); print(c(lambda,Cp))}

####GAM fit for concrete data
CRT<-read.csv("concrete.csv", header=TRUE); CRT1<-CRT
CRT1[1:8]<-sapply(CRT1[1:8], function(x) (x-mean(x))/sd(x)) #standardize predictors
CRT1[9]<-(CRT1[9]-min(CRT1[9]))/(max(CRT1[9])-min(CRT1[9]))
library(mgcv)  #stands for Mixed GAM Computation Vehicle
out<-gam(Strength~s(Cement)+s(Slag)+s(FlyAsh)+s(Water)+ s(SPlast) + s(CAgg) + s(FAgg) + s(Age), data=CRT1, family=gaussian(), sp=c(-1,-1,-1,-1,-1,-1,-1,-1)) 
summary(out)
out$sp  ##estimated smoothing parameters for each constituent function 
yhat<-predict(out)
plot(yhat,CRT1$Strength)  #probably quite a bit of overfitting
##
par(mfrow=c(2,4))
plot(out); par(mfrow=c(1,1))  #plot component functions

####PPR fit for concrete data
out<-ppr(Strength~., data=CRT1, nterms=6)
summary(out)
yhat<-predict(out)
plot(yhat,CRT1$Strength)  
##
par(mfrow=c(2,3)); plot(out)  #plot component functions

####CV to Find Best nterms in PPR on Concrete Data
Nrep<-5 #number of replicates of CV
K<-10  #K-fold CV on each replicate
n.models = 2 #number of different models to fit
n=nrow(CRT1)
y<-CRT1$Strength
yhat=matrix(0,n,n.models) 
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K)
  for (k in 1:K) {
    out<- ppr(Strength~., data=CRT1[-Ind[[k]],], nterms=10)  
    yhat[Ind[[k]],1]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
    out<- ppr(Strength~., data=CRT1[-Ind[[k]],], nterms=20)  
    yhat[Ind[[k]],2]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSEAve<- apply(MSE,2,mean); MSEAve #averaged mean square CV error
MSEsd <- apply(MSE,2,sd); MSEsd   #SD of mean square CV error
r2<-1-MSEAve/var(y); r2  #CV r^2
MSE

####Gradient boosting trees for concrete data
library(gbm)
gbm1 <- gbm(Strength~., data=CRT1, var.monotone=rep(0,8), distribution="gaussian", n.trees=5000, shrinkage=0.1, interaction.depth=3, bag.fraction = .5, train.fraction = 1, n.minobsinnode = 10, cv.folds = 10, keep.data=TRUE, verbose=FALSE)
best.iter <- gbm.perf(gbm1,method="cv");best.iter
sqrt(gbm1$cv.error[best.iter]) #CV error SD
1-gbm1$cv.error[best.iter]/var(CRT1$Strength)  #CV r^2
yhat<-predict(gbm1,CRT1, n.trees = best.iter); sd(CRT1$Strength-yhat)  #training error SD
##
summary(gbm1,n.trees=best.iter)  # based on the optimal number of trees
##
par(mfrow=c(2,4))
for (i in c(8,1,4,2,7,5,6,3)) plot(gbm1, i.var = i, n.trees = best.iter)
par(mfrow=c(1,1))
##
plot(gbm1, i.var = c(8,1), n.trees = best.iter)
##
print(pretty.gbm.tree(gbm1,1))  #show the first tree
##
print(pretty.gbm.tree(gbm1,best.iter))  #show the last tree

####Random forest for concrete data
library(randomForest)
rForest1 <- randomForest(Strength~., data=CRT1, mtry=3, ntree = 500, nodesize = 3, importance = TRUE)
plot(rForest1)  #plots OOB mse vs # trees
rForest1 #check the OOB mse and r^2
importance(rForest1); varImpPlot(rForest1)
par(mfrow=c(2,4))
for (i in c(8,1,4,2,7,5,6,3)) partialPlot(rForest1, pred.data=CRT1, x.var = names(CRT1)[i], xlab = names(CRT1)[i], main=NULL) #creates "partial dependence" plots 
par(mfrow=c(1,1))
c(rForest1$mse[rForest1$ntree], sum((rForest1$predicted - CRT1$Strength)^2)/nrow(CRT1)) #both give the OOB MSE

####Boosted tree for INCOME data regression
library(gbm)
XX<-read.table("adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX  #there is no need to standardize the predictors with trees (why not)
Inc.gbm1 <- gbm(hours.per.week ~ ., data=INCOME[,-c(3,4)], distribution="gaussian", n.trees=1000, shrinkage=0.05, interaction.depth=3, bag.fraction = .5, train.fraction = 1, n.minobsinnode = 10, cv.folds = 10, keep.data=TRUE, verbose=FALSE)
best.iter <- gbm.perf(Inc.gbm1,method="cv");best.iter
VarCV<-Inc.gbm1$cv.error[best.iter]; 1-VarCV/var(INCOME$hours.per.week)
summary(Inc.gbm1,n.trees=best.iter)
Inc.gbm1$var.names

plot(Inc.gbm1, i.var = 1, n.trees = best.iter)
plot(Inc.gbm1, i.var = 5, n.trees = best.iter, cex.axis=.7)
plot(Inc.gbm1, i.var = 6, n.trees = best.iter, cex.axis=1)

####Boosted tree for INCOME data classification
Inc.gbm2 <- gbm(income==">50K" ~ ., data=INCOME[,-c(3,4)], distribution="bernoulli", n.trees=5000, shrinkage=0.05, interaction.depth=3, bag.fraction = .5, train.fraction = 1, n.minobsinnode = 10, cv.folds = 10, keep.data=TRUE, verbose=FALSE)
best.iter <- gbm.perf(Inc.gbm2,method="cv");best.iter
Inc.gbm2$cv.error[best.iter]
summary(Inc.gbm2,n.trees=best.iter)
Inc.gbm2$var.names

for (i in c(6,9,3,5,1,4)) {plot(Inc.gbm2, i.var = i, type="response",n.trees = best.iter); readline()}

####Random forest regression for INCOME data
library(randomForest)
XX<-read.table("adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX  #there is no need to standardize the predictors with trees (why not)

rForest <- randomForest(x=INCOME[,-c(3,4,13)], y=INCOME$hours.per.week, data=XX, mtry=4, ntree = 100, nodesize = 50, importance = TRUE)
plot(rForest)  #plots OOB mse vs # trees
rForest #check the OOB mse and r^2
importance(rForest); varImpPlot(rForest)

i=1; partialPlot(rForest, pred.data=XX, x.var = names(XX)[i], xlab = names(XX)[i], main=NULL) #a partial dependence plot for age


