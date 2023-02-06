#######R code for reading in the concrete data, converting the response to [0,1] interval, and standardizing predictors##############
CRT <- read.csv("concrete.csv",header=TRUE)
k<-ncol(CRT)-1  #number of predictors
CRT1 <- CRT  #will be standardized and scaled version of data
CRT1[1:k]<-sapply(CRT1[1:k], function(x) (x-mean(x))/sd(x)) #standardize predictors
CRT1[k+1]<-(CRT1[k+1]-min(CRT1[k+1]))/(max(CRT1[k+1])-min(CRT1[k+1]))
CRT[1:10,]
pairs(CRT, cex=.5, pch=16)

#############Fit a neural network model to the CRT1 data####################
library(nnet)
nn1<-nnet(Strength~.,CRT1, linout=T, skip=F, size=10, decay=0.01, maxit=1000, trace=F)
yhat<-as.numeric(predict(nn1))
y<-CRT1[[9]]; e<-y-yhat
plot(yhat,y)
c(sd(y),sd(e))
#repeat but using logistic output function, for which the response MUST BE SCALED TO [0,1] RANGE
nn1<-nnet(Strength~.,CRT1, linout=F, skip=F, size=10, decay=0.01, maxit=1000, trace=F)
yhat<-as.numeric(predict(nn1)) 
y<-CRT1[[9]]; e<-y-yhat
plot(yhat,y)
c(sd(y),sd(e))
##
summary(nn1)

##Now use multiple reps of CV to compare Neural Nets and linear reg models###
Nrep<-3 #number of replicates of CV
K<-3  #K-fold CV on each replicate
n.models = 3 #number of different models to fit
n=nrow(CRT1)
y<-CRT1$Strength
yhat=matrix(0,n,n.models)
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K)
  for (k in 1:K) {
    out<-nnet(Strength~.,CRT1[-Ind[[k]],], linout=F, skip=F, size=15, decay=.01, maxit=1000, trace=F)
    yhat[Ind[[k]],1]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
    out<-nnet(Strength~.,CRT1[-Ind[[k]],], linout=F, skip=F, size=30, decay=0, maxit=1000, trace=F)
    yhat[Ind[[k]],2]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
    out<-lm(Strength~.,CRT1[-Ind[[k]],])
    yhat[Ind[[k]],3]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSE
MSEAve<- apply(MSE,2,mean); MSEAve #averaged mean square CV error
MSEsd <- apply(MSE,2,sd); MSEsd   #SD of mean square CV error
r2<-1-MSEAve/var(y); r2  #CV r^2

#############Visualizing the effects of the predictors####################
nn1<-nnet(Strength~.,CRT1, linout=F, skip=F, size=15, decay=0.1, maxit=1000, trace=F) ##From CV, these are about the best tuning parameters
summary(nn1)

## Use ALEPlot package to create accumulated local effects (ALE) plots
library(ALEPlot)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
par(mfrow=c(2,4))
for (j in 1:8)  {ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=j, K=50, NA.plot = TRUE)
  rug(CRT1[,j]) }  ## This creates main effect ALE plots for all 8 predictors
par(mfrow=c(1,1))

par(mfrow=c(2,2))  ## This creates 2nd-order interaction ALE plots for x1, x2, x8
a=ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(1,2), K=50, NA.plot = TRUE)
a=ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(1,8), K=50, NA.plot = TRUE)
a=ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(2,8), K=50, NA.plot = TRUE)
par(mfrow=c(1,1))

####Combined effect of cement and age
f0 <- mean(CRT1$Strength)
f1 <- ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=1, K=50, NA.plot = TRUE)
f8 <- ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=8, K=50, NA.plot = TRUE) 
f18 <- ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(1,8), K=50, NA.plot = TRUE)
f18.combined <- f0 + outer(f1$f.values,rep(1,13)) + outer(rep(1,51),f8$f.values) + f18$f.values
image(f1$x.values, f8$x.values, f18.combined, xlab="Cement", ylab= "Age", xlim = range(f1$x.values), ylim = range(f8$x.values), xaxs = "i", yaxs = "i")
contour(f1$x.values, f8$x.values, f18.combined, add=TRUE, drawlabels=TRUE)


####Predictive modeling of income data
XX<-read.table("adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX
INCOME[,c(1,5,11,12,13)]<-scale(INCOME[,c(1,5,11,12,13)]) #standardize the continuous variables

#exploring individual variables
summary(INCOME)
par(mfrow=c(2,3)); for (i in c(1,5,11,12,13)) hist(XX[[i]],xlab=names(XX)[i]); plot(XX[[15]])
par(mfrow=c(1,1)); plot(XX[[2]],cex.names=.7)
for (i in c(2,4,6,7,8,9,10,14,15)) print(round(table(XX[[i]])/nrow(XX),3))

#exploring pairwise predictor/response relationships
par(mfrow=c(2,1))
plot(jitter(XX$age,3),jitter(XX$hours.per.week,3),pch=16,cex=.5)
plot(jitter(XX$education.num,3),jitter(XX$hours.per.week,3),pch=16,cex=.5)
par(mfrow=c(1,1))
barplot(tapply(XX$hours.per.week,XX$education.num,mean),ylim=c(30,50),cex.names=.7,xpd=F, xlab="Education.num")
for (i in c(2,4,6,7,8,9,14,15)) {print(round(tapply(XX$hours.per.week,XX[[i]],mean),2)); cat("\n")}

#linear regression with all predictors included
Inc.lm<-lm(hours.per.week ~ .,data=INCOME[,-3])
summary(Inc.lm)

#linear regression including interactions (generally not a good next step)
Inc.lm.full<-lm(hours.per.week ~ .^2,data=INCOME[,-c(3,4,14)])
summary(Inc.lm.full)

#stepwise linear regression
Inc.lm0<-lm(hours.per.week ~ 1,data=INCOME[,-c(3,4)])
Inc.lm<-lm(hours.per.week ~ .,data=INCOME[,-c(3,4)])
Inc.lm.step<-step(Inc.lm0, scope=formula(Inc.lm), direction="both", trace=0)
summary(Inc.lm.step)

#Neural network model
library(nnet)
Inc.nn1<-nnet(hours.per.week ~ . ,INCOME[,-c(3,4)], linout=T, skip=F, size=10, decay=.05, maxit=100, trace=F)
yhat<-as.numeric(predict(Inc.nn1))
y<-INCOME$hours.per.week; e<-y-yhat
1-var(e)/var(y) #training r^2
summary(Inc.nn1)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
ALEPlot(INCOME[,-c(3,4,13)], Inc.nn1, pred.fun=yhat, J=1, K=500, NA.plot = TRUE);


######Read FGL data, convert response to binary, and standardize predictors
FGL<-read.table("fgl.txt",sep="\t")
z<-(FGL$type == "WinF") | (FGL$type == "WinNF")
y<-as.character(FGL$type)
y[z]<-"Win"; y[!z]<-"Other"
FGL<-data.frame(FGL,"type_bin"=as.factor(y))  #add a binary factor response column
y[y == "Win"]<-1;y[y == "Other"]<-0;
FGL<-data.frame(FGL,"type01"=as.numeric(y))  #also add a binary numeric response column
FGL1<-FGL
k<-ncol(FGL)-3;
FGL1[1:k]<-sapply(FGL1[1:k], function(x) (x-mean(x))/sd(x)) #standardize predictors
FGL

#############Fit a neural network classification model to the FGL1 data
library(nnet)
fgl.nn1<-nnet(type_bin~., FGL1[,c(1:9,11)], linout=F, skip=F, size=10, decay=.05, maxit=1000, trace=F)
phat<-as.numeric(predict(fgl.nn1))
y<-FGL1[[12]] 
yhat<-as.numeric(phat >= 0.5)  #classify as 1 if predicted probability >= 0.5
sum(y != yhat)/length(y)  #misclassification rate
summary(fgl.nn1)
plot(phat,jitter(y,0.05))

##Now use multiple reps of CV to compare Neural Nets and logistic reg models###
Nrep<-20 #number of replicates of CV
K<-3  #K-fold CV on each replicate
n.models = 3 #number of different models to fit
n=nrow(FGL1)
y<-FGL1[[12]]
yhat=matrix(0,n,n.models)
CV.rate<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K)
  for (k in 1:K) {
    out<-nnet(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],linout=F,skip=F,size=10,decay=.3, maxit=1000,trace=F)
    phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)]));  yhat[Ind[[k]],1]<-as.numeric(phat >= 0.5)
    out<-nnet(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],linout=F,skip=F,size=10,decay=0, maxit=1000,trace=F)
    phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)]));  yhat[Ind[[k]],2]<-as.numeric(phat >= 0.5)
    out<-glm(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],family=binomial(link="logit"))
    phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)],type="response"));  yhat[Ind[[k]],3]<-as.numeric(phat >= 0.5)
  } #end of k loop
  CV.rate[j,]=apply(yhat,2,function(x) sum(y != x)/n)
} #end of j loop
CV.rate
CV.rateAve<- apply(CV.rate,2,mean); CV.rateAve #averaged CV misclass rate
CV.rateSD <- apply(CV.rate,2,sd); CV.rateSD   #SD of CV misclass rate

#############Visualizing the effects of the predictors####################
fgl.nn1<-nnet(type_bin~., FGL1[,c(1:9,11)], linout=F, skip=F, size=10, decay=.3, maxit=1000, trace=F) ##From CV, these are about the best tuning parameters
summary(fgl.nn1)

## Use ALEPlot package to create accumulated local effects (ALE) plots
library(ALEPlot)
yhat <- function(X.model, newdata) {p.hat = as.numeric(predict(X.model, newdata, type="raw")); log(p.hat/(1-p.hat))}  ## to plot the log-odds
par(mfrow=c(3,3))
for (j in 1:9)  {ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=j, K=50, NA.plot = TRUE)
  rug(FGL1[,j]) }  ## This creates main effect ALE plots for all 9 predictors
par(mfrow=c(1,1))

par(mfrow=c(2,2))  ## This creates 2nd-order interaction ALE plots for x1, x3, x7
ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(1,3), K=50, NA.plot = TRUE)
ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(1,7), K=50, NA.plot = TRUE)
ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(3,7), K=50, NA.plot = TRUE)
par(mfrow=c(1,1))

############Same, but use the original 6-category response######
library(nnet)
fgl.nn1<-nnet(type~.,FGL1[,c(1:10)],linout=F,skip=F,size=10,decay=.05,maxit=1000,trace=F)
##output the class probabilities
phat<-predict(fgl.nn1,type="raw")
phat[1:20,]
apply(phat,1,sum)  #you can see that the 6 predicted class probabilities sum to 1.0
##output the class with the largest class probability
yhat<-predict(fgl.nn1,type="class")
yhat
y<-FGL1$type 
sum(y != yhat)/length(y)  #training misclassification rate

#####Fit classification neural network to income data
XX<-read.table("adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX
INCOME[,c(1,5,11,12,13)]<-scale(INCOME[,c(1,5,11,12,13)]) #standardize the continuous variables 
library(nnet)
Inc.nn1<-nnet(income ~ . ,INCOME[,-c(3,4)], linout=F, skip=F, size=10, decay=.05, maxit=100, trace=F)
y<-INCOME$income
phat<-predict(Inc.nn1, type="raw")   #vector of predicted  probabilities
yhat<-predict(Inc.nn1, type="class")  #vector of predicted classes
sum(y != yhat)/length(y)  #training misclassification rate
plot(phat,jitter(as.numeric(y==">50K"), 1.5))

##compare to a logistic regression model
Inc.glm <- glm(income ~ . ,family = binomial(link = "logit"), data=INCOME[,-c(3,4)])
phat <- predict(Inc.glm, type="response")
summary(Inc.glm)
sum((y == ">50K") != (phat > 0.5))/length(y)

####Regression tree for the concrete data
library(rpart)
control <- rpart.control(minbucket = 5, cp = 0.0001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
CRT.tr <- rpart(Strength ~ .,CRT, method = "anova", control = control)
plotcp(CRT.tr)  #plot of CV r^2 vs. size
printcp(CRT.tr)  #same info is in CRT.tr$cptable
#prune back to optimal size, according to plot of CV 1-r^2
CRT.tr1 <- prune(CRT.tr, cp=0.0015)  #approximately the best size pruned tree
CRT.tr1$variable.importance
CRT.tr1$cptable[nrow(CRT.tr1$cptable),] #shows training and CV 1-r^2, and other things
#prune and plot a little smaller tree than the optimal one, just for display
CRT.tr2 <- prune(CRT.tr, cp=0.007)  #bigger cp gives smaller size tree
CRT.tr2
par(cex=.9); plot(CRT.tr2, uniform=F); text(CRT.tr2, use.n = T); par(cex=1)
##
yhat<-predict(CRT.tr1); e<-CRT$Strength-yhat
c(1-var(e)/var(CRT$Strength), 1-CRT.tr1$cptable[nrow(CRT.tr1$cptable),3]) #check to see training r^2 agrees with what is in cptable


####Classification tree for the FGL data
library(rpart)
control <- rpart.control(minbucket = 1, cp = 0.001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
FGL.tr <- rpart(type_bin~., FGL[,c(1:9,11)], method = "class", control = control)
plotcp(FGL.tr)
printcp(FGL.tr)  #same info in FGL.tr$cptable
#prune to optimal size
FGL.tr1 <- prune(FGL.tr, cp=0.055)  #approximately the cp corresponding to the optimal size
FGL.tr1
par(cex=1); plot(FGL.tr1, uniform=F); text(FGL.tr1, use.n = F); par(cex=1)
FGL.tr1$variable.importance
FGL.tr1$cptable[nrow(FGL.tr1$cptable),] 
#calculate training and CV misclass rates
FGL.tr1$cptable[nrow(FGL.tr1$cptable),c(3,4)]*min(table(FGL$type_bin)/nrow(FGL))  #training and cv misclass rates
yhat<-predict(FGL.tr1, type="class")
sum(yhat != FGL$type_bin)/nrow(FGL) #check the training misclass rate

####Same but for the 6-category response
library(rpart)
control <- rpart.control(minbucket = 1, cp = 0.00001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
FGL.tr <- rpart(type~.,FGL[,c(1:10)], method = "class", control = control)
plotcp(FGL.tr)
printcp(FGL.tr)  #same info in FGL.tr$cptable
#prune to optimal size
FGL.tr1 <- prune(FGL.tr, cp=0.01)  #approximately the cp corresponding to the optimal size
FGL.tr1
par(cex=1); plot(FGL.tr1, uniform=F); text(FGL.tr1, use.n = F); par(cex=1)
FGL.tr1$variable.importance
FGL.tr1$cptable[nrow(FGL.tr1$cptable),] 
#see what the predicted class probabilities are
yhat<-predict(FGL.tr1, type="prob")
yhat[1:20,]


####Regression tree for the income data
library(rpart)
XX<-read.table("adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX  #there is no need to standardize the predictors with trees (why not)
control <- rpart.control(minbucket = 20, cp = 0.0001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
INC.tr <- rpart(hours.per.week ~ ., INCOME[,-c(3,4)], method = "anova", control = control)
plotcp(INC.tr)
printcp(INC.tr) 
#prune back to optimal size, according to plot of CV r^2
INC.tr1 <- prune(INC.tr, cp=0.001)  #approximately the cp corresponding to the best size
INC.tr1
par(cex=.9); plot(INC.tr1, uniform=F); text(INC.tr1, use.n = F); par(cex=1)
INC.tr1$variable.importance
INC.tr1$cptable[nrow(INC.tr1$cptable),] #shows training and CV r^2, and other things


####Classification tree for the income data
library(rpart)
XX<-read.table("adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX  #there is no need to standardize the predictors with trees (why not)
control <- rpart.control(minbucket = 20, cp = 0.00001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
INC.tr <- rpart(income ~ ., INCOME[,-c(3,4)], method = "class", control = control)
plotcp(INC.tr)
printcp(INC.tr) 
#prune back to optimal size, according to plot of CV r^2
INC.tr1 <- prune(INC.tr, cp=0.0007)  #approximately the cp corresponding to the best size
INC.tr1
par(cex=.7); plot(INC.tr1, uniform=F); text(INC.tr1, use.n = F); par(cex=1)
INC.tr1$variable.importance
INC.tr1$cptable[nrow(INC.tr1$cptable),] 
INC.tr1$cptable[nrow(INC.tr1$cptable),][c(3,4)]*min(table(XX$income)/nrow(XX))  #training and cv misclass rates
yhat<-predict(INC.tr1, type="class"); sum(yhat != XX$income)/nrow(XX) #check training misclass rate
