#######Illustration of car purchase data and likelihood function############
CAR<-read.table("Car.csv",sep=",",header=TRUE)
CAR
glm1 <- glm(y ~ ., family = binomial(link="logit"), data = CAR)
summary(glm1)
p_hat <- predict(glm1, type="response")
data.frame(CAR, p_hat=round(p_hat,3))
attach(CAR); y <- CAR$y
plot(car_age[y==1],income[y==1],col="red", pch=15, xlab="car_age", ylab="income", xlim=c(1,6), ylim=c(10,100))
points(car_age[y==0], income[y==0], col="black", pch=19)
detach(CAR)

###R commands for fitting learning curve example using the general optimizer nlm()
MLC<-read.table("MLC.csv",sep=",",header=TRUE)
x1<-MLC$Location;x2<-MLC$Week;y<-MLC$Efficiency
fn <- function(p) {yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); sum((y-yhat)^2)} 
out<-nlm(fn,p=c(1,0,-.5,-.1),hessian=TRUE)
theta<-out$estimate  #parameter estimates
theta
###we will use the following later, for finding SEs and CIs#######
MSE<-out$minimum/(length(y) - length(theta))  #estimate of the error variance
InfoMat<-out$hessian/2/MSE  #observed information matrix
CovTheta<-solve(InfoMat)
SE<-sqrt(diag(CovTheta))  #standard errors of parameter estimates
MSE
CovTheta
SE

####R commands for bootstrapping response CIs for the manufacturing learning curve
library(boot)   #need to load the boot package
MLC<-read.table("MLC.csv",sep=",",header=TRUE)
MLCfit<-function(Z,i,theta0,x_pred) {
  Zboot<-Z[i,]
  x1<-Zboot[[1]];x2<-Zboot[[2]];y<-Zboot[[3]]
  fn <- function(p) {yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); sum((y-yhat)^2)} 
  out<-nlm(fn,p=theta0)
  theta<-out$estimate 
  y_pred<- theta[1]+theta[2]*x_pred[1]+theta[4]*exp(theta[3]*x_pred[2])} #predicted response
MLCboot<-boot(MLC, MLCfit, R=5000, theta0=c(1,-.05,-.14,-.55), x_pred=c(1,15))
MLCboot
VarYhat<-var(MLCboot$t); VarYhat
SEYhat<-sqrt(VarYhat); SEYhat
plot(MLCboot)  
boot.ci(MLCboot,conf=c(.9,.95,.99),type=c("norm","basic"))

#Bootstrapping response PIs for the manufacturing learning curve (simpler PI)
Yhat0<-MLCboot$t0 #predicted response using actual estimated parameters 
Yhatboot<-MLCboot$t
SEY<-sqrt(var(Yhatboot)+MSE); SEY
c(Yhat0-qnorm(.975)*SEY, Yhat0+qnorm(.975)*SEY) #simpler PI

####K-fold CV for Manu. Learning Curve Data
Nrep<-20 #number of replicates of CV
K<-10  #K-fold CV on each replicate
n.models = 2 #number of different models to fit and compare
FitFun1 <- function(x1,x2,p) p[1]+p[2]*x1+p[4]*exp(p[3]*x2)
FitFun2 <- function(x1,x2,p) p[1]+p[3]*exp(p[2]*x2)
n=nrow(MLC)
y<-MLC$Efficiency
yhat=matrix(0,n,n.models)
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K)
  for (k in 1:K) {
    out<-nls(Efficiency~FitFun1(Location,Week,p),data=MLC[-Ind[[k]],],start=list(p=c(1,-.05,-.15,-.55)))
    yhat[Ind[[k]],1]<-as.numeric(predict(out,MLC[Ind[[k]],]))
    out<-nls(Efficiency~FitFun2(Location,Week,p),data=MLC[-Ind[[k]],],start=list(p=c(1,-.15,-.55)))
    yhat[Ind[[k]],2]<-as.numeric(predict(out,MLC[Ind[[k]],]))
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSE
MSEAve<- apply(MSE,2,mean); MSEAve #averaged mean square CV error
MSEsd <- apply(MSE,2,sd); MSEsd   #SD of mean square CV error
r2<-1-MSEAve/var(y); r2  #CV r^2

####R commands for AIC and CV for a logistic regression model for the car purchase data
library(boot)
CAR<-read.table("Car.csv",sep=",",header=TRUE)
n<-nrow(CAR) 
car.fit<-glm(y~income+car_age,family=binomial(link = "logit"),data=CAR)
summary(car.fit)
AIC<- -2*as.numeric(logLik(car.fit))/n+2*3/n
out<-cv.glm(CAR, car.fit, function(y,phat) -mean(log(phat)*y+log(1-phat)*(1-y)), K=11)
AIC
car.fit$aic/n
out$delta

car.fit<-glm(y~income+car_age+income:car_age, family=binomial(link = "logit"),data=CAR)
summary(car.fit)
AIC<- -2*as.numeric(logLik(car.fit))/n+2*4/n
out<-cv.glm(CAR, car.fit, function(y,phat) -mean(log(phat)*y+log(1-phat)*(1-y)), K=11)
AIC
car.fit$aic/n
out$delta


