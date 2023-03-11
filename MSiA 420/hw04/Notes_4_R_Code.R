####MA filtering/prediction for chem.csv
chem<-read.csv("chem.csv",header=F)
y<-ts(chem[[1]], frequency=1)  
m=20;k=20;n=length(y)  #m = MA window length, k = prediction horizon
plot(y,type="b",xlim=c(0,n+k))
MAchem<-filter(y, filter=rep(1/m,m), method = "convolution", sides = 1)
yhat=c(NA, MAchem, rep(MAchem[n],k-1))  #One-step-ahead forecasts. The output of MAchem is L_t. The leading NA in yhat gives yhat_t = L_(t-1)
lines(yhat, col="red")

####Manual EWMA filtering/prediction for chem.csv
y<-ts(chem[[1]], frequency=1)
alpha=0.2;k=20;n=length(y)  #alpha = EWMA parameter, k = prediction horizon
plot(y,type="b",xlim=c(0,n+k))
EWMAchem<-filter(alpha*y, filter=1-alpha, method = "recursive", sides = 1, init=y[1])
yhat=c(NA,EWMAchem,rep(EWMAchem[n],k-1))
lines(yhat,col="red")

####EWMA filtering/prediction for chem.csv using HoltWinter()
y<-ts(chem[[1]], frequency=1)  
k=20;n=length(y)  #k = prediction horizon
EWMAchem<-HoltWinters(y, seasonal = "additive", beta = FALSE, gamma = FALSE) 
EWMAchemPred<-predict(EWMAchem, n.ahead=k, prediction.interval = TRUE, level = 0.95)
plot(EWMAchem,EWMAchemPred,type="b")
EWMAchem

####MA and EWMA Not Appropriate for Seasonal Data
trade<-read.csv("trade.csv",header=F)
y<-ts(trade[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
EWMAtrade<-HoltWinters(y, seasonal = "additive", beta = FALSE, gamma = FALSE) 
EWMAtradePred<-predict(EWMAtrade, n.ahead=12, prediction.interval = T, level = 0.95)
plot(EWMAtrade,EWMAtradePred,type="b")
EWMAtrade

####Centered Vs. Noncentered MA
##noncentered MA
par(mfrow=c(2,1))
y<-ts(chem[[1]], frequency=1)  
m=20;n=length(y)  
MAchem<-filter(y, filter=rep(1/m,m), method = "convolution", sides = 1)
plot(y,type="b",xlim=c(0,n))
lines(MAchem,col="red")
##centered MA
y<-ts(chem[[1]], frequency=1)  
m=20;n=length(y)  
MAchem<-filter(y, filter=rep(1/m,m), method = "convolution", sides = 2)
plot(y,type="b",xlim=c(0,n))
lines(MAchem,col="red")
par(mfrow=c(1,1))

####Using m = seasonality period vs. other m
##m = 12 for centered MA
trade<-read.csv("trade.csv",header=F)
y<-ts(trade[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
m=12;n=length(y)  
MAtrade<-filter(y, filter=rep(1/m,m), method = "convolution", sides = 2)
plot(y,type="b")
lines(MAtrade,col="red")
##m = 10 for centered MA
trade<-read.csv("trade.csv",header=F)
y<-ts(trade[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
m=10;n=length(y)  
MAtrade<-filter(y, filter=rep(1/m,m), method = "convolution", sides = 2)
plot(y,type="b")
lines(MAtrade,col="red")

####Double EWMA for Chem Data
#let algorithm find optimal alpha and beta
y<-ts(chem[[1]], frequency=1)  
k=20;n=length(y)  #k = prediction horizon
Holtchem<-HoltWinters(y, seasonal = "additive", gamma = FALSE) 
HoltchemPred<-predict(Holtchem, n.ahead=k, prediction.interval = T, level = 0.95)
plot(Holtchem,HoltchemPred,type="b",ylim=c(16,18))
Holtchem

#repeat, but specifying alpha and larger beta
Holtchem<-HoltWinters(y, seasonal = "additive", alpha=.4, beta=.3,gamma = FALSE, l.start=17, b.start=.03) 
HoltchemPred<-predict(Holtchem, n.ahead=k, prediction.interval = T, level = 0.95)
plot(Holtchem,HoltchemPred,type="b",ylim=c(16,18))

#repeat, but specifying alpha and smaller beta
Holtchem<-HoltWinters(y, seasonal = "additive", alpha=.4, beta=.05,gamma = FALSE, l.start=17, b.start=.03) 
HoltchemPred<-predict(Holtchem, n.ahead=k, prediction.interval = T, level = 0.95)
plot(Holtchem,HoltchemPred,type="b",ylim=c(16,18))

####Holt Method Not Appropriate for Seasonal Data
trade<-read.csv("trade.csv",header=F)
y<-ts(trade[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
k=20;n=length(y)  #k = prediction horizon
EWMAtrade<-HoltWinters(y, seasonal = "additive", gamma = FALSE) 
EWMAtradePred<-predict(EWMAtrade, n.ahead=k, prediction.interval = T, level = 0.95)
plot(EWMAtrade,EWMAtradePred,type="b",ylim=c(300,450))

####Holt-Winters Method
trade<-read.csv("trade.csv",header=F)
y<-ts(trade[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period. Could instead specify frequency = 12
k=24;n=length(y)  #k = prediction horizon
HWtrade<-HoltWinters(y, seasonal = "additive") 
HWtradePred<-predict(HWtrade, n.ahead=k, prediction.interval = T, level = 0.95)
plot(HWtrade,HWtradePred,type="b",ylim=c(300,450))
HWtrade

####Decomposition of trade.csv Data
trade<-read.csv("trade.csv",header=F)
y<-ts(trade[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
k=24;n=length(y)  #k = prediction horizon
Dectrade<-decompose(y, type = "additive") 
plot(Dectrade,type="b")
Dectrade
##
y_hat<-Dectrade$trend+Dectrade$seasonal
plot(y,type="b")
lines(y_hat,col="red")
