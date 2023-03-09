# import modules
library(reticulate)
library(nnet)


# setup path to anaconda python environment
# replace <PATH_TO_ANACONDA> with the correct path
#use_python('<PATH_TO_ANACONDA>/bin/python/')

CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m<-floor(n/K)  #approximate size of each part
  r<-n-m*K  
  I<-sample(n,n)  #random reordering of the indices
  Ind<-list()  #will be list of indices for all K parts
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] <- I[kpart]  #indices for kth part of data
  }
  Ind
}

crt <- read.csv('../data/concrete.csv')
# standardize all variables
crt <- sapply(crt,function(x) (x-mean(x))/sd(x))

# create a fold partition
set.seed(123)
fold_idxs <- CVInd(nrow(crt),5) # 1 - replicate of 5-fold cross validation

#%%% Hyperopt
# import the library
hyperopt <- import('hyperopt')
hp <- hyperopt$hp
np.random <- import('numpy.random')

## STEP1: Define the search space for hyperparameters
search_space <- list(
  size = hp$quniform('size',2, 40, 1),
  decay = hp$loguniform('decay',log(1e-4),log(1)),
  skip = hp$choice('skip',c(T,F))
)

## STEP2: Define the CV objective

nnet_cv_objective <- function(params){
  # params - list containing the hyperparameters
  
  # setup for cross-validation loop
  # error metric : root mean square error rmse
  K <- length(fold_idxs)
  rmse_folds <- numeric(K)
  
  for(k in 1:length(fold_idxs)){
    # fit a neural network model on training data
    fit_nnet <- nnet(
      Strength ~. ,crt[-fold_idxs[[k]],],linout=T,
      size=params$size,skip=params$skip,decay=params$decay,
      maxit=500,trace=F
    )
    
    # obtain predictions
    y_fold <- predict(fit_nnet,crt[fold_idxs[[k]],])
    
    # compute metric
    rmse_folds[k] <- sqrt(mean((crt[fold_idxs[[k]],'Strength']-y_fold)^2))
  }
  
  # Return metrics - 
  return(list(status = "ok", loss = mean(rmse_folds)))
}


## STEP 3: run the TPE algorithm

set.seed(1)
trials = hyperopt$Trials() # trials object to record losses
best = hyperopt$fmin( # minimize cross-validation loss using hyperopt
  nnet_cv_objective, search_space, trials=trials,
  algo=hyperopt$tpe$suggest, max_evals=30,
  return_argmin=FALSE,
  rstate=np.random$default_rng(as.integer(1))
)


# print best configuration
cat('\n')
print(best)

# extract results from the trials object
tmp <- do.call(cbind, trials$vals)
cv_res <- as.data.frame(
  do.call(rbind,lapply(1:nrow(tmp), function(i) hyperopt$space_eval(search_space,tmp[i,])))
)
cv_res['rmse'] <- sapply(trials$results,`[[`,2)
cv_res['r2'] <- 1-cv_res$rmse^2/var(crt[,'Strength'])

# print top 10 configurations
print(head(cv_res[order(cv_res$rmse),],10))
