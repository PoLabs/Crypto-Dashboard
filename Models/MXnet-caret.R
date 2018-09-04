setwd("/home/ubuntu/Crypto/9.03.2018")# setwd('C:/Users/Po/Sync/Crypto/dashboard/model data')
pcklibs <- c("dplyr", "caTools", "Metrics", "rpart", "pROC", "caret", "caretEnsemble", "data.table")#, "xgboost") #"TTR")#"Boruta", #install.packages(c("dplyr", "caTools", "Metrics", "rpart", "pROC", "caret", "caretEnsemble", "data.table"))
lapply(pcklibs, require, character.only=TRUE)
library(mxnet)

pair.vector <- c('TRXBTC', 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'XLMBTC', 'ETHUSDT', 'BNBUSDT', 'BTCUSDT', 'BNBBTC', 'ETHBTC', 'BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')
pair.vector <- c('BTCUSDT')

for(pair in pair.vector){
  
  dataset.list <- list()
  dataset.list[[1]] <- fread(file=paste0(pair, '-a1.csv'))
  dataset.list[[2]] <- fread(file=paste0(pair, '-a2.csv'))
  dataset.list[[3]] <- fread(file=paste0(pair, '-a3.csv'))
  dataset.list[[4]] <- fread(file=paste0(pair, '-a4.csv'))
  dataset.list[[5]] <- fread(file=paste0(pair, '-a5.csv'))
  dataset.list[[6]] <- fread(file=paste0(pair, '-a6.csv'))
  
  for (i in 1:6){    #model: we now have all 6 Y's in combined dfs for BTC
    mx.set.seed(420)
    mxnet_grid_A1 = expand.grid(layer1 = c(10, 18),   #layer1, layer2, layer3, learning.rate, momentum, dropout, activation
                                layer2 = c(4, 6),
                                layer3 = c(2),
                                learningrate = c(0.001, 0.01),
                                dropout = c(0.1),
                                beta1 = .9,
                                beta2 = 0.999,
                                activation = 'relu')
    ###################
    #XGBoost tune grid
    xgb_grid_A1 = expand.grid(nrounds = c(1400), eta = c(0.01, 0.005), max_depth = c(12), subsample = c(1), gamma = c(1, 0.5), colsample_bytree = c(1), min_child_weight = c(1, 0.5) )
    #################
    #SVM-RBF tuner
    svmrbf_grid_A1 = expand.grid(C=c(4), sigma=c(0.01, 0.1)) #method = 'svmRadial'
    ###################
    #random forest tune grid
    rf_grid_A1 = expand.grid(mtry = c(6))
    
    #Ensemble
    Ensemble_control_A1 <- trainControl(method = "repeatedcv",  number = 5,  repeats = 1,  verboseIter = TRUE,  returnData = FALSE, trim=TRUE, returnResamp = "all", 
                                        classProbs = TRUE, summaryFunction = twoClassSummary,  savePredictions = TRUE,  allowParallel = TRUE,  sampling = "up")
    
    yE = dataset.list[[i]][,1]
    xE = data.matrix(dataset.list[[i]][,-1])
    yEf <- yE
    yEf <- ifelse(yE == 0, "no", "yes") 
    yEf <- factor(yEf)
    
    Ensemble_list_A1 <- caretList(preProcess=c("center", "scale"),  x=xE,  y=yEf,  trControl=Ensemble_control_A1,  metric="ROC",  methodList=c("glm", "rpart", "bayesglm"),
                                 # tuneList=list(xgbA1=caretModelSpec(method="xgbTree", tuneGrid=xgb_grid_A1), svmA1=caretModelSpec(method="svmRadialSigma", tuneGrid=svmrbf_grid_A1),rfA1=caretModelSpec(method="rf", tuneGrid=rf_grid_A1) )) 
                                  tuneList=list(mxA1=caretModelSpec(method="mxnetAdam", tuneGrid=mxnet_grid_A1, num.round=500, ctx=mx.gpu()) ))
    
    Ensemble_greedy_A1 <- caretEnsemble(Ensemble_list_A1, metric="ROC", trControl=trainControl(number=5, summaryFunction=twoClassSummary, trim=TRUE, returnData=FALSE, classProbs=TRUE ))
    summary(Ensemble_greedy_A1)
    
    CaretListName <- paste0('A', i, '-',pair,'-modellist')
    EnsembleName <- paste0('A', i, '-',pair,'-ensemble')
    saveRDS(Ensemble_list_A1, file=CaretListName)
    saveRDS(Ensemble_greedy_A1, file=EnsembleName)    # readRDS(file='A5-ETHBTC-ensemble')
  }
}
