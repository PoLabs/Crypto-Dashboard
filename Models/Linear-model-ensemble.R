setwd("/home/polabs1/Crypto/12.09.2018/datasets")# setwd('C:/Users/Po/Sync/Crypto/dashboard/model data')
pcklibs <- c("dplyr", "caTools", "Metrics", "rpart", "pROC", "caret", "caretEnsemble", "data.table", "xgboost", "tictoc") #"TTR")#"Boruta", #install.packages(c("dplyr", "caTools", "Metrics", "rpart", "pROC", "caret", "caretEnsemble", "data.table"))
lapply(pcklibs, require, character.only=TRUE)
set.seed(420420)
# configure multicore
 # library(doMC)
 # registerDoMC(cores=7)

# library(doParallel)
# cl <- makePSOCKcluster(7)
# registerDoParallel(cl)

pair.vector <- c('BTCUSDT')#, 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'XLMBTC', 'ETHUSDT', 'BNBUSDT', 'BNBBTC', 'ETHBTC')
#pair.vector <- c('XLMBTC', 'ETHUSDT', 'BNBUSDT', 'BNBBTC', 'ETHBTC', 'BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')'TRXBTC',
# pair.vector <- c('BTCUSDT')

tic("start time")
for(pair in pair.vector){
  
  dataset.list <- list()
  dataset.list[[1]] <- fread(file=paste0(pair, '-a1.csv'))
  dataset.list[[2]] <- fread(file=paste0(pair, '-a2.csv'))
  dataset.list[[3]] <- fread(file=paste0(pair, '-a3.csv'))
  dataset.list[[4]] <- fread(file=paste0(pair, '-a4.csv'))
  dataset.list[[5]] <- fread(file=paste0(pair, '-a5.csv'))
  dataset.list[[6]] <- fread(file=paste0(pair, '-a6.csv'))
  
  for (i in 1:6){
    print(paste0(pair, ' frame # ', i))
    #model: we now have all 6 Y's in combined dfs for BTC
    ###################
    #XGBoost tune grid
    # xgb_grid_A1 = expand.grid(nrounds = c(1000), eta = c(0.3519),
    #                           max_depth = c(6), subsample = c(0.702977), gamma = c(5.07345), colsample_bytree = c(0.43328), min_child_weight = c(17) )
    # 
    # #################
    # #SVM-RBF tuner
    # svmrbf_grid_A1 = expand.grid(C=c(1.40537), sigma=c(0.13114)) #method = 'svmRadial'
    # 
    # ###################
    # #random forest tune grid
    # rf_grid_A1 = expand.grid(mtry = c(10))

    #Ensemble
    Ensemble_control_A1 <- trainControl(method = "repeatedcv",  number = 10,  repeats = 2,  verboseIter = TRUE,  returnData = FALSE, trim=TRUE, returnResamp = "all", search = "random", #or grid
                                        classProbs = TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE,  allowParallel = TRUE,  sampling = "up")

    yE = dataset.list[[i]][,1]
    xE = data.matrix(dataset.list[[i]][,-1])
    yEf <- yE
    yEf <- ifelse(yE == 0, "no", "yes") 
    yEf <- factor(yEf)
    
    Ensemble_list_A1 <- caretList(preProcess=c("center", "scale"),  x=xE,  y=yEf,  trControl=Ensemble_control_A1,  metric="ROC", methodList=c("glm", "rpart", "bayesglm"), 
                                  tuneList=list(
                                    xgbA1=caretModelSpec(method="xgbTree"), #nrounds = c(200)),#, tuneGrid=xgb_grid_A1 ), 
                                    svmA1=caretModelSpec(method="svmRadialSigma"),#, tuneGrid=svmrbf_grid_A1 ),
                                    rfA1=caretModelSpec(method="rf")))#, tuneGrid=rf_grid_A1) )) #)))#
    
    Ensemble_greedy_A1 <- caretEnsemble(Ensemble_list_A1, metric="ROC", trControl=trainControl(number=5, summaryFunction=twoClassSummary, trim=TRUE, returnData=FALSE, classProbs=TRUE ), tuneLength = 100)
    summary(Ensemble_greedy_A1)
    
    CaretListName <- paste0('A', i, '-',pair,'-modellist')
    EnsembleName <- paste0('A', i, '-',pair,'-ensemble')
    print(CaretListName)
    print(EnsembleName)
    saveRDS(Ensemble_list_A1, file=CaretListName)
    saveRDS(Ensemble_greedy_A1, file=EnsembleName)    # readRDS(file='A5-ETHBTC-ensemble')
  }
}
toc()
# stopCluster(cl)

#timing

#allow parallel
# 10 cv, 0 repeat, btc, xg 1000/0.01/12/1/.5/1/1, svm 4/0.01, rf 6, glm+rpart+bayesglm - upsampling
#PC on: 635.214 secs
#PC off 714.02 secs

#repeated, 15, 10, random search, 10x ensemble, 6 models up: 22722 secs
#  parameter       ROC      Sens      Spec       ROCSD      SensSD      SpecSD
#1      none 0.9643636 0.9333756 0.8784934 0.001237426 0.002510534 0.004713099
#eta = "0.351943538730033", max_depth = "6", gamma = "5.07345240330324", colsample_bytree = "0.433281855005771", min_child_weight = "17", subsample = "0.702977129665669"
#svm: C=1.4053718, sigma=0.1311418
#rf: mtry=10