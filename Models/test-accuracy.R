#test accuracy
setwd("/home/ubuntu/crypto")
pcklibs <- c("dplyr", "caTools", "Metrics", "rpart", "pROC", "caret", "caretEnsemble", "data.table", "TTR")#"Boruta"
lapply(pcklibs, require, character.only=TRUE)
model.list <- c('glm', 'bayesglm', 'rpart', 'rf', 'xgbTree', 'svmRadialSigma')#, 'ensemble')
test.df <- data.frame(c('a1', 'a2'), c('c', 'd'))

feat.engineer <- function(PAIRdata){
  PAIRdata$SMA12close <- SMA(PAIRdata$PAIRclose, n = 6)
  PAIRdata$SMA36close <- SMA(PAIRdata$PAIRclose, n = 24)
  PAIRdata$SMA12vol <- SMA(PAIRdata$PAIRvolume, n = 6)
  PAIRdata$SMA36vol <- SMA(PAIRdata$PAIRvolume, n = 24)
  PAIRdata$SMA12spread <- SMA(PAIRdata$PAIRspread, n = 6)
  #EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...)
  PAIRdata$EMA12close <- EMA(PAIRdata$PAIRclose, n = 6)
  PAIRdata$EMA36close <- EMA(PAIRdata$PAIRclose, n = 24)
  PAIRdata$EMA12vol <- EMA(PAIRdata$PAIRvolume, n = 6)
  PAIRdata$EMA36vol <- EMA(PAIRdata$PAIRvolume, n = 24)
  PAIRdata$EMA12spread <- EMA(PAIRdata$PAIRspread, n = 6)
  #EVWMA(price, volume, n = 10, ...)
  PAIRdata$EVWMA12close <- EVWMA(PAIRdata$PAIRclose, PAIRdata$PAIRvolume,  n = 6)
  #VWAP(price, volume, n = 10, ...)
  PAIRdata$VWAP12close <- VWAP(PAIRdata$PAIRclose, PAIRdata$PAIRvolume,  n = 6)
  PAIRdata$VWAP36close <- VWAP(PAIRdata$PAIRclose, PAIRdata$PAIRvolume, n = 24)
  #HMA(x, n = 12, ...)
  PAIRdata$HMA12close <- HMA(PAIRdata$PAIRclose, n = 6)
  PAIRdata$HMA36close <- HMA(PAIRdata$PAIRclose, n = 24)
  PAIRdata$HMA12vol <- HMA(PAIRdata$PAIRvolume, n = 6)
  PAIRdata$HMA36vol <- HMA(PAIRdata$PAIRvolume, n = 24)
  #RSI(price, n = 14, maType, ...)     EMA..?
  PAIRdata$RSIPAIR12 <- RSI(PAIRdata$PAIRclose, n=6)
  PAIRdata$RSIPAIR36 <- RSI(PAIRdata$PAIRclose, n=24)
  #MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)
  MACDpriceE <- MACD(PAIRdata$PAIRclose, nFast = 6, nSlow = 24, nSig = 3, maType="EMA", percent = TRUE)
  PAIRdata$MACDpriceE <- MACDpriceE[,1]
  MACDvolE <- MACD(PAIRdata$PAIRvolume, nFast = 6, nSlow = 24, nSig = 3, maType="EMA", percent = TRUE)
  PAIRdata$MACDvolE <- MACDvolE[,1]
  MACDspreadE <- MACD(PAIRdata$PAIRspread, nFast = 6, nSlow = 24, nSig = 3, maType="EMA", percent = TRUE)
  PAIRdata$MACDspreadE <- MACDspreadE[,1] 
  return(PAIRdata) 
}#feature engineering
Ybuilder.list <- function(pair.df){   #a list of X dfs in 
  money5 <- as.double(0.005)
  money1 <- as.double(0.01)
  money2 <- as.double(0.02)
  L24 <- data.frame(matrix(ncol=8, nrow=nrow(pair.df)-24))
  L24[,1] <- pair.df[25:nrow(pair.df),4]
  colnames(L24) <- c("lag24close", "lag24percent", "Y5", "Y1", "Y2", "Yn5", "Yn1", "Yn2")
  L24$lag24percent <- (L24$lag24close - pair.df[1:(nrow(pair.df)-24),4]) / pair.df[1:(nrow(pair.df)-24),4]
  L24$Y5 <- ifelse (L24$lag24percent > money5, 1, 0)
  L24$Y1 <- ifelse (L24$lag24percent > 0-money1, 1, 0)
  L24$Y2 <- ifelse (L24$lag24percent > money2, 1, 0)
  L24$Yn5 <- ifelse (L24$lag24percent < 0-money5, 1, 0)
  L24$Yn1 <- ifelse (L24$lag24percent < money1, 1, 0)
  L24$Yn2 <- ifelse (L24$lag24percent < 0-money2, 1, 0) 
  return(L24)
}# Y dataframe builder

pair.list <- c('BTCUSDT', 'ETHUSDT','BNBUSDT','BNBBTC','ETHBTC','ADABTC','EOSBTC') #'ETHBTC', 'BNBBTC', 
pair.df.list <- list()

for(pair in pair.list){
  csvfolder <- paste0(pair, "/small", pair, ".csv")  #read in csv
  pair.df <- fread(csvfolder)
  pair.df <- pair.df[(nrow(pair.df)-4000):(nrow(pair.df)),]
  
  #feat eng, find actuall +24 price, 
  n <- 12
  vector.df <- pair.df[,1:27]
  veclist <- list(vector.df, vector.df)
  veclist2 <- lapply(veclist, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
  for(dset in 1:2){
    v.ntrade <- aggregate(veclist[[dset]][,7], list(rep(1:(nrow(veclist[[dset]])%/%n+1), each=n, len=nrow(veclist[[dset]]))), sum)
    v.volume <- aggregate(veclist[[dset]][,5], list(rep(1:(nrow(veclist[[dset]])%/%n+1), each=n, len=nrow(veclist[[dset]]))), sum)
    veclist2[[dset]][,7] <- v.ntrade[1:nrow(veclist2[[dset]]),2]    #replace vol and ntrades
    veclist2[[dset]][,5] <- v.volume[1:nrow(veclist2[[dset]]),2]      }
  
  pred.vector <- feat.engineer(veclist2[[1]]) #call feat.eng
  pred.vector <- pred.vector[complete.cases(pred.vector),] 
  pred.varnum <- ncol(pred.vector)  #REMOVE NAs --- done automatically
  pred.L24 <- Ybuilder.list(pred.vector) #call L24
  
  Mlist <- list()
  testpath <- paste0(pair,'/models/test-', pair, '.csv') #read in test.csv if not creation run
  test.df <- as.data.frame(fread(file='test-blank.csv'))
  
  for(m in 1:length(model.list)){
    if(pair != 'BNBBTC'){
      for(x in 1:6){      #load all 6 model times      
        modelloaderA <- paste0(pair, '/models/A', x, '-', pair, '-', model.list[[m]])
        Mlist[[x]] <- readRDS(modelloaderA)      }
    }else{
      for(x in 1:3){      #load all 6 model times      
        modelloaderA <- paste0(pair, '/models/A', x, '-', pair, '-', model.list[[m]])
        Mlist[[x]] <- readRDS(modelloaderA)      }
    }
    
    if(pair != 'BNBBTC'){
      for(y in 1:length(Mlist)){      #take bind of data and 1-6 frames from pred.L24.df to pred.vectorA
        current.df <- cbind(pred.vector[1:(nrow(pred.vector)-24),], pred.L24[,(2+y)])     #compute all 6 times for model
        pred <- predict(Mlist[[y]], newdata=current.df[,1:pred.varnum], type="prob")
        current.df$test <- ifelse(pred[,2]>0.5,1,0)
        current.df$wrong <- ifelse(current.df$test != current.df[,(pred.varnum+1)], 1, 0)
        test.df[y, (m+1)] <- 1-(as.double(sum(current.df$wrong)/nrow(current.df)))
      }
    }else{
      for(y in 1:3){      #take bind of data and 1-6 frames from pred.L24.df to pred.vectorA
        current.df <- cbind(pred.vector[1:(nrow(pred.vector)-24),], pred.L24[,(2+y)])     #compute all 6 times for model
        pred <- predict(Mlist[[y]], newdata=current.df[,1:pred.varnum], type="prob")
        current.df$test <- ifelse(pred[,2]>0.5,1,0)
        current.df$wrong <- ifelse(current.df$test != current.df[,(pred.varnum+1)], 1, 0)
        test.df[y, (m+1)] <- 1-(as.double(sum(current.df$wrong)/nrow(current.df)))
      }
    }
      fwrite(test.df, file=testpath)
    }
}