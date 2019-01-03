setwd('/home/polabs1/Crypto')
pcklibs <- c("dplyr", "caTools", "Metrics", "rpart", "pROC", "caret", "Boruta", "caretEnsemble", "data.table", "TTR")
lapply(pcklibs, require, character.only=TRUE)
money5 <- as.double(0.005)
money1 <- as.double(0.01)
money2 <- as.double(0.02)
FeatEng.list <- function(pairlist){
  for (dset in 1:length(pairlist)){
    #12 unit moving average > vol, close, spread, btc close #SMA(x, n = 10, ...)
    pairlist[[dset]]$SMA12close <- SMA(pairlist[[dset]]$PAIRclose, n = 6)
    pairlist[[dset]]$SMA36close <- SMA(pairlist[[dset]]$PAIRclose, n = 24)
    pairlist[[dset]]$SMA12vol <- SMA(pairlist[[dset]]$PAIRvolume, n = 6)
    pairlist[[dset]]$SMA36vol <- SMA(pairlist[[dset]]$PAIRvolume, n = 24)
    pairlist[[dset]]$SMA12spread <- SMA(pairlist[[dset]]$PAIRspread, n = 6)
    #EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...)
    pairlist[[dset]]$EMA12close <- EMA(pairlist[[dset]]$PAIRclose, n = 6)
    pairlist[[dset]]$EMA36close <- EMA(pairlist[[dset]]$PAIRclose, n = 24)
    pairlist[[dset]]$EMA12vol <- EMA(pairlist[[dset]]$PAIRvolume, n = 6)
    pairlist[[dset]]$EMA36vol <- EMA(pairlist[[dset]]$PAIRvolume, n = 24)
    pairlist[[dset]]$EMA12spread <- EMA(pairlist[[dset]]$PAIRspread, n = 6)
    #EVWMA(price, volume, n = 10, ...)
    pairlist[[dset]]$EVWMA12close <- EVWMA(pairlist[[dset]]$PAIRclose, pairlist[[dset]]$PAIRvolume,  n = 6)
    #VWAP(price, volume, n = 10, ...)
    pairlist[[dset]]$VWAP12close <- VWAP(pairlist[[dset]]$PAIRclose, pairlist[[dset]]$PAIRvolume,  n = 6)
    pairlist[[dset]]$VWAP36close <- VWAP(pairlist[[dset]]$PAIRclose, pairlist[[dset]]$PAIRvolume, n = 24)
    #HMA(x, n = 12, ...)
    pairlist[[dset]]$HMA12close <- HMA(pairlist[[dset]]$PAIRclose, n = 6)
    pairlist[[dset]]$HMA36close <- HMA(pairlist[[dset]]$PAIRclose, n = 24)
    pairlist[[dset]]$HMA12vol <- HMA(pairlist[[dset]]$PAIRvolume, n = 6)
    pairlist[[dset]]$HMA36vol <- HMA(pairlist[[dset]]$PAIRvolume, n = 24)
    #RSI(price, n = 14, maType, ...)     EMA..?
    pairlist[[dset]]$RSIPAIR12 <- RSI(pairlist[[dset]]$PAIRclose, n=6)
    pairlist[[dset]]$RSIPAIR36 <- RSI(pairlist[[dset]]$PAIRclose, n=24)
    #MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)
    MACDpriceE <- MACD(pairlist[[dset]]$PAIRclose, nFast = 6, nSlow = 24, nSig = 3, maType="EMA", percent = TRUE)
    pairlist[[dset]]$MACDpriceE <- MACDpriceE[,1]
    MACDvolE <- MACD(pairlist[[dset]]$PAIRvolume, nFast = 6, nSlow = 24, nSig = 3, maType="EMA", percent = TRUE)
    pairlist[[dset]]$MACDvolE <- MACDvolE[,1]
    MACDspreadE <- MACD(pairlist[[dset]]$PAIRspread, nFast = 6, nSlow = 24, nSig = 3, maType="EMA", percent = TRUE)
    pairlist[[dset]]$MACDspreadE <- MACDspreadE[,1] 
  }
  return(pairlist)
}

#need a second feat eng for BTC features

Ybuilder.list <- function(pairlist, frame){
  L24.list <- list()
  Y.df.list <- list()
  for (dset in 1:length(pairlist)){
    L24 <- data.frame(matrix(ncol=8, nrow=nrow(pairlist[[dset]])-24))
    L24[,1] <- pairlist[[dset]][25:nrow(pairlist[[dset]]),4]
    colnames(L24) <- c("lag24close", "lag24percent", "Y5", "Y1", "Y2", "Yn5", "Yn1", "Yn2")
    L24$lag24percent <- (L24$lag24close - pairlist[[dset]][1:(nrow(pairlist[[dset]])-24),4]) / pairlist[[dset]][1:(nrow(pairlist[[dset]])-24),4]
    L24$Y5 <- ifelse (L24$lag24percent > money5, 1, 0)
    L24$Y1 <- ifelse (L24$lag24percent > 0-money1, 1, 0)
    L24$Y2 <- ifelse (L24$lag24percent > money2, 1, 0)
    L24$Yn5 <- ifelse (L24$lag24percent < 0-money5, 1, 0)
    L24$Yn1 <- ifelse (L24$lag24percent < money1, 1, 0)
    L24$Yn2 <- ifelse (L24$lag24percent < 0-money2, 1, 0)    
    L24.list[[dset]] <- L24  }
  for (dset in 1:length(pairlist)){
    Y.list <- list()   #build new df with BinanceData and future vector --->  
    A1.df <- cbind(L24.list[[dset]]$Y5, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ])   
    names(A1.df)[1] <- "A1"
    A1.df <- A1.df[sample(nrow(A1.df)),]#shuffle
    Y.list[[1]] <- A1.df  
    A2.df <- cbind(L24.list[[dset]]$Y1, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A2.df)[1] <- "A2"
    A2.df <- A2.df[sample(nrow(A2.df)),]#shuffle
    Y.list[[2]] <- A2.df 
    A3.df <- cbind(L24.list[[dset]]$Y2, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A3.df)[1] <- "A3"
    A3.df <- A3.df[sample(nrow(A3.df)),]#shuffle
    Y.list[[3]] <- A3.df 
    A4.df <- cbind(L24.list[[dset]]$Yn5, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A4.df)[1] <- "A4"
    A4.df <- A4.df[sample(nrow(A4.df)),]#shuffle
    Y.list[[4]] <- A4.df 
    A5.df <- cbind(L24.list[[dset]]$Yn1, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A5.df)[1] <- "A5"
    A5.df <- A5.df[sample(nrow(A5.df)),]#shuffle
    Y.list[[5]] <- A5.df 
    A6.df <- cbind(L24.list[[dset]]$Yn2, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A6.df)[1] <- "A6"
    A6.df <- A6.df[sample(nrow(A6.df)),]#shuffle
    Y.list[[6]] <- A6.df 
    Y.df.list[[dset]] <- Y.list   }       #will be list of length datasets each with 6 dfs
  for(dset in 1:length(Y.df.list)){     #remove NA's
    for(dfs in 1:6){
      Y.df.list[[dset]][[dfs]] <- Y.df.list[[dset]][[dfs]][complete.cases(Y.df.list[[dset]][[dfs]]),]  } }
  #if frame = 60combine different: 60 will be list of 3 lists of 6df, others are just list of 6df
  dataset.list <- list()  #combine datasets  (Y.df.list is a list of X length of 6 datasets
  if(frame == 60){
    dataset.list <- Y.df.list
  }
  if(frame == 12){
    for(y in 1:6){
      for(dset in 1:length(Y.df.list)){
        if(dset == 1){          
          holddf <- Y.df.list[[dset]][[y]]
        }else{holddf <- rbind(holddf, Y.df.list[[dset]][[y]])}    #on this loop grab X dfs for 1-6 times
      }
      dataset.list[[y]] <- holddf  
    }
  }
  return(dataset.list)
}
Ybuilder.list.singles <- function(pairlist, frame){
  L24.list <- list()
  Y.df.list <- list()
  for (dset in 1:length(pairlist)){
    L24 <- data.frame(matrix(ncol=8, nrow=nrow(pairlist[[dset]])-24))
    L24[,1] <- pairlist[[dset]][25:nrow(pairlist[[dset]]),4]
    colnames(L24) <- c("lag24close", "lag24percent", "Y5", "Y1", "Y2", "Yn5", "Yn1", "Yn2")
    L24$lag24percent <- (L24$lag24close - pairlist[[dset]][1:(nrow(pairlist[[dset]])-24),4]) / pairlist[[dset]][1:(nrow(pairlist[[dset]])-24),4]
    L24$Y5 <- ifelse (L24$lag24percent > money5, 1, 0)
    L24$Y1 <- ifelse (L24$lag24percent > 0-money1, 1, 0)
    L24$Y2 <- ifelse (L24$lag24percent > money2, 1, 0)
    L24$Yn5 <- ifelse (L24$lag24percent < 0-money5, 1, 0)
    L24$Yn1 <- ifelse (L24$lag24percent < money1, 1, 0)
    L24$Yn2 <- ifelse (L24$lag24percent < 0-money2, 1, 0)    
    L24.list[[dset]] < - L24  }
  for (dset in 1:length(pairlist)){
    Y.list <- list()   #build new df with BinanceData and future vector --->  
    A1.df <- cbind(L24.list[[dset]]$Y5, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ])   
    names(A1.df)[1] <- "A1" 
    A1.df <- A1.df[sample(nrow(A1.df)),]#shuffle
    Y.list[[1]] <- A1.df  
    A2.df <- cbind(L24.list[[dset]]$Y1, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A2.df)[1] <- "A2"
    A2.df <- A2.df[sample(nrow(A2.df)),]#shuffle
    Y.list[[2]] <- A2.df 
    A3.df <- cbind(L24.list[[dset]]$Y2, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A3.df)[1] <- "A3"
    A3.df <- A3.df[sample(nrow(A3.df)),]#shuffle
    Y.list[[3]] <- A3.df 
    A4.df <- cbind(L24.list[[dset]]$Yn5, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A4.df)[1] <- "A4"
    A4.df <- A4.df[sample(nrow(A4.df)),]#shuffle
    Y.list[[4]] <- A4.df 
    A5.df <- cbind(L24.list[[dset]]$Yn1, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A5.df)[1] <- "A5"
    A5.df <- A5.df[sample(nrow(A5.df)),]#shuffle
    Y.list[[5]] <- A5.df 
    A6.df <- cbind(L24.list[[dset]]$Yn2, pairlist[[dset]][1:(nrow(pairlist[[dset]])-24), ]) 
    names(A6.df)[1] <- "A6"
    A6.df <- A6.df[sample(nrow(A6.df)),]#shuffle
    Y.list[[6]] <- A6.df 
    Y.df.list[[dset]] <- Y.list   }       #will be list of length datasets each with 6 dfs
  for(dset in 1:length(Y.df.list)){     #remove NA's
    for(dfs in 1:6){
      Y.df.list[[dset]][[dfs]] <- Y.df.list[[dset]][[dfs]][complete.cases(Y.df.list[[dset]][[dfs]]),]  } }
  #if frame = 60combine different: 60 will be list of 3 lists of 6df, others are just list of 6df
  dataset.list <- list()  #combine datasets  (Y.df.list is a list of X length of 6 datasets
  # if(frame == 60){
  dataset.list <- Y.df.list
  # }
  # if(frame == 12){
  #   for(y in 1:6){
  #     for(dset in 1:length(Y.df.list)){
  #       if(dset == 1){          
  #         holddf <- Y.df.list[[dset]][[y]]
  #       }else{holddf <- rbind(holddf, Y.df.list[[dset]][[y]])}    #on this loop grab X dfs for 1-6 times
  #     }
  #     dataset.list[[y]] <- holddf  
  #   }
  # }
  return(dataset.list)
}


#1. read in data sets
##########################
BNBUSDT.new1min.df <- fread(file='BNBUSDT-oldBOX.csv')#1
ETHUSDT.new1min.df <- fread(file='ETHUSDT-oldBOX.csv')#2
BNBUSDT.new5min.df <- fread(file='12.09.2018/smallBNBUSDT.csv')#3
BNBUSDT.new5min.df <- BNBUSDT.new5min.df[(nrow(BNBUSDT.new5min.df)-4000):nrow(BNBUSDT.new5min.df),]
ETHUSDT.new5min.df <- fread(file='12.09.2018/smallETHUSDT.csv')#4
ETHUSDT.new5min.df <- ETHUSDT.new5min.df[(nrow(ETHUSDT.new5min.df)-4000):nrow(ETHUSDT.new5min.df),]
BNBBTC.old5min.df <- fread(file='BNBBTC-oldBOX.csv')#5
BNBBTC.new5min.df <- fread(file='12.09.2018/smallBNBBTC.csv')#6
BNBBTC.new5min.df <- BNBBTC.new5min.df[(nrow(BNBBTC.new5min.df)-4000):nrow(BNBBTC.new5min.df),]
ETHBTC.old5min.df <- fread(file='ETHBTC-oldBOX.csv')#7
ETHBTC.new5min.df <- fread(file='12.09.2018/smallETHBTC.csv')#8
ETHBTC.new5min.df <- ETHBTC.new5min.df[(nrow(ETHBTC.new5min.df)-4000):nrow(ETHBTC.new5min.df),]
ADABTC.old5min.df <- fread(file='ADABTC-oldBOX.csv')#9
ADABTC.new5min.df <- fread(file='12.09.2018/smallADABTC.csv')#10
ADABTC.new5min.df <- ADABTC.new5min.df[(nrow(ADABTC.new5min.df)-4000):nrow(ADABTC.new5min.df),]
EOSBTC.old5min.df <- fread(file='EOSBTC-oldBOX.csv')#11
EOSBTC.new5min.df <- fread(file='12.09.2018/smallEOSBTC.csv')#12
EOSBTC.new5min.df <- EOSBTC.new5min.df[(nrow(EOSBTC.new5min.df)-4000):nrow(EOSBTC.new5min.df),]
IOTABTC.old5min.df <- fread(file='IOTABTC-oldBOX.csv')#13
IOTABTC.new5min.df <- fread(file='12.09.2018/smallIOTABTC.csv')#14
IOTABTC.new5min.df <- IOTABTC.new5min.df[(nrow(IOTABTC.new5min.df)-4000):nrow(IOTABTC.new5min.df),]
NEOBTC.new5min.df <- fread(file='12.09.2018/smallNEOBTC.csv')#15
NEOBTC.new5min.df <- NEOBTC.new5min.df[(nrow(NEOBTC.new5min.df)-4000):nrow(NEOBTC.new5min.df),]
TRXBTC.new5min.df <- fread(file='12.09.2018/smallTRXBTC.csv')#16
TRXBTC.new5min.df <- TRXBTC.new5min.df[(nrow(TRXBTC.new5min.df)-4000):nrow(TRXBTC.new5min.df),]
XLMBTC.new5min.df <- fread(file='12.09.2018/smallXLMBTC.csv')#17
XLMBTC.new5min.df <- XLMBTC.new5min.df[(nrow(XLMBTC.new5min.df)-4000):nrow(XLMBTC.new5min.df),]
#new
XLMBTC.old5min.df <- fread(file='XLMBTC-oldBOX.csv')#18
TRXBTC.old5min.df <- fread(file='TRXBTC-oldBOX.csv')#19
BTCUSDT.old5min.df <-fread(file='BTCUSDT-old5mins.csv')#20 fuckedf
BTCUSDT.new1min.df <- fread(file='BTCUSDT-new1mins.csv')#21
BTCUSDT.new5min.df <- fread(file='12.09.2018/smallBTCUSDT.csv')#22
BTCUSDT.new5min.df <- BTCUSDT.new5min.df[(nrow(BTCUSDT.new5min.df)-4000):nrow(BTCUSDT.new5min.df),]
#newer
BCCBTC.new5min.df <- fread(file='12.09.2018/smallBCCBTC.csv')#22
BCCBTC.new5min.df <- BCCBTC.new5min.df[1:(nrow(BCCBTC.new5min.df)-4000),]
ICXBTC.new5min.df <- fread(file='12.09.2018/smallICXBTC.csv')#22
ICXBTC.new5min.df <- ICXBTC.new5min.df[1:(nrow(ICXBTC.new5min.df)-4000),]
LTCBTC.new5min.df <- fread(file='12.09.2018/smallLTCBTC.csv')#22
LTCBTC.new5min.df <- LTCBTC.new5min.df[1:(nrow(LTCBTC.new5min.df)-4000),]
NANOBTC.new5min.df <- fread(file='12.09.2018/smallNANOBTC.csv')#22
NANOBTC.new5min.df <- NANOBTC.new5min.df[1:(nrow(NANOBTC.new5min.df)-4000),]
OMGBTC.new5min.df <- fread(file='12.09.2018/smallOMGBTC.csv')#22
OMGBTC.new5min.df <- OMGBTC.new5min.df[1:(nrow(OMGBTC.new5min.df)-4000),]
ONTBTC.new5min.df <- fread(file='12.09.2018/smallONTBTC.csv')#22
ONTBTC.new5min.df <- ONTBTC.new5min.df[1:(nrow(ONTBTC.new5min.df)-4000),]
VENBTC.new5min.df <- fread(file='12.09.2018/smallVENBTC.csv')#22
VENBTC.new5min.df <- VENBTC.new5min.df[1:(nrow(VENBTC.new5min.df)-4000),]
XMRBTC.new5min.df <- fread(file='12.09.2018/smallXMRBTC.csv')#22
XMRBTC.new5min.df <- XMRBTC.new5min.df[1:(nrow(XMRBTC.new5min.df)-4000),]
XRPBTC.new5min.df <- fread(file='12.09.2018/smallXRPBTC.csv')#22
XRPBTC.new5min.df <- XRPBTC.new5min.df[1:(nrow(XRPBTC.new5min.df)-4000),]
XEMBTC.new5min.df <- fread(file='12.09.2018/smallXEMBTC.csv')#22
XEMBTC.new5min.df <- XEMBTC.new5min.df[1:(nrow(XEMBTC.new5min.df)-4000),]

datasetlist <- list(BNBUSDT.new1min.df, ETHUSDT.new1min.df, BNBUSDT.new5min.df, ETHUSDT.new5min.df, BNBBTC.old5min.df, BNBBTC.new5min.df, ETHBTC.old5min.df, 
                    ETHBTC.new5min.df, ADABTC.old5min.df, ADABTC.new5min.df, EOSBTC.old5min.df, EOSBTC.new5min.df, IOTABTC.old5min.df, IOTABTC.new5min.df, NEOBTC.new5min.df, 
                    TRXBTC.new5min.df, XLMBTC.new5min.df, XLMBTC.old5min.df, TRXBTC.old5min.df, BTCUSDT.new1min.df, BTCUSDT.new5min.df, BTCUSDT.new5min.df, BCCBTC.new5min.df,
                    ICXBTC.new5min.df, LTCBTC.new5min.df, NANOBTC.new5min.df, OMGBTC.new5min.df, ONTBTC.new5min.df, VENBTC.new5min.df, XMRBTC.new5min.df, XRPBTC.new5min.df, XEMBTC.new5min.df) 

for(i in 1:32){
  if(i == 20){
    datasetlist[[i]] <- datasetlist[[i]][,1:27]
  }else{
    if(ncol(datasetlist[[i]])>30){
      if(colnames(datasetlist[[i]][,28]) %in% c('time', 'TIME')){
        datasetlist[[i]] <- datasetlist[[i]][,1:27]
      }else{
        tempdf1 <- datasetlist[[i]][,1:26]
        tempdf2 <- datasetlist[[i]][,41]
        datasetlist[[i]] <- cbind(tempdf1, tempdf2)  } }
  }
}

dupes.list <- list()
for (i in 1:32){    dupes.list[[i]] <- which(duplicated(datasetlist[[i]]) | duplicated(datasetlist[[i]][nrow(datasetlist[[i]]):1, ]) [nrow(datasetlist[[i]]):1])}
#check 23->31

no.dupes.list <- list()
no.dupes.list[[1]] <- datasetlist[[3]]   #BNBUSDT
no.dupes.list[[2]] <- datasetlist[[4]]   #ETHUSDT
no.dupes.list[[3]] <- datasetlist[[6]]  #BNBTC
no.dupes.list[[4]] <- datasetlist[[8]]    #ETHBTC
no.dupes.list[[5]] <- datasetlist[[10]]   #ADABTC
no.dupes.list[[6]] <- datasetlist[[12]]   #EOSBTC
no.dupes.list[[7]] <- datasetlist[[14]]    #IOTABTC
no.dupes.list[[8]] <- datasetlist[[15]]    #NEOBTC
no.dupes.list[[9]] <- datasetlist[[16]]    #TRX
no.dupes.list[[10]] <- datasetlist[[17]]   #xlm
no.dupes.list[[11]] <- datasetlist[[21]]   #btc

paircombo.list <- no.dupes.list


finalsets <- list() #pair DF list
for (i in 1:length(paircombo.list)){
  print(i)
    print(paste0('running 12 at num: ', i))
    n=12 #group by 12
      twelve.listA <- list()
      twelve.listA[[1]] <- paircombo.list[[i]] 
      twelve.listA[[2]] <- paircombo.list[[i]] 
    print(paste0('length 12.list ', length(twelve.listA)))
    twelve.list <- lapply(twelve.listA, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
    for (dset in 1:length(twelve.listA)){
      v.ntrade <- aggregate(twelve.listA[[dset]][,7],list(rep(1:(nrow(twelve.listA[[dset]])%/%n+1),each=n,len=nrow(twelve.listA[[dset]]))),sum)
      v.volume <- aggregate(twelve.listA[[dset]][,5],list(rep(1:(nrow(twelve.listA[[dset]])%/%n+1),each=n,len=nrow(twelve.listA[[dset]]))),sum)
      twelve.list[[dset]][,7] <- v.ntrade[1:nrow(twelve.list[[dset]]),2] #replace vol and ntrades
      twelve.list[[dset]][,5] <- v.volume[1:nrow(twelve.list[[dset]]),2]       } #agg trades and vol
    for(dset in 1:length(twelve.list)){#remove NA
      twelve.list[[dset]] <- twelve.list[[dset]][complete.cases(twelve.list[[dset]]),]  }
    print('hr avrg fine')
    twelve.list <- FeatEng.list(twelve.list)  #feat eng
    for(dset in 1:length(twelve.list)){  #remove NA
      twelve.list[[dset]] <- twelve.list[[dset]][complete.cases(twelve.list[[dset]]),]  }
    print('feat eng fine')
      temp6df <- Ybuilder.list(twelve.list, 12)#offset for Y, build dfs
      print(paste0('length temp6df: ', length(temp6df)))
      for(dset in 1:length(temp6df)){#clean NA again
        temp6df[[dset]] <- temp6df[[dset]][complete.cases(temp6df[[dset]]),]  }
      finalsets[[i]] <- temp6df   
}

finalsets11 <- finalsets
dfname.vector <- c('BNBUSDT', 'ETHUSDT', 'BNBBTC', 'ETHBTC', 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'TRXBTC', 'XLMBTC', 'BTCUSDT')#, 'BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')


#write csvs
for(Npair in 1:11){
  for(frame in 1:6){
    #build csv file name
    csvname <- paste0('/home/polabs1/Crypto/12.09.2018/test sets/', dfname.vector[Npair], '-a', frame, '.csv')
    #save
    fwrite(as.data.frame(finalsets11[[Npair]][frame]), file=csvname)
  }
}





 # library(ggplot2)
 # ggplot(data=as.data.frame(finalsets11[[11]][1]), aes(x=1:nrow(as.data.frame(finalsets11[[11]][1])), y=as.data.frame(finalsets11[[11]][1])[,4]   )) +
 #     geom_line()+
 #     geom_point()








