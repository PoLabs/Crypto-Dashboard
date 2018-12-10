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
  # if(frame == 60){
    dataset.list <- list(Y.df.list)
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
BNBUSDT.new5min.df <- BNBUSDT.new5min.df[1:(nrow(BNBUSDT.new5min.df)-4000),]
ETHUSDT.new5min.df <- fread(file='12.09.2018/smallETHUSDT.csv')#4
ETHUSDT.new5min.df <- ETHUSDT.new5min.df[1:(nrow(ETHUSDT.new5min.df)-4000),]
BNBBTC.old5min.df <- fread(file='BNBBTC-oldBOX.csv')#5
BNBBTC.new5min.df <- fread(file='12.09.2018/smallBNBBTC.csv')#6
BNBBTC.new5min.df <- BNBBTC.new5min.df[1:(nrow(BNBBTC.new5min.df)-4000),]
ETHBTC.old5min.df <- fread(file='ETHBTC-oldBOX.csv')#7
ETHBTC.new5min.df <- fread(file='12.09.2018/smallETHBTC.csv')#8
ETHBTC.new5min.df <- ETHBTC.new5min.df[1:(nrow(ETHBTC.new5min.df)-4000),]
ADABTC.old5min.df <- fread(file='ADABTC-oldBOX.csv')#9
ADABTC.new5min.df <- fread(file='12.09.2018/smallADABTC.csv')#10
ADABTC.new5min.df <- ADABTC.new5min.df[1:(nrow(ADABTC.new5min.df)-4000),]
EOSBTC.old5min.df <- fread(file='EOSBTC-oldBOX.csv')#11
EOSBTC.new5min.df <- fread(file='12.09.2018/smallEOSBTC.csv')#12
EOSBTC.new5min.df <- EOSBTC.new5min.df[1:(nrow(EOSBTC.new5min.df)-4000),]
IOTABTC.old5min.df <- fread(file='IOTABTC-oldBOX.csv')#13
IOTABTC.new5min.df <- fread(file='12.09.2018/smallIOTABTC.csv')#14
IOTABTC.new5min.df <- IOTABTC.new5min.df[1:(nrow(IOTABTC.new5min.df)-4000),]
NEOBTC.new5min.df <- fread(file='12.09.2018/smallNEOBTC.csv')#15
NEOBTC.new5min.df <- NEOBTC.new5min.df[1:(nrow(NEOBTC.new5min.df)-4000),]
TRXBTC.new5min.df <- fread(file='12.09.2018/smallTRXBTC.csv')#16
TRXBTC.new5min.df <- TRXBTC.new5min.df[1:(nrow(TRXBTC.new5min.df)-4000),]
XLMBTC.new5min.df <- fread(file='12.09.2018/smallXLMBTC.csv')#17
XLMBTC.new5min.df <- XLMBTC.new5min.df[1:(nrow(XLMBTC.new5min.df)-4000),]
#new
XLMBTC.old5min.df <- fread(file='XLMBTC-oldBOX.csv')#18
TRXBTC.old5min.df <- fread(file='TRXBTC-oldBOX.csv')#19
BTCUSDT.old5min.df <-fread(file='BTCUSDT-old5mins.csv')#20 fuckedf
BTCUSDT.new1min.df <- fread(file='BTCUSDT-new1mins.csv')#21
BTCUSDT.new5min.df <- fread(file='12.09.2018/smallBTCUSDT.csv')#22
BTCUSDT.new5min.df <- BTCUSDT.new5min.df[1:(nrow(BTCUSDT.new5min.df)-4000),]
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
for (i in 1:32){
  if (i == 1 | i == 2 | i == 5 | i == 20 | i == 6 | i == 8 | i == 14 | i == 26 | i == 32){ #no dupes
    if(i ==5){   no.dupes.list[[i]] <- list(datasetlist[[i]], datasetlist[[i]])   
    }else{     no.dupes.list[[i]] <- datasetlist[[i]] } }
  #3 and 5: 3, 4, 6, 8 also 21,22
  if(i == 3 | i == 4 | i == 21 | i == 22){
    list35 <- list(datasetlist[[i]][1:3430,], datasetlist[[i]][3870:5720,], datasetlist[[i]][5810:35700,], datasetlist[[i]][35750:43140,], datasetlist[[i]][43240:nrow(datasetlist[[i]]),] )  
    no.dupes.list[[i]] <- list35 }
  #7 and 8: 7, 9, 11, 13 also 18, 19, 
  if(i == 7 | i == 9 | i == 11 | i == 13 | i == 18 | i == 19){ 
    list78 <- list(datasetlist[[i]][1:7650,], datasetlist[[i]][8570:nrow(datasetlist[[i]]),])
    no.dupes.list[[i]] <- list78  }
  #1 and 3: 14:17, 12, 10, 
  if(i == 15 | i == 16 | i == 17 | i == 10 | i == 12){
    list13 <- list(datasetlist[[i]][1:1260,], datasetlist[[i]][1700:3535,], datasetlist[[i]][3635:33540,], datasetlist[[i]][33600:40980,], datasetlist[[i]][41075:nrow(datasetlist[[i]]),])
    no.dupes.list[[i]] <- list13
  }
  if(i == 23 | i == 25 | i == 29 | i == 30 | i == 31){
    list61k2k <- list(datasetlist[[i]][1:601,], datasetlist[[i]][1023:2865,], datasetlist[[i]][2960:nrow(datasetlist[[i]]),])# datasetlist[[i]][710:1008,], 
    no.dupes.list[[i]] <- list61k2k
  }
  if(i == 24 | i == 27 | i == 28){
    list142 <- list(datasetlist[[i]][430:2275,], datasetlist[[i]][2365:nrow(datasetlist[[i]]),])#datasetlist[[i]][115:410,], 
    no.dupes.list[[i]] <- list142
    }
}   

#BNBBTC: 5,6
no.dupes.list[[5]][[2]] <- no.dupes.list[[6]]
#ETHBTC: 7, 8
no.dupes.list[[7]][[3]] <- no.dupes.list[[8]]
#ADABTC: 9, 10
no.dupes.list[[9]][[3]] <- no.dupes.list[[10]][[1]]
no.dupes.list[[9]][[4]] <- no.dupes.list[[10]][[2]]
no.dupes.list[[9]][[5]] <- no.dupes.list[[10]][[3]]
#EOSBTC: 11,12 
no.dupes.list[[11]][[3]] <- no.dupes.list[[12]][[1]]
no.dupes.list[[11]][[4]] <- no.dupes.list[[12]][[2]]
no.dupes.list[[11]][[5]] <- no.dupes.list[[12]][[3]]
#iotabtc: 13,14
no.dupes.list[[13]][[3]] <- no.dupes.list[[14]]
#xlmbtc: 17,18
no.dupes.list[[17]][[4]] <- no.dupes.list[[18]][[1]]
no.dupes.list[[17]][[5]] <- no.dupes.list[[18]][[2]]
#trxbtc: 16 19
no.dupes.list[[16]][[4]] <- no.dupes.list[[19]][[1]]
no.dupes.list[[16]][[5]] <- no.dupes.list[[19]][[2]]
#btcusdt: (20),21,22
no.dupes.list[[21]][[4]] <- no.dupes.list[[22]][[1]]
no.dupes.list[[21]][[5]] <- no.dupes.list[[22]][[2]]
no.dupes.list[[21]][[6]] <- no.dupes.list[[22]][[3]]
#bccbtc: 23 4parts
#icxbtc: 24 3parts
#ltcbtc:25 4 parts
#nanobtc: 26 1 part
#omgbtc: 27 3 parts
#ontbtc: 28 3parts
#venbtc: 29 4parts
#XMRbtc: 30 4 parts
#Xrpbtc: 31 4 parts
#XEMbtc: 32 1 part

#1-1ETHUSDT, 1-2BNBUSDT, 3-20BTCUSDT, 4 ETHUSDT, 5BNBYSDT, 6BNBBTC, 7ETHBTC, 9ADABTC, 11EOSBTC, 13IOTABTC, 15NEOBTC, 16XLMBTC, 14TRXBTC,21BTCUSDT, 23->32: BCC, ICX, LTC, NANO, OMG, ONT, VEN, XMR, XRP, XEM
paircombo.list <- list(no.dupes.list[[1]],no.dupes.list[[2]],no.dupes.list[[20]],no.dupes.list[[3]],no.dupes.list[[4]],no.dupes.list[[5]],no.dupes.list[[7]],no.dupes.list[[9]],no.dupes.list[[11]],no.dupes.list[[13]],no.dupes.list[[15]],no.dupes.list[[16]],no.dupes.list[[17]], no.dupes.list[[21]], no.dupes.list[[23]], no.dupes.list[[24]], no.dupes.list[[25]], no.dupes.list[[26]], no.dupes.list[[27]], no.dupes.list[[28]], no.dupes.list[[29]], no.dupes.list[[30]], no.dupes.list[[31]], no.dupes.list[[32]])

finalsets <- list() #pair DF list
for (i in 1:24){
  print(i)
  if(i == 1){
    print('running 60')
    n=60 #group by 60 for both 1 and 2
    sixty.listA <- list(paircombo.list[[1]], paircombo.list[[2]] , paircombo.list[[3]])
    sixty.list <- lapply(sixty.listA, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
    for (dset in 1:length(sixty.listA)){
      v.ntrade <- aggregate(sixty.listA[[dset]][,7],list(rep(1:(nrow(sixty.listA[[dset]])%/%n+1),each=n,len=nrow(sixty.listA[[dset]]))),sum)
      v.volume <- aggregate(sixty.listA[[dset]][,5],list(rep(1:(nrow(sixty.listA[[dset]])%/%n+1),each=n,len=nrow(sixty.listA[[dset]]))),sum)
      sixty.list[[dset]][,7] <- v.ntrade[1:nrow(sixty.list[[dset]]),2] #replace vol and ntrades
      sixty.list[[dset]][,5] <- v.volume[1:nrow(sixty.list[[dset]]),2]       } #agg trades and vol
    print('hr avrg fine')
    for(dset in 1:length(sixty.list)){
      sixty.list[[dset]] <- sixty.list[[dset]][complete.cases(sixty.list[[dset]]),]  }#remove NA
    sixty.list <- FeatEng.list(sixty.list)#feat eng
    print('feat eng fine')
    for(dset in 1:length(sixty.list)){
      sixty.list[[dset]] <- sixty.list[[dset]][complete.cases(sixty.list[[dset]]),]  }#remove NA
    # for(s in 1:3){ #loop sending 1 pair at time to Ybuilder
    templist <- Ybuilder.list(sixty.list, 60)#offset for Y, build dfs
    print('Ys built fine')
    for(s in 1:3){
      finalsets[[s]] <- templist[[s]]  }  }   
  if(i > 3 && i != 24){
    print(paste0('running 12 at num: ', i))
    n=12 #group by 12
    if(i == 18){
      twelve.listA <- list(paircombo.list[[18]], paircombo.list[[24]])
    }else{    twelve.listA <- paircombo.list[[i]] }
    print(length(twelve.listA))
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
    if(i != 18){
      temp6df <- Ybuilder.list(twelve.list, 12)#offset for Y, build dfs
      for(dset in 1:length(temp6df)){#clean NA again
        temp6df[[dset]] <- temp6df[[dset]][complete.cases(temp6df[[dset]]),]  }
      finalsets[[i]] <- temp6df   
    }else{
      print('its 18')
      temp6df <- Ybuilder.list.singles(twelve.list, 12)#offset for Y, build dfs
      print('Ys built fine')
      for(ndf in 1:2){
        for(dset in 1:length(temp6df[[1]][[ndf]])){#clean NA again
          temp6df[[1]][[ndf]][[dset]] <- temp6df[[1]][[ndf]][[dset]][complete.cases(temp6df[[1]][[ndf]][[dset]]),]  }
      }
      finalsets[[i]] <- temp6df[[1]][[1]]   
      finalsets[[24]] <- temp6df[[1]][[2]]
    }
  } }#add to finalsets list

finalsets2 <- finalsets
#now combine ETHUSDT (1,4), BNBUSDT (2,5) BTCUSDTs (3,14)
for(i in 1:3){
  if(i == 1){
    for(j in 1:6){
      print(dim(finalsets2[[i]][[j]]))
      finalsets2[[i]][[j]] <- rbind(finalsets[[1]][[j]], finalsets[[4]][[j]])     }  }
  if(i == 2){
    for(j in 1:6){
      print(dim(finalsets2[[i]][[j]]))
      finalsets2[[i]][[j]] <- rbind(finalsets[[2]][[j]], finalsets[[5]][[j]])     }  }
  if(i == 3){
    for(j in 1:6){
      print(dim(finalsets2[[i]][[j]]))
      finalsets2[[i]][[j]] <- rbind(finalsets[[3]][[j]], finalsets[[14]][[j]])     }  }  }

finalsets11 <- list(finalsets2[[1]], finalsets2[[2]], finalsets2[[3]], finalsets2[[6]], finalsets2[[7]], finalsets2[[8]], finalsets2[[9]], finalsets2[[10]], finalsets2[[11]], finalsets2[[12]], finalsets2[[14]], finalsets2[[15]], finalsets2[[16]], finalsets2[[17]], finalsets2[[18]], finalsets2[[19]], finalsets2[[20]], finalsets2[[21]], finalsets2[[22]], finalsets2[[23]], finalsets2[[24]])
dfname.vector <- c('ETHUSDT', 'BNBUSDT', 'BTCUSDT', 'BNBBTC', 'ETHBTC', 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'XLMBTC', 'TRXBTC', 'BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')





NLP.name <-  c('BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')
#get btc vectors
btc.sent <- fread('12.09.2018/NLPdump/BTCUSDT-NLP.csv')
btc.sent <- btc.sent[,2:3]
names(btc.sent) <- c('raw.sent.btc', 'comment.sent.btc')
print(paste0('btc length sent ', nrow(btc.sent)))
stop <- 0

for(pair in 12:21){  #this will have to loop over all 10 pairs and all 6df fro each
  #get sentvectors
  print(NLP.name[(pair-11)])
  pair.sent <- fread(paste0('12.09.2018/NLPdump/', NLP.name[(pair-11)], '-NLP.csv'))
  pair.sent <- pair.sent[,2:3]
  print(paste0('pair length sent ', nrow(pair.sent)))
  
  for(frame in 1:6){
    #add sentiment x2,  #add btc sentiment x2
    print(paste0('pair data length ', nrow(finalsets11[[pair]][[frame]])))
    smallest <- min(c(nrow(btc.df), nrow(pair.sent), nrow(btc.sent), nrow(finalsets11[[pair]][[frame]])))
    btc.df <- finalsets11[[3]][[frame]][,c(5,6,8,30,32,47)]
    names(btc.df) <- c('btc.price', 'btc.volume', 'btc.ntrade', 'btc.price.12h', 'btc.volume.12h', 'btc.RSI.12h')
    finalsets11[[pair]][[frame]] <- cbind(finalsets11[[pair]][[frame]][((nrow(finalsets11[[pair]][[frame]])-smallest)+1):nrow(finalsets11[[pair]][[frame]]),], pair.sent[((nrow(pair.sent)-smallest)+1):nrow(pair.sent),], btc.sent[((nrow(btc.sent)-smallest)+1):nrow(btc.sent),])
    #btc price  #btc volume  ntrade #12hr btc price  #12 hr btc volume  #12hr btc RSI
    print('sent added fine')
    finalsets11[[pair]][[frame]] <- cbind(finalsets11[[pair]][[frame]], btc.df[((nrow(btc.df)-smallest)+1):nrow(btc.df),])#owr::cbind.fill(finalsets11[[pair]][[frame]], btc.df[(nrow()-):,])
    finalsets11[[pair]][[frame]] <- finalsets11[[pair]][[frame]][complete.cases(finalsets11[[pair]][[frame]]),] 
  }
}





#write csvs
for(Npair in 1:21){
  for(frame in 1:6){
    #build csv file name
    csvname <- paste0('12.09.2018/', dfname.vector[Npair], '-a', frame, '.csv')
    #save
    fwrite(finalsets11[[Npair]][[frame]], file=csvname)
  }e]], file=csvname)
e]], file=csvname)

}





