setwd('C:/Users/Po/Desktop/crypto data')
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

# Ybuilder.list <- function(pair.dfs){   #a list of X dfs in 
#   #print(length(pair.dfs))
#   L24.list <- list()
#   for (dset in 1:length(pair.dfs)){
#     L24 <- data.frame(matrix(ncol=8, nrow=nrow(pair.dfs[[dset]])-24))
#     L24[,1] <- pair.dfs[[dset]][25:nrow(pair.dfs[[dset]]),4]
#     colnames(L24) <- c("lag24close", "lag24percent", "Y5", "Y1", "Y2", "Yn5", "Yn1", "Yn2")
#     L24$lag24percent <- (L24$lag24close - pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24),4]) / pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24),4]
#     L24$Y5 <- ifelse (L24$lag24percent > money5, 1, 0)
#     L24$Y1 <- ifelse (L24$lag24percent > 0-money1, 1, 0)
#     L24$Y2 <- ifelse (L24$lag24percent > money2, 1, 0)
#     L24$Yn5 <- ifelse (L24$lag24percent < 0-money5, 1, 0)
#     L24$Yn1 <- ifelse (L24$lag24percent < money1, 1, 0)
#     L24$Yn2 <- ifelse (L24$lag24percent < 0-money2, 1, 0)    
#     L24.list[[dset]] <- L24  }          #a list of X dfs w Y vectors for each
#   Y.df.list <- list()                 # will be list of X dfs with Ys
#   for (dset in 1:length(pair.dfs)){
#     Y.list <- list()     
#     # print('ok prior to future vectors')
#     A1.df <- cbind(L24.list[[dset]]$Y5, pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24), ])   
#     names(A1.df)[1] <- "A1"
#     A1.df <- A1.df[sample(nrow(A1.df)),]#shuffle
#     Y.list[[1]] <- A1.df  
#     A2.df <- cbind(L24.list[[dset]]$Y1, pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24), ]) 
#     names(A2.df)[1] <- "A2"
#     A2.df <- A2.df[sample(nrow(A2.df)),]#shuffle
#     Y.list[[2]] <- A2.df 
#     A3.df <- cbind(L24.list[[dset]]$Y2, pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24), ]) 
#     names(A3.df)[1] <- "A3"
#     A3.df <- A3.df[sample(nrow(A3.df)),]#shuffle
#     Y.list[[3]] <- A3.df 
#     A4.df <- cbind(L24.list[[dset]]$Yn5, pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24), ]) 
#     names(A4.df)[1] <- "A4"
#     A4.df <- A4.df[sample(nrow(A4.df)),]#shuffle
#     Y.list[[4]] <- A4.df 
#     A5.df <- cbind(L24.list[[dset]]$Yn1, pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24), ]) 
#     names(A5.df)[1] <- "A5"
#     A5.df <- A5.df[sample(nrow(A5.df)),]#shuffle
#     Y.list[[5]] <- A5.df 
#     A6.df <- cbind(L24.list[[dset]]$Yn2, pair.dfs[[dset]][1:(nrow(pair.dfs[[dset]])-24), ]) 
#     names(A6.df)[1] <- "A6"
#     A6.df <- A6.df[sample(nrow(A6.df)),]#shuffle
#     Y.list[[6]] <- A6.df 
#     Y.df.list[[dset]] <- Y.list   }
#   # print('success building list of 6dfs')       #will be list of length datasets each with 6 dfs
#   for(dset in 1:length(Y.df.list))     {         #remove NA's
#     for(dfs in 1:6){
#       Y.df.list[[dset]][[dfs]] <- Y.df.list[[dset]][[dfs]][complete.cases(Y.df.list[[dset]][[dfs]]),]  } }      #NAs 1 to 6
#   dataset.list <- list()                        #combine datasets  (Y.df.list is a list of X length of 6 datasets)
#   for (y in 1:6){
#     for(dset in 1:length(Y.df.list)){
#       if(dset == 1){
#         # print(paste0('adding 1st, dim: ', dim(Y.df.list[[dset]][[y]])))
#         holddf <- Y.df.list[[dset]][[y]]
#       }else{
#         # print(paste0('adding ', dset, 'st, dim: ', dim(Y.df.list[[dset]][[y]])))
#         # print(colnames(Y.df.list[[dset]][[y]]))
#         holddf <- rbind(holddf, Y.df.list[[dset]][[y]])}    } #}
#     dataset.list[[y]] <- holddf  }
#   print('added to dataset.list fine')
#   return(dataset.list)
# }
# 
# Ybuilder60.list <- function(pair.dfs){   #a list of X dfs in 
#   L24 <- data.frame(matrix(ncol=8, nrow=(nrow(pair.dfs)-24)))
#   L24[,1] <- pair.dfs[25:nrow(pair.dfs),4]
#   colnames(L24) <- c("lag24close", "lag24percent", "Y5", "Y1", "Y2", "Yn5", "Yn1", "Yn2")
#   L24$lag24percent <- (L24$lag24close - pair.dfs[1:(nrow(pair.dfs)-24),4]) / pair.dfs[1:(nrow(pair.dfs)-24),4]
#   L24$Y5 <- ifelse (L24$lag24percent > money5, 1, 0)
#   L24$Y1 <- ifelse (L24$lag24percent > 0-money1, 1, 0)
#   L24$Y2 <- ifelse (L24$lag24percent > money2, 1, 0)
#   L24$Yn5 <- ifelse (L24$lag24percent < 0-money5, 1, 0)
#   L24$Yn1 <- ifelse (L24$lag24percent < money1, 1, 0)
#   L24$Yn2 <- ifelse (L24$lag24percent < 0-money2, 1, 0)    
#   
#   Y.list <- list()     
#   
#   A1.df <- cbind(L24$Y5, pair.dfs[1:(nrow(pair.dfs)-24), ])   
#   names(A1.df)[1] <- "A1"
#   A1.df <- A1.df[sample(nrow(A1.df)),]#shuffle
#   Y.list[[1]] <- A1.df  
#   A2.df <- cbind(L24$Y1, pair.dfs[1:(nrow(pair.dfs)-24), ]) 
#   names(A2.df)[1] <- "A2"
#   A2.df <- A2.df[sample(nrow(A2.df)),]#shuffle
#   Y.list[[2]] <- A2.df 
#   A3.df <- cbind(L24$Y2, pair.dfs[1:(nrow(pair.dfs)-24), ]) 
#   names(A3.df)[1] <- "A3"
#   A3.df <- A3.df[sample(nrow(A3.df)),]#shuffle
#   Y.list[[3]] <- A3.df 
#   A4.df <- cbind(L24$Yn5, pair.dfs[1:(nrow(pair.dfs)-24), ]) 
#   names(A4.df)[1] <- "A4"
#   A4.df <- A4.df[sample(nrow(A4.df)),]#shuffle
#   Y.list[[4]] <- A4.df 
#   A5.df <- cbind(L24$Yn1, pair.dfs[1:(nrow(pair.dfs)-24), ]) 
#   names(A5.df)[1] <- "A5"
#   A5.df <- A5.df[sample(nrow(A5.df)),]#shuffle
#   Y.list[[5]] <- A5.df 
#   A6.df <- cbind(L24$Yn2, pair.dfs[1:(nrow(pair.dfs)-24), ]) 
#   names(A6.df)[1] <- "A6"
#   A6.df <- A6.df[sample(nrow(A6.df)),]#shuffle
#   Y.list[[6]] <- A6.df 
#   
#   for(dfs in 1:6){
#     Y.list[[dfs]] <- Y.list[[dfs]][complete.cases(Y.list[[dfs]]),]  } #}      #NAs 1 to 6
#   
#   print(length((Y.list)))
#   print(dim(Y.list[[1]]))
#   print(dim(Y.list[[2]]))
#   print(dim(Y.list[[3]]))
#   print(dim(Y.list[[4]]))
#   print(dim(Y.list[[5]]))
#   print(dim(Y.list[[6]]))
#   return(Y.list)
# }

#1. read in data sets
##########################
BNBUSDT.new1min.df <- fread(file='BNBUSDT-oldBOX.csv')#1
ETHUSDT.new1min.df <- fread(file='ETHUSDT-oldBOX.csv')#2
BNBUSDT.new5min.df <- fread(file='9.02.2018/smallBNBUSDT.csv')#3
BNBUSDT.new5min.df <- BNBUSDT.new5min.df[1:(nrow(BNBUSDT.new5min.df)-4000),]
ETHUSDT.new5min.df <- fread(file='9.02.2018/smallETHUSDT.csv')#4
ETHUSDT.new5min.df <- ETHUSDT.new5min.df[1:(nrow(ETHUSDT.new5min.df)-4000),]
BNBBTC.old5min.df <- fread(file='BNBBTC-oldBOX.csv')#5
BNBBTC.new5min.df <- fread(file='9.02.2018/smallBNBBTC.csv')#6
BNBBTC.new5min.df <- BNBBTC.new5min.df[1:(nrow(BNBBTC.new5min.df)-4000),]
ETHBTC.old5min.df <- fread(file='ETHBTC-oldBOX.csv')#7
ETHBTC.new5min.df <- fread(file='9.02.2018/smallETHBTC.csv')#8
ETHBTC.new5min.df <- ETHBTC.new5min.df[1:(nrow(ETHBTC.new5min.df)-4000),]
ADABTC.old5min.df <- fread(file='ADABTC-oldBOX.csv')#9
ADABTC.new5min.df <- fread(file='9.02.2018/smallADABTC.csv')#10
ADABTC.new5min.df <- ADABTC.new5min.df[1:(nrow(ADABTC.new5min.df)-4000),]
EOSBTC.old5min.df <- fread(file='EOSBTC-oldBOX.csv')#11
EOSBTC.new5min.df <- fread(file='9.02.2018/smallEOSBTC.csv')#12
EOSBTC.new5min.df <- EOSBTC.new5min.df[1:(nrow(EOSBTC.new5min.df)-4000),]
IOTABTC.old5min.df <- fread(file='IOTABTC-oldBOX.csv')#13
IOTABTC.new5min.df <- fread(file='9.02.2018/smallIOTABTC.csv')#14
IOTABTC.new5min.df <- IOTABTC.new5min.df[1:(nrow(IOTABTC.new5min.df)-4000),]
NEOBTC.new5min.df <- fread(file='9.02.2018/smallNEOBTC.csv')#15
NEOBTC.new5min.df <- NEOBTC.new5min.df[1:(nrow(NEOBTC.new5min.df)-4000),]
TRXBTC.new5min.df <- fread(file='9.02.2018/smallTRXBTC.csv')#16
TRXBTC.new5min.df <- TRXBTC.new5min.df[1:(nrow(TRXBTC.new5min.df)-4000),]
XLMBTC.new5min.df <- fread(file='9.02.2018/smallXLMBTC.csv')#17
XLMBTC.new5min.df <- XLMBTC.new5min.df[1:(nrow(XLMBTC.new5min.df)-4000),]
#new
XLMBTC.old5min.df <- fread(file='XLMBTC-oldBOX.csv')#18
TRXBTC.old5min.df <- fread(file='TRXBTC-oldBOX.csv')#19
BTCUSDT.old5min.df <-fread(file='BTCUSDT-old5mins.csv')#20 fuckedf
BTCUSDT.new1min.df <- fread(file='BTCUSDT-new1mins.csv')#21
BTCUSDT.new5min.df <- fread(file='9.02.2018/smallBTCUSDT.csv')#22
BTCUSDT.new5min.df <- BTCUSDT.new5min.df[1:(nrow(BTCUSDT.new5min.df)-4000),]

datasetlist <- list(BNBUSDT.new1min.df, ETHUSDT.new1min.df, BNBUSDT.new5min.df, ETHUSDT.new5min.df, BNBBTC.old5min.df, BNBBTC.new5min.df, ETHBTC.old5min.df, 
                    ETHBTC.new5min.df, ADABTC.old5min.df, ADABTC.new5min.df, EOSBTC.old5min.df, EOSBTC.new5min.df, IOTABTC.old5min.df, IOTABTC.new5min.df, NEOBTC.new5min.df, 
                    TRXBTC.new5min.df, XLMBTC.new5min.df, XLMBTC.old5min.df, TRXBTC.old5min.df, BTCUSDT.new1min.df, BTCUSDT.new5min.df, BTCUSDT.new5min.df)

for(i in 1:22){
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
for (i in 1:22){    dupes.list[[i]] <- which(duplicated(datasetlist[[i]]) | duplicated(datasetlist[[i]][nrow(datasetlist[[i]]):1, ]) [nrow(datasetlist[[i]]):1])}

no.dupes.list <- list()
for (i in 1:22){
  if (i == 1 | i == 2 | i == 5 | i == 20 | i == 6 | i == 8 | i == 14){ #no dupes
    if(i ==5){   no.dupes.list[[i]] <- list(datasetlist[[i]], datasetlist[[i]])   
    }else{     no.dupes.list[[i]] <- datasetlist[[i]] } }
  #3 and 5: 3, 4, 6, 8 also 21,22
  if(i == 3 | i == 4 | i == 21 | i == 22){
    list35 <- list(datasetlist[[i]][1:3430,], datasetlist[[i]][3870:5720,], datasetlist[[i]][5810:nrow(datasetlist[[i]]),])  
    no.dupes.list[[i]] <- list35 }
  #7 and 8: 7, 9, 11, 13 also 18, 19, 
  if(i == 7 | i == 9 | i == 11 | i == 13 | i == 18 | i == 19){
    list78 <- list(datasetlist[[i]][1:7650,], datasetlist[[i]][8560:nrow(datasetlist[[i]]),])
    no.dupes.list[[i]] <- list78  }
  #1 and 3: 14:17, 12, 10, 
  if(i == 15 | i == 16 | i == 17 | i == 12 | i == 10){
    list13 <- list(datasetlist[[i]][1:1260,], datasetlist[[i]][1700:3535,], datasetlist[[i]][3640:nrow(datasetlist[[i]]),])
    no.dupes.list[[i]] <- list13
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

#1-1ETHUSDT, 1-2BNBUSDT, 3-20BTCUSDT, 4 ETHUSDT, 5BNBYSDT, 6BNBBTC, 7ETHBTC, 9ADABTC, 11EOSBTC, 13IOTABTC, 15NEOBTC, 16XLMBTC, 14TRXBTC,21BTCUSDT
paircombo.list <- list(no.dupes.list[[1]],no.dupes.list[[2]],no.dupes.list[[20]],no.dupes.list[[3]],no.dupes.list[[4]],no.dupes.list[[5]],no.dupes.list[[7]],no.dupes.list[[9]],no.dupes.list[[11]],no.dupes.list[[13]],no.dupes.list[[15]],no.dupes.list[[16]],no.dupes.list[[17]], no.dupes.list[[21]])

finalsets <- list() #pair DF list
for (i in 1:14){
  if(i == 1){
    print(paste('run num: 1 and 2 and 13'))    # print(paste0('dims: ',dim(sixty.list[[1]])))
    n=60 #group by 60 for both 1 and 2
    print(paste0('current 3 dim', dim(paircombo.list[[3]])))
    sixty.listA <- list(paircombo.list[[1]], paircombo.list[[2]] , paircombo.list[[3]])
    sixty.list <- lapply(sixty.listA, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
    for (dset in 1:length(sixty.listA)){
      v.ntrade <- aggregate(sixty.listA[[dset]][,7],list(rep(1:(nrow(sixty.listA[[dset]])%/%n+1),each=n,len=nrow(sixty.listA[[dset]]))),sum)
      v.volume <- aggregate(sixty.listA[[dset]][,5],list(rep(1:(nrow(sixty.listA[[dset]])%/%n+1),each=n,len=nrow(sixty.listA[[dset]]))),sum)
      sixty.list[[dset]][,7] <- v.ntrade[1:nrow(sixty.list[[dset]]),2] #replace vol and ntrades
      sixty.list[[dset]][,5] <- v.volume[1:nrow(sixty.list[[dset]]),2]       } #agg trades and vol
    for(dset in 1:length(sixty.list)){
      sixty.list[[dset]] <- sixty.list[[dset]][complete.cases(sixty.list[[dset]]),]  }#remove NA
    print(paste0('current 3 dim prior feat', dim(sixty.list[[3]])))
    sixty.list <- FeatEng.list(sixty.list)#feat eng
    for(dset in 1:length(sixty.list)){
      sixty.list[[dset]] <- sixty.list[[dset]][complete.cases(sixty.list[[dset]]),]  }#remove NA
    # for(s in 1:3){ #loop sending 1 pair at time to Ybuilder
    # print(paste0('runnum: ', s))
    print(paste0('current 3 dim prior Y', dim(sixty.list[[3]])))
    templist <- Ybuilder.list(sixty.list, 60)#offset for Y, build dfs
    print(length(templist))
    for(s in 1:3){
      finalsets[[s]] <- templist[[s]]  }  }   
  if(i > 3){
    print(paste('run num: ',i))
    n=12 #group by 12
    twelve.listA <- paircombo.list[[i]]
    twelve.list <- lapply(twelve.listA, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
    for (dset in 1:length(twelve.listA)){
      v.ntrade <- aggregate(twelve.listA[[dset]][,7],list(rep(1:(nrow(twelve.listA[[dset]])%/%n+1),each=n,len=nrow(twelve.listA[[dset]]))),sum)
      v.volume <- aggregate(twelve.listA[[dset]][,5],list(rep(1:(nrow(twelve.listA[[dset]])%/%n+1),each=n,len=nrow(twelve.listA[[dset]]))),sum)
      twelve.list[[dset]][,7] <- v.ntrade[1:nrow(twelve.list[[dset]]),2] #replace vol and ntrades
      twelve.list[[dset]][,5] <- v.volume[1:nrow(twelve.list[[dset]]),2]       } #agg trades and vol
    for(dset in 1:length(twelve.list)){#remove NA
      twelve.list[[dset]] <- twelve.list[[dset]][complete.cases(twelve.list[[dset]]),]  }
    twelve.list <- FeatEng.list(twelve.list)  #feat eng
    for(dset in 1:length(twelve.list)){  #remove NA
      twelve.list[[dset]] <- twelve.list[[dset]][complete.cases(twelve.list[[dset]]),]  }
    temp6df <- Ybuilder.list(twelve.list, 12)#offset for Y, build dfs
    for(dset in 1:length(temp6df)){#clean NA again
      temp6df[[dset]] <- temp6df[[dset]][complete.cases(temp6df[[dset]]),]  }
    finalsets[[i]] <- temp6df   } }#add to finalsets list

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

finalsets11 <- list(finalsets2[[1]], finalsets2[[2]], finalsets2[[3]], finalsets2[[6]], finalsets2[[7]], finalsets2[[8]], finalsets2[[9]], finalsets2[[10]], finalsets2[[11]], finalsets2[[12]], finalsets2[[14]])
dfname.vector <- c('ETHUSDT', 'BNBUSDT', 'BTCUSDT', 'BNBBTC', 'ETHBTC', 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'XLMBTC', 'TRXBTC')

#write csvs
for(Npair in 1:11){
  for(frame in 1:6){
    #build csv file name
    csvname <- paste0('9.02.2018/', dfname.vector[Npair], '-a', frame, '.csv')
    #save
    fwrite(finalsets11[[Npair]][[frame]], file=csvname)
  }
}




# 
# # loop 1 to 12
# finalsets <- list() #pair DF list
# for (i in 1:14){
#   if(i == 1){
#     n=60 #group by 60 for both 1 and 2
#     sixty.list2 <- list(paircombo.list[[1]], paircombo.list[[2]], paircombo.list[[13]])#][1]), as.data.frame(no.dupes.list[[13]][2]), as.data.frame(no.dupes.list[[13]][3]))
#     sixty.list <- lapply(sixty.list2, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
#     for (dset in 1:length(sixty.list)){
#       v.ntrade <- aggregate(sixty.list2[[dset]][,7],list(rep(1:(nrow(sixty.list2[[dset]])%/%n+1),each=n,len=nrow(sixty.list2[[dset]]))),sum)
#       v.volume <- aggregate(sixty.list2[[dset]][,5],list(rep(1:(nrow(sixty.list2[[dset]])%/%n+1),each=n,len=nrow(sixty.list2[[dset]]))),sum)
#       sixty.list[[dset]][,7] <- v.ntrade[1:nrow(sixty.list[[dset]]),2] #replace vol and ntrades
#       sixty.list[[dset]][,5] <- v.volume[1:nrow(sixty.list[[dset]]),2]       } #agg trades and vol
#     #remove NA
#     for(dset in 1:length(sixty.list)){
#       sixty.list[[dset]] <- sixty.list[[dset]][complete.cases(sixty.list[[dset]]),]  }
#     #feat eng
#     sixty.list <- FeatEng.list(sixty.list)
#     #remove NA
#     for(dset in 1:length(sixty.list)){
#       sixty.list[[dset]] <- sixty.list[[dset]][complete.cases(sixty.list[[dset]]),]  }
#     #loop sending 1 pair at time to Ybuilder
#     for(s in 1:5){
#       #offset for Y, build dfs
#       temp6df <- Ybuilder.list(sixty.list[[s]])
#       #clean NA again
#       for(dset in 1:length(temp6df)){
#         temp6df[[dset]] <- temp6df[[dset]][complete.cases(temp6df[[dset]]),]  }
#       #add to finalsets list
#       finalsets[[s]] <- temp6df
#     }#this turned 1,2 and 13into 3 (13,14,15?)
#   }
#   if(i > 2 && i != 13){
#     n=12 #group by 12
#     twelve.list <- lapply(paircombo.list[[i]], function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
#     for (dset in 1:length(twelve.list)){
#       v.ntrade <- aggregate(twelve.list[[dset]][,7],list(rep(1:(nrow(twelve.list[[dset]][,7])%/%n+1),each=n,len=nrow(twelve.list[[dset]][,7]))),sum)
#       v.volume <- aggregate(twelve.list[[dset]][,5],list(rep(1:(nrow(twelve.list[[dset]][,5])%/%n+1),each=n,len=nrow(twelve.list[[dset]][,5]))),sum)
#       twelve.list[[dset]][,7] <- v.ntrade[1:nrow(twelve.list[[dset]]),2] #replace vol and ntrades
#       twelve.list[[dset]][,5] <- v.volume[1:nrow(twelve.list[[dset]]),2]       } #agg trades and vol
#     #remove NA
#     for(dset in 1:length(twelve.list)){
#       twelve.list[[dset]] <- twelve.list[[dset]][complete.cases(twelve.list[[dset]]),]  }
#     #feat eng
#     twelve.list <- FeatEng.list(twelve.list)
#     #remove NA
#     for(dset in 1:length(twelve.list)){
#       twelve.list[[dset]] <- twelve.list[[dset]][complete.cases(twelve.list[[dset]]),]  }
#     #offset for Y, build dfs
#     temp6df <- Ybuilder.list(twelve.list)
#     #clean NA again
#     for(dset in 1:length(temp6df)){
#       temp6df[[dset]] <- temp6df[[dset]][complete.cases(temp6df[[dset]]),]  }
#     #add to finalsets list
#     finalsets[[i]] <- temp6df
#   }
# 
# }
# 
# 
# 
# 


# 
# #print out all 6 csvs
# fwrite(dataset.list[[1]], file="BTC-a1.csv")
# fwrite(dataset.list[[2]], file="BTC-a2.csv")
# fwrite(dataset.list[[3]], file="BTC-a3.csv")
# fwrite(dataset.list[[4]], file="BTC-a4.csv")
# fwrite(dataset.list[[5]], file="BTC-a5.csv")
# fwrite(dataset.list[[6]], file="BTC-a6.csv")
# 


