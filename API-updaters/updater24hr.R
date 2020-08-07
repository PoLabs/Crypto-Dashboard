#this is central data script run ~4min
#1. scan dirs for changes 
  #-contain dir list in R object
#2. import new data
#3. write csv
setwd("/home/ubuntu/cryptoAPI")

pcklibs <- c("dplyr", 'rjson', 'TTR')#"arm", xts httr",RCurl, caret, 
lapply(pcklibs, require, character.only=TRUE)

ss.BTCUSDT <- fileSnapshot(path="/home/ubuntu/cryptoAPI/BTCUSDT/raw", file.info = T)
ss.ETHUSDT <- fileSnapshot(path="/home/ubuntu/cryptoAPI/ETHUSDT/raw", file.info = T)
ss.BNBUSDT <- fileSnapshot(path="/home/ubuntu/cryptoAPI/BNBUSDT/raw", file.info = T)

dx.BTCUSDT <- changedFiles(ss.BTCUSDT, path="/home/ubuntu/cryptoAPI/BTCUSDT/raw")
dx.ETHUSDT <- changedFiles(ss.ETHUSDT, path="/home/ubuntu/cryptoAPI/ETHUSDT/raw")
dx.BNBUSDT <- changedFiles(ss.BNBUSDT, path="/home/ubuntu/cryptoAPI/BNBUSDT/raw")

pair.vector <- c('BTCUSDT','ETHUSDT','BNBUSDT')
change.list <- list(dx.BTCUSDT, dx.ETHUSDT, dx.BNBUSDT)

#sub("^[^.]*", "", x)
#substring(x, 1)

for (Cpair in pair.vector){
  for (changes in change.list){ #or should 'changes' be 'i'?

#NEW data
#####################
print('NEW data incoming!')
KlinesResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/klines@symbol=", Cpair, "&interval=5m.", i)) #need to udpate 'i'
KlinesResp.df <- data.frame(t(sapply(KlinesResp,c)))
klinesrows <- nrow(KlinesResp.df)
KlinesResp.df <- data.frame(KlinesResp.df[klinesrows-1, c(2,3,4,5,6, 8, 9, 10, 11)])
KlinesResp.df[,1] <- as.double(KlinesResp.df[,1])
KlinesResp.df[,2] <- as.double(KlinesResp.df[,2])
KlinesResp.df[,3] <- as.double(KlinesResp.df[,3])
KlinesResp.df[,4] <- as.double(KlinesResp.df[,4])
KlinesResp.df[,5] <- as.double(KlinesResp.df[,5])
KlinesResp.df[,6] <- as.double(KlinesResp.df[,6])
KlinesResp.df[,7] <- as.double(KlinesResp.df[,7])
KlinesResp.df[,8] <- as.double(KlinesResp.df[,8])
KlinesResp.df[,9] <- as.double(KlinesResp.df[,9])
nvector <- c("PAIRopen", "PAIRhigh", "PAIRlow", "PAIRclose", "PAIRvolume", "PAIRqav", "PAIRntrade", "PAIRbavTB", "PAIRqavTB")
colnames(KlinesResp.df) <- nvector

BookResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/bookTicker@symbol=", Cpair, ".", i)) #need to udpate 'i'    #"NANO/bookTicker@symbol=NANOBTC."
BookResp.df <- data.frame(t(sapply(BookResp,c)))
BookResp.df[,1] <- as.double(as.character(BookResp.df[,2]))
BookResp.df[,2] <- as.double(as.character(BookResp.df[,4]))
nvectorPAIR <- c("PAIRbid", "PAIRask")
colnames(BookResp.df) <- nvectorPAIR
BookResp.df <- BookResp.df[,1:2]
BookResp.df$PAIRspread <- as.double(BookResp.df$PAIRask - BookResp.df$PAIRbid) #bid-ask spread (lowest ask - highest bid)
BookResp.df$PAIRaskspread <- as.double(BookResp.df$PAIRask - KlinesResp.df$PAIRclose)
BookResp.df$PAIRbidspread <- as.double(KlinesResp.df$PAIRclose - BookResp.df$PAIRbid)

DepthResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/depth@symbol=", Cpair, ".", i)) #need to udpate 'i'    #"NANO/bookTicker@symbol=NANOBTC."
DepthResp.df <- data.frame(t(sapply(DepthResp,c)))
cols = c(1, 2);    
DepthBids <- data.frame(matrix(unlist(DepthResp.df$bids), nrow=200, ncol=2, byrow=T))
DepthBids[,cols] = apply(DepthBids[,cols], 2, function(x) as.numeric(as.character(x)));
DepthAsks <- data.frame(matrix(unlist(DepthResp.df$asks), nrow=200, ncol=2, byrow=T))
DepthAsks[,cols] = apply(DepthAsks[,cols], 2, function(x) as.numeric(as.character(x)));
P.2BidDepth <- subset(DepthBids, X1 >= (0.998*KlinesResp.df[,4]))
P.2BidVol <- sum(P.2BidDepth$X2)
P.4BidDepth <- subset(DepthBids, X1 >= (0.996*KlinesResp.df[,4]) & X1 <= (0.998*KlinesResp.df[,4]))
P.4BidVol <- sum(P.4BidDepth$X2)
P.6BidDepth <- subset(DepthBids, X1 >= (0.994*KlinesResp.df[,4]) & X1 <= (0.996*KlinesResp.df[,4]))
P.6BidVol <- sum(P.6BidDepth$X2)
P1BidDepth <- subset(DepthBids, X1 >= (0.99*KlinesResp.df[,4]) & X1 <= (0.994*KlinesResp.df[,4]))
P1BidVol <- sum(P1BidDepth$X2)
P1.5BidDepth <- subset(DepthBids, X1 >= (0.985*KlinesResp.df[,4]) & X1 <= (0.99*KlinesResp.df[,4]))
P1.5BidVol <- sum(P1.5BidDepth$X2)
P2BidDepth <- subset(DepthBids, X1 >= (0.98*KlinesResp.df[,4]) & X1 <= (0.985*KlinesResp.df[,4]))
P2BidVol <- sum(P2BidDepth$X2)
DepthBids <- cbind(P.2BidVol, P.4BidVol, P.6BidVol, P1BidVol, P1.5BidVol, P2BidVol)#, P2.5BidVol, P3BidVol)
P.2AskDepth <- subset(DepthAsks, X1 <= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.002)))
P.2AskVol <- sum(P.2AskDepth$X2)
P.4AskDepth <- subset(DepthAsks, X1 <= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.004)) & X1 >= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.002)))
P.4AskVol <- sum(P.4AskDepth$X2)
P.6AskDepth <- subset(DepthAsks, X1 <= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.006)) & X1 >= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.004)))
P.6AskVol <- sum(P.6AskDepth$X2)
P1AskDepth <- subset(DepthAsks, X1 <= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.01)) & X1 >= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.006)))
P1AskVol <- sum(P1AskDepth$X2)
P1.5AskDepth <- subset(DepthAsks, X1 <= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.015)) & X1 >= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.01)))
P1.5AskVol <- sum(P1.5AskDepth$X2)
P2AskDepth <- subset(DepthAsks, X1 <= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.02)) & X1 >= (KlinesResp.df[,4] + (KlinesResp.df[,4]*0.015)))
P2AskVol <- sum(P2AskDepth$X2)
DepthAsks <- cbind(P.2AskVol, P.4AskVol, P.6AskVol, P1AskVol, P1.5AskVol, P2AskVol)#, P2.5AskVol, P3AskVol)
DepthData <- cbind(DepthBids, DepthAsks)

KlinesRespBTC <- rjson::fromJSON(file=paste0("BTCUSDT/raw/klines@symbol=BTCUSDT&interval=5m.", i))
KlinesRespBTC.df <- data.frame(t(sapply(KlinesRespBTC,c)))
klinesrows <- nrow(KlinesRespBTC.df)
KlinesRespBTC.df <- data.frame(KlinesRespBTC.df[klinesrows-1, c(2,3,4,5,6, 8, 9, 10, 11)])
KlinesRespBTC.df[,1] <- as.double(KlinesRespBTC.df[,1])
KlinesRespBTC.df[,2] <- as.double(KlinesRespBTC.df[,2])
KlinesRespBTC.df[,3] <- as.double(KlinesRespBTC.df[,3])
KlinesRespBTC.df[,4] <- as.double(KlinesRespBTC.df[,4])
KlinesRespBTC.df[,5] <- as.double(KlinesRespBTC.df[,5])
KlinesRespBTC.df[,6] <- as.double(KlinesRespBTC.df[,6])
KlinesRespBTC.df[,7] <- as.double(KlinesRespBTC.df[,7])
KlinesRespBTC.df[,8] <- as.double(KlinesRespBTC.df[,8])
KlinesRespBTC.df[,9] <- as.double(KlinesRespBTC.df[,9])
nvector <- c("BTCopen", "BTChigh", "BTClow", "BTCclose", "BTCvolume", "BTCqav", "BTCntrade", "BTCbavTB", "BTCqavTB")#, "BTCprice")
colnames(KlinesRespBTC.df) <- nvector

BookRespBTC <- rjson::fromJSON(file=paste0("BTCUSDT/raw/bookTicker@symbol=BTCUSDT.", i))
BookRespBTC.df <- data.frame(t(sapply(BookRespBTC,c)))
BookRespBTC.df[,1] <- as.double(as.character(BookRespBTC.df[,2]))
BookRespBTC.df[,2] <- as.double(as.character(BookRespBTC.df[,4]))
nvectorBTC <- c("BTCbid", "BTCask")
colnames(BookRespBTC.df) <- nvectorBTC
BookRespBTC.df <- BookRespBTC.df[,1:2]
BookRespBTC.df$BTCspread <- as.double(BookRespBTC.df$BTCask - BookRespBTC.df$BTCbid) #bid-ask spread (lowest ask - highest bid)
BookRespBTC.df$BTCaskspread <- as.double(BookRespBTC.df$BTCask - KlinesRespBTC.df$BTCclose)
BookRespBTC.df$BTCbidspread <- as.double(KlinesRespBTC.df$BTCclose - BookRespBTC.df$BTCbid)

BinanceObs <- cbind(KlinesResp.df, BookResp.df, DepthData, KlinesRespBTC.df, BookRespBTC.df)
BinanceObs$PAIRd <- BinanceObs$PAIRclose - BinanceObs$PAIRopen

#read in latest .csv and bind to it
library(data.table)
if(nrow(BinanceData.f)>1){
  csvfolder <- paste0(Cpair, "/CSV/small", Cpair, ".csv")
  SeedData <- fread(file=csvfolder)
  BinanceData.f <- rbind(SeedData, BinanceObs)
}else{
  BinanceData.f <- BianceObs}


###############################
#Feature Engineering#
################################
#12 unit moving average > vol, close, spread, btc close #SMA(x, n = 10, ...)
BinanceData.f$SMA12close <- SMA(BinanceData.f$PAIRclose, n = 12)
BinanceData.f$SMA36close <- SMA(BinanceData.f$PAIRclose, n = 144)
BinanceData.f$SMA12vol <- SMA(BinanceData.f$PAIRvolume, n = 12)
BinanceData.f$SMA36vol <- SMA(BinanceData.f$PAIRvolume, n = 144)
BinanceData.f$SMA12spread <- SMA(BinanceData.f$PAIRspread, n = 12)
BinanceData.f$SMA12btcclose <- SMA(BinanceData.f$BTCclose, n = 12)

#EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...)
BinanceData.f$EMA12close <- EMA(BinanceData.f$PAIRclose, n = 12)
BinanceData.f$EMA36close <- EMA(BinanceData.f$PAIRclose, n = 144)
BinanceData.f$EMA12vol <- EMA(BinanceData.f$PAIRvolume, n = 12)
BinanceData.f$EMA36vol <- EMA(BinanceData.f$PAIRvolume, n = 144)
BinanceData.f$EMA12spread <- EMA(BinanceData.f$PAIRspread, n = 12)
BinanceData.f$EMA12btcclose <- EMA(BinanceData.f$BTCclose, n = 12)

#EVWMA(price, volume, n = 10, ...)
BinanceData.f$EVWMA12close <- EVWMA(BinanceData.f$PAIRclose, BinanceData.f$PAIRvolume,  n = 12)

#VWAP(price, volume, n = 10, ...)
BinanceData.f$VWAP12close <- VWAP(BinanceData.f$PAIRclose, BinanceData.f$PAIRvolume,  n = 12)
BinanceData.f$VWAP36close <- VWAP(BinanceData.f$PAIRclose, BinanceData.f$PAIRvolume, n = 144)
BinanceData.f$VWAP12btcclose <- VWAP(BinanceData.f$BTCclose, BinanceData.f$BTCvolume, n = 12)
BinanceData.f$VWAP36btcclose<- VWAP(BinanceData.f$BTCclose, BinanceData.f$BTCvolume, n = 144) 

#HMA(x, n = 12, ...)
BinanceData.f$HMA12close <- HMA(BinanceData.f$PAIRclose, n = 12)
BinanceData.f$HMA36close <- HMA(BinanceData.f$PAIRclose, n = 144)
BinanceData.f$HMA12vol <- HMA(BinanceData.f$PAIRvolume, n = 12)
BinanceData.f$HMA36vol <- HMA(BinanceData.f$PAIRvolume, n = 144)

#RSI(price, n = 14, maType, ...)     EMA..?
BinanceData.f$RSIPAIR12 <- RSI(BinanceData.f$PAIRclose, n=12)
BinanceData.f$RSIPAIR36 <- RSI(BinanceData.f$PAIRclose, n=144)

#MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)
MACDpriceE <- MACD(BinanceData.f$PAIRclose, nFast = 12, nSlow = 72, nSig = 3, maType="EMA", percent = TRUE)
BinanceData.f$MACDpriceE <- MACDpriceE[,1]
MACDvolE <- MACD(BinanceData.f$PAIRvolume, nFast = 12, nSlow = 72, nSig = 3, maType="EMA", percent = TRUE)
BinanceData.f$MACDvolE <- MACDvolE[,1]
MACDspreadE <- MACD(BinanceData.f$PAIRspread, nFast = 12, nSlow = 72, nSig = 3, maType="EMA", percent = TRUE)
BinanceData.f$MACDspreadE <- MACDspreadE[,1]
MACDbtcE <- MACD(BinanceData.f$BTCclose, nFast = 12, nSlow = 72, nSig = 3, maType="EMA", percent = TRUE)
BinanceData.f$MACDbtcE <- MACDbtcE[,1]

rm(MACDPAIRE, MACDbtcE, MACDspreadE, MACDvolE, MACDpriceE, MACDPAIRS, MACDbtcS, MACDspreadS, MACDvolS, MACDpriceS)

#leave NA's, let modeling program handle
#BinanceData.f <- BinanceData.f[150:nrow(BinanceData.f),]
#n5minObs <- nrow(BinanceData.f)


  }
  #write csv to correct dir, perhaps only write csv of 24hr data, everyhour 7d data can be updated


  
  
  
  if((nrow(BinanceData.f)%%1000)==0){    # the %% moduls operator gives remainder                    #if nrow/1000 is integer
    fullcsv <- paste0(Cpair, "/CSV/full", Cpair, ".csv")                                                 #then call full csv and rbind with small csv + new data, write full .csv
    fullSeed <- fread(file=fullcsv)
    fullData <- rbind(fullSeed, BinanceData.f[(nrow(BinanceData.f)-999):nrow(BinanceData.f),])
    fwrite(fullData, file=fullcsv)
    
    if(nrow(BinanceData.f) < 2016){                                                                 #also FASThour: if <2016, write to nrow, else write to 2016rows
      fwrite(BinanceData.f, file=csvfolder)
    }else{                                                                                           
      fwrite(BinanceData.f[(nrow(BinanceData.f)-2020):nrow(BinanceData.f),])    }                 
  }else{
    if(nrow(BinanceData.f) < 2016){                                                                 #also FASThour: if <2016, write to nrow, else write to 2016rows
      fwrite(BinanceData.f, file=csvfolder)
    }else{                                                                                           
      fwrite(BinanceData.f[(nrow(BinanceData.f)-2020):nrow(BinanceData.f),])    } 
  }
}


