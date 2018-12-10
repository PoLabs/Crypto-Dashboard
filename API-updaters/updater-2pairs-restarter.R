
setwd("/home/ubuntu/cryptoAPI")#
pcklibs <- c("dplyr", 'rjson', 'TTR', 'data.table')#"arm", xts httr",RCurl, caret, 
lapply(pcklibs, require, character.only=TRUE)
runnum <- 1
BinanceData.f <- data.frame(c('a', 'b'), c('c', 'd'))
TxN.df <- data.frame(c('a', 'b'), c('c', 'd'))
# SeedData <- data.frame(c('a', 'b'), c('c', 'd'))

pair.vector <- c('BTCUSDT','ETHUSDT','BNBUSDT','ADABTC', 'EOSBTC', 'NEOBTC', 'TRXBTC', 'XLMBTC', 'BNBBTC', 'ETHBTC', 'IOTABTC')#c('BTCUSDT','ETHUSDT','BNBUSDT')
#pair.vector <- c('XLMBTC', 'XLMBTC')

ss.new.BTCUSDT <- fileSnapshot(path="/home/ubuntu/cryptoAPI/BTCUSDT/raw", file.info = T) #"/home/ubuntu/cryptoAPI/BTCUSDT/raw"
ss.new.ETHUSDT <- fileSnapshot(path="/home/ubuntu/cryptoAPI/ETHUSDT/raw", file.info = T) #"/home/ubuntu/cryptoAPI/ETHUSDT/raw"
ss.new.BNBUSDT <- fileSnapshot(path="/home/ubuntu/cryptoAPI/BNBUSDT/raw", file.info = T)

ss.new.ADABTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/ADABTC/raw", file.info = T) #"/home/ubuntu/cryptoAPI/BTCUSDT/raw"
ss.new.EOSBTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/EOSBTC/raw", file.info = T) #"/home/ubuntu/cryptoAPI/ETHUSDT/raw"
ss.new.NEOBTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/NEOBTC/raw", file.info = T)
ss.new.TRXBTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/TRXBTC/raw", file.info = T)
ss.new.XLMBTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/XLMBTC/raw", file.info = T)
ss.new.BNBBTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/BNBBTC/raw", file.info = T)
ss.new.ETHBTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/ETHBTC/raw", file.info = T)
ss.new.IOTABTC <- fileSnapshot(path="/home/ubuntu/cryptoAPI/IOTABTC/raw", file.info = T)
ss.new.list <- list(ss.new.BTCUSDT, ss.new.ETHUSDT, ss.new.BNBUSDT, ss.new.ADABTC, ss.new.EOSBTC, ss.new.NEOBTC, ss.new.TRXBTC, ss.new.XLMBTC, ss.new.BNBBTC, ss.new.ETHBTC, ss.new.IOTABTC)
#ss.new.list <- list(ss.new.XLMBTC, ss.new.XLMBTC)
names(ss.new.list) <- pair.vector

for(Cpair in pair.vector){
  
  runnum <- 1
  allNames <- as.character(row.names(ss.new.list[[Cpair]]$info))# pull all times
  allTimes<- as.character(ss.new.list[[Cpair]]$info$mtime)
  allTimes <- substr(allTimes, 1, nchar(allTimes)-1)
  uniqTimes <- unique(allTimes)
  uniqTimes <- sort(uniqTimes, decreasing = FALSE)
  TxN.df <- data.frame(cbind(as.character(allNames), as.character(allTimes)))
  
  for(x in 1:length(uniqTimes)){
    position.vec <- which(TxN.df[,2] %in% uniqTimes[x])
    print(length(position.vec))
    
    if(length(position.vec) > 3){
      for(i in length(position.vec)){
        tryCatch({
          
          #find K first, then find B, D
          spotK <- which(substring(TxN.df[position.vec,1], 1, 1) %in% c('k','K'))
          # if(substring(TxN.df[position.vec[i],1], 1, 1) %in% c('k','K')){
          print(paste0(Cpair, "/raw/", TxN.df[position.vec[spotK],1]))
          KlinesResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/", TxN.df[position.vec[spotK],1]))
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
          # print(paste0('dims klines: '), dim(KlinesResp.df))#}
          
          # if(substring(TxN.df[position.vec[i],1], 1, 1) %in% c('p','P')){
          spotD <- which(substring(TxN.df[position.vec,1], 1, 1) %in% c('d','D'))
          print(spotD)
          # if(length(spotP)>1){          spotP <- spotP[1]        }
          print(paste0(Cpair, "/raw/", TxN.df[position.vec[spotD],1]))
          DepthResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/", TxN.df[position.vec[spotD],1]))
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
          # print(paste0('dims depth: '), dim(DepthData))#}
          
          # if(substring(TxN.df[position.vec[i],1], 1, 1) %in% c('b','B')){
          spotB <- which(substring(TxN.df[position.vec,1], 1, 1) %in% c('b','B'))
          print(paste0(Cpair, "/raw/", TxN.df[position.vec[spotB],1]))
          BookResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/", TxN.df[position.vec[spotB],1]))
          BookResp.df <- data.frame(t(sapply(BookResp,c)))
          BookResp.df[,1] <- as.double(as.character(BookResp.df[,2]))
          BookResp.df[,2] <- as.double(as.character(BookResp.df[,4]))
          nvectorPAIR <- c("PAIRbid", "PAIRask")
          colnames(BookResp.df) <- nvectorPAIR
          BookResp.df <- BookResp.df[,1:2]
          BookResp.df$PAIRspread <- as.double(BookResp.df$PAIRask - BookResp.df$PAIRbid)
          BookResp.df$PAIRaskspread <- as.double(BookResp.df$PAIRask - KlinesResp.df$PAIRclose)
          BookResp.df$PAIRbidspread <- as.double(KlinesResp.df$PAIRclose - BookResp.df$PAIRbid)
          # print(paste0('dims book: '), dim(BookResp.df))#}
          
        }, error = function(e) print(paste('error in ', uniqTimes[x])))
      }
      tryCatch({
        BinanceObs <- cbind(KlinesResp.df, BookResp.df, DepthData)
        BinanceObs$PAIRd <- BinanceObs$PAIRclose - BinanceObs$PAIRopen
        BinanceObs$time <- uniqTimes[(x+1)]
        # newdata <- 1
        if(runnum!=1){
          if(is.na(BinanceObs$time)){            print('NA')
          }else{BinanceData.f <- rbind(BinanceData.f, BinanceObs)              }
        }else{BinanceData.f <- BinanceObs}
        runnum <- runnum +1
      }, error = function(e2) print(paste('missing data skip')) )
      
    } 
    rm(KlinesResp.df, BookResp.df, DepthData, BookResp, KlinesResp)
  }
  
  print('feature engineering time')
  ###############################
  #Feature Engineering#
  ###############################
  #12 unit moving average > vol, close, spread, btc close #SMA(x, n = 10, ...)
  BinanceData.f$SMA12close <- SMA(BinanceData.f$PAIRclose, n = 12)
  BinanceData.f$SMA36close <- SMA(BinanceData.f$PAIRclose, n = 144)
  BinanceData.f$SMA12vol <- SMA(BinanceData.f$PAIRvolume, n = 12)
  BinanceData.f$SMA36vol <- SMA(BinanceData.f$PAIRvolume, n = 144)
  
  #EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...)
  BinanceData.f$EMA12close <- EMA(BinanceData.f$PAIRclose, n = 12)
  BinanceData.f$EMA36close <- EMA(BinanceData.f$PAIRclose, n = 144)
  BinanceData.f$EMA12vol <- EMA(BinanceData.f$PAIRvolume, n = 12)
  BinanceData.f$EMA36vol <- EMA(BinanceData.f$PAIRvolume, n = 144)
  BinanceData.f$EMA12spread <- EMA(BinanceData.f$PAIRspread, n = 12)
  
  #EVWMA(price, volume, n = 10, ...)
  BinanceData.f$EVWMA12close <- EVWMA(BinanceData.f$PAIRclose, BinanceData.f$PAIRvolume,  n = 12)
  
  #VWAP(price, volume, n = 10, ...)
  BinanceData.f$VWAP12close <- VWAP(BinanceData.f$PAIRclose, BinanceData.f$PAIRvolume,  n = 12)
  BinanceData.f$VWAP36close <- VWAP(BinanceData.f$PAIRclose, BinanceData.f$PAIRvolume, n = 144)
  
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
  
  ###############################
  csvfolder <- paste0(Cpair, "/CSV/small", Cpair, ".csv")
  fwrite(BinanceData.f, file = csvfolder)
}




