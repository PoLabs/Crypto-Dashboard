setwd("/home/ubuntu/cryptoAPI")#
pcklibs <- c("dplyr", 'rjson', 'TTR', 'data.table')#"arm", xts httr",RCurl, caret, 
lapply(pcklibs, require, character.only=TRUE)
runnum <- 1
BitfinexData.f <- data.frame(c('a', 'b'), c('c', 'd'))
TxN.df <- data.frame(c('a', 'b'), c('c', 'd'))
newdata <- 0

pair.vector <- c('LTCUSD', 'ETHUSD', 'BTCUSD', 'EOSUSD', 'BCHUSD', 'XRPUSD')

ss.new.LTCUSD <- fileSnapshot(path="/home/ubuntu/cryptoAPI/bitfinex/LTCUSD/raw", file.info = T)
ss.new.ETHUSD <- fileSnapshot(path="/home/ubuntu/cryptoAPI/bitfinex/ETHUSD/raw", file.info = T)
ss.new.BTCUSD <- fileSnapshot(path="/home/ubuntu/cryptoAPI/bitfinex/BTCUSD/raw", file.info = T)
ss.new.EOSUSD <- fileSnapshot(path="/home/ubuntu/cryptoAPI/bitfinex/EOSUSD/raw", file.info = T)
ss.new.BCHUSD <- fileSnapshot(path="/home/ubuntu/cryptoAPI/bitfinex/BCHUSD/raw", file.info = T)
ss.new.XRPUSD <- fileSnapshot(path="/home/ubuntu/cryptoAPI/bitfinex/XRPUSD/raw", file.info = T)

ss.new.list <- list(ss.new.LTCUSD, ss.new.ETHUSD, ss.new.BTCUSD, ss.new.EOSUSD, ss.new.BCHUSD, ss.new.XRPUSD)
names(ss.new.list) <- pair.vector
                    
for(Cpair in pair.vector){
  runnum <- 1
  allNames <- as.character(row.names(ss.new.list[[Cpair]]$info))# pul l all times
  allTimes<- as.character(ss.new.list[[Cpair]]$info$mtime)
  allTimes <- substr(allTimes, 1, nchar(allTimes)-1)
  uniqTimes <- unique(allTimes)
  uniqTimes <- sort(uniqTimes, decreasing = FALSE)
  TxN.df <- data.frame(cbind(as.character(allNames), as.character(allTimes)))
  
  for(x in 1:length(uniqTimes)){
    position.vec <- which(TxN.df[,2] %in% uniqTimes[x])
    print(length(position.vec))

    if(length(position.vec) > 2){
      for(i in length(position.vec)){
         tryCatch({

          #find K first, then find B, D
          spotC <- which(substring(TxN.df[position.vec,1], 1, 1) %in% c('L','l'))
          print(paste0(Cpair, "/raw/", TxN.df[position.vec[spotC],1]))
          # KlinesResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/", TxN.df[position.vec[spotC],1]))
          #last = 5min candle: MTsec, open, close, high, low, volume
          CandleResp <- rjson::fromJSON(file=paste0("bitfinex/", Cpair, "/raw/", TxN.df[position.vec[spotC],1]))
          CandleResp.df <- data.frame(t(sapply(CandleResp,c)))
          CandleResp.df <- CandleResp.df[,2:6]
          colnames(CandleResp.df) <- c('PAIRopen', 'PAIRclose', 'PAIRhigh', 'PAIRlow', 'PAIRvolume')
          CandleResp.df$PAIRd <- CandleResp.df$PAIRclose - CandleResp.df$PAIRopen

          spotB <- which(substring(TxN.df[position.vec,1], 1, 1) %in% c('p','P'))
          print(paste0(Cpair, "/raw/", TxN.df[position.vec[spotB],1]))
          # DepthResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/", TxN.df[position.vec[spotD],1]))
          #P3 = book: price, rate, period, count, +/-amount
          BookResp <- rjson::fromJSON(file=paste0("bitfinex/", Cpair, "/raw/", TxN.df[position.vec[spotB],1]))
          BookResp.df <- data.frame(t(sapply(BookResp,c)))          #1-5, 26-30...PAIRorder1, PAIRorder2, PAIRorder3, PAIRordern1, PAIRordern2, PAIRordern3   #number orders / total  amount
          BookResp.df$PAIRorder1 <- (BookResp.df[1,2] / BookResp.df[1,3])
          BookResp.df$PAIRorder2 <- (BookResp.df[2,2] / BookResp.df[2,3])
          BookResp.df$PAIRorder3 <- (BookResp.df[3,2] / BookResp.df[3,3])
          BookResp.df$PAIRorder4 <- (BookResp.df[4,2] / BookResp.df[4,3])
          BookResp.df$PAIRordern1 <- (BookResp.df[26,2] / BookResp.df[26,3])
          BookResp.df$PAIRordern2 <- (BookResp.df[27,2] / BookResp.df[27,3])
          BookResp.df$PAIRordern3 <- (BookResp.df[28,2] / BookResp.df[28,3])
          BookResp.df$PAIRordern4 <- (BookResp.df[29,2] / BookResp.df[29,3])
          BookResp.df <- BookResp.df[1,4:ncol(BookResp.df)]

          spotT <- which(substring(TxN.df[position.vec,1], 1, 1) %in% c('t','T'))
          print(paste0(Cpair, "/raw/", TxN.df[position.vec[spotT],1]))
          # BookResp <-  rjson::fromJSON(file=paste0(Cpair, "/raw/", TxN.df[position.vec[spotT],1]))
          #tPAIR; FRR, bid, bid period, bid size, asl, ask period, ask size, daily change, daily change percent, last price, daily: volume, high, low
          TickerResp <- rjson::fromJSON(file=paste0("bitfinex/", Cpair, "/raw/", TxN.df[position.vec[spotT],1]))
          TickerResp.df <- data.frame(t(sapply(TickerResp,c)))
          colnames(TickerResp.df) <- c('PAIRbid', 'bidps', 'PAIRask', 'askps', 'dailychange', 'dailychangepercent', 'lastprice','dailyvol', 'dailyhigh', 'dailylow')
          TickerResp.df$PAIRbaspread <- (TickerResp.df$PAIRask - TickerResp.df$PAIRbid)
          TickerResp.df <- TickerResp.df[,c(1,3,11)]
          TickerResp.df$PAIRbidspread <- CandleResp.df$PAIRclose - TickerResp.df$PAIRbid
          TickerResp.df$PAIRaskspread <- TickerResp.df$PAIRask - CandleResp.df$PAIRclose

         }, error = function(e) print(paste('error in ', uniqTimes[x])))
                  }
      tryCatch({
        BinanceObs <- cbind(CandleResp.df, TickerResp.df, BookResp.df)
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
      rm(KlinesResp.df, BookResp.df, DepthData, KlinesResp, BookResp
  #    }
 #     BitfinexObs <- cbind(CandleResp.df, TickerResp.df, BookResp.df)
    #  BitfinexObs$time <- uniqTimes[time]
#      if(runnum!=1){BitfinexData.f <- rbind(BitfinexData.f, BitfinexObs)
  #    }else{BitfinexData.f <- BitfinexObs}
 #   }
  #  runnum <- runnum +1
  }
  csvfolder <- paste0("bitfinex/", Cpair, "/CSV/small", Cpair, ".csv")
  fwrite(BitfinexData.f, file = csvfolder)
}




