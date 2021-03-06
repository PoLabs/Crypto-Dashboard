setwd("/home/ubuntu/crypto/")
pcklibs <- c('shiny', 'shinydashboard', 'data.table', 'wordcloud', 'caretEnsemble','caret','TTR')#("dplyr", 'rjson', 'TTR')#"arm", xts httr",RCurl, caret, 
lapply(pcklibs, require, character.only=TRUE)
n <- 4
options("scipen"=100, "digits"=4)
exUIA <- 'binance'
exUIB <- 'binance'
nameA <-'BTCUSDT'
nameB <- 'BTCUSDT'
timeA <- 30
timeB <- 30
#feature engineering
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
}

shinyServer(function(input, output){
  exchangeA <- reactive({as.character(input$exchangeInputA)})
  exchangeB <- reactive({as.character(input$exchangeInputB)})
  nameA <- reactive({as.character(input$pairAInput)})
  nameB <- reactive({as.character(input$pairBInput)})
  # timeA <- reactive({as.numeric(input$pair.timeA)})
  # timeB <- reactive({as.numeric(input$pair.timeB)})
  pairA <- reactive({as.data.frame(fread(paste0('/home/ubuntu/crypto/', nameA(), "/small", nameA(), ".csv")))})
  pairB <- reactive({as.data.frame(fread(paste0('/home/ubuntu/crypto/', nameB(), "/small", nameB(), ".csv")))})#rawToChar(get_object(paste0("s3://polabs-datasets/", exchangeB(), "-", input$pairBInput, "/small", input$pairBInput, ".csv")))))})
  nlpA <- reactive({as.data.frame(fread(paste0(nameA(), "/sync/tracker.csv")))})#rawToChar(get_object(paste0("s3://polabs-datasets/", exchangeA(), "-", input$pairAInput, "/sync/tracker.csv")))))})
  nlpB <- reactive({as.data.frame(fread(paste0(nameB(), "/sync/tracker.csv")))})#rawToChar(get_object(paste0("s3://polabs-datasets/", exchangeB(), "-", input$pairBInput, "/sync/tracker.csv")))))})
  
  output$pair.varsA <- renderUI({
    #exUIA <- exchangeA()
    if(exUIA %in% c('binance')){
      selectInput('varselectA', label='Binance variables: ', choices=c('Close'='PAIRclose', 'NLP sentiment'='raw.sent', 'NLP sentiment (weighted)'='comment.sent',  'Volume'='PAIRvolume', 'n Trades'='PAIRntrade','High'='PAIRhigh', 'Low'='PAIRlow', 'Bid'='PAIRbid', 'Ask'='PAIRask', 'SMA12close'='SMA12close', 'SMA144close'='SMA36close', 'SMA12spread'='SMA12spread', 'SMA12btcclose'='SMA12btcclose',  'EMA12close'='EMA12close', 'EMA144close'='EMA36close', 'EMA12vol'='EMA12vol', 'EMA144vol'='EMA36vol', 'EMA12spread'='EMA12spread', 'EVWMA12close'='EVWMA12close', 'VWAP12close'='VWAP12close', 'VWAP144close'='VWAP36close', 'HMA12close'='HMA12close', 'HMA144close'='HMA36close',                                                                 'HMA12vol'='HMA12vol', 'HMA144vol'='HMA36vol', 'RSIPAIR12'='RSIPAIR12', 'RSIPAIR144'='RSIPAIR36', 'MACDpriceE'='MACDpriceE','MACDvolE'='MACDvolE', 'MACDspreadE'='MACDspreadE'), selected='PAIRclose')
    }else{selectInput('varselectA', label='Bitfinex variables: ', choices=c('Open'='PAIRopen',	'Volume'='PAIRvolume','NLP sentiment'='raw.sent', 'NLP sentiment (weighted)'='comment.sent','Close'='PAIRclose', 'High'='PAIRhigh',	'Low'='PAIRlow',	'delta open>close' ='PAIRd', 'Bid'='PAIRbid', 'Ask'='PAIRask',	'Spread'='PAIRbaspread',	'Bid spread'='PAIRbidspread',	'Ask spread'='PAIRaskspread'), selected='PAIRclose')  }})      #pairs in exchange A
  
  output$pair.varsB <- renderUI({
    #exUIB <- exchangeB()
    if(exUIB %in% c('binance', 'Binance')){
      selectInput('varselectB', label='Binance variables: ', choices=c('Close'='PAIRclose', 'NLP sentiment'='raw.sent', 'NLP sentiment (weighted)'='comment.sent','Volume'='PAIRvolume', 'n Trades'='PAIRntrade', 'High'='PAIRhigh', 'Low'='PAIRlow', 'Bid'='PAIRbid', 'Ask'='PAIRask', 'SMA12close'='SMA12close', 'SMA144close'='SMA36close', 'SMA12spread'='SMA12spread', 'SMA12btcclose'='SMA12btcclose',  'EMA12close'='EMA12close', 'EMA144close'='EMA36close', 'EMA12vol'='EMA12vol', 'EMA144vol'='EMA36vol', 'EMA12spread'='EMA12spread', 'EVWMA12close'='EVWMA12close', 'VWAP12close'='VWAP12close', 'VWAP144close'='VWAP36close', 'HMA12close'='HMA12close', 'HMA144close'='HMA36close',                                                                 'HMA12vol'='HMA12vol', 'HMA144vol'='HMA36vol', 'RSIPAIR12'='RSIPAIR12', 'RSIPAIR144'='RSIPAIR36', 'MACDpriceE'='MACDpriceE','MACDvolE'='MACDvolE', 'MACDspreadE'='MACDspreadE'), selected='PAIRclose')
    }else{selectInput('varselectB', label='Bitfinex variables: ', choices=c('Open'='PAIRopen','Volume'='PAIRvolume', 'NLP sentiment'='raw.sent', 'NLP sentiment (weighted)'='comment.sent', 'Close'='PAIRclose', 'High'='PAIRhigh',	'Low'='PAIRlow',	 'delta open>close' ='PAIRd', 'Bid'='PAIRbid', 'Ask'='PAIRask',	'Spread'='PAIRbaspread',	'Bid spread'='PAIRbidspread',	'Ask spread'='PAIRaskspread'), selected='PAIRclose')  }})       #pairs for exchange B
  
  output$pair.controlsA <- renderUI({
    #exUIA <- exchangeA()
    #if(exUIA %in% c('binance', 'Binance')){      #render binance pairs
      selectInput('pairAInput', label='Pair A', choices=c('ADABTC', 'BNBBTC', 'BNBUSDT', 'BTCUSDT', 'EOSBTC', 'ETHBTC', 'ETHUSDT', 'IOTABTC', 'NEOBTC', 'TRXBTC', 'XLMBTC'), selected = 'BTCUSDT')
     # }else{      #render bitfinex pairs#'ADABTC', 'BCCBTC', 'BNBBTC', 'BNBUSDT', 'BTCUSDT', 'EOSBTC', 'ETHBTC', 'ETHUSDT', 'ICXBTC', 'IOTABTC', 'LTCBTC', 'NANOBTC', 'NEOBTC', 'OMGBTC', 'ONTBTC', 'TRXBTC', 'VENBTC', 'XEMBTC', 'XLMBTC', 'XMRBTC', 'XRPBTC'
     }) #selectInput('pairAInput', label='Pair A', choices=c('BCHUSD', 'BTCUSD', 'EOSUSD', 'ETHUSD', 'LTCUSD', 'XRPUSD'), selected = 'BTCUSD')    }  })   #variable selection A
  
  output$pair.controlsB <- renderUI({
    #exUIB <- exchangeB()
    #if(exUIB %in% c('binance')){      #render binance pairs
      selectInput('pairBInput', label='Pair B', choices=c('ADABTC', 'BNBBTC', 'BNBUSDT', 'BTCUSDT', 'EOSBTC', 'ETHBTC', 'ETHUSDT', 'IOTABTC', 'NEOBTC', 'TRXBTC', 'XLMBTC'), selected = 'BTCUSDT')
    #}else{      #render bitfinex pairs
    })  #selectInput('pairBInput', label='Pair B', choices=c('BCHUSD', 'BTCUSD', 'EOSUSD', 'ETHUSD', 'LTCUSD', 'XRPUSD'), selected = 'BTCUSD')    }  })   #variable selection B
  
  # output$pair.timeA <- renderUI({  #   numericInput('pair.timeA', label='Pair A time', value=30, min=15, max=150)  })
  # output$pair.timeB <- renderUI({  #   numericInput('pair.timeB', label='Pair B time', value=30, min=15, max=150)  })
  
  output$plot1 <- renderPlot({
    n <- input$varselectA
    d <- as.numeric(input$pair.timeA)
    #plottimeA <- timeA()*1440
    if(n %in% c('raw.sent', 'comment.sent')){
      nlpA.df <- nlpA()
      plot(x=c(1:(d*24)), y=as.data.frame(nlpA.df[[n]])[(nrow(nlpA.df)-(d*24)+1):nrow(nlpA.df),], type = "l", main=input$pairAInput, ylab='', xlab=paste0('Last ', d, ' days in 1hr intervals'))#round((nrow(nlpA.df)/24),1)
    }else{pairA.df <- pairA()
    plot(x=c(1:(d*288)), y=as.data.frame(pairA.df[[n]])[(nrow(pairA.df)-(d*288)+1):nrow(pairA.df),], type = "l", main=input$pairAInput, ylab='', xlab=paste0('Last ', d, ' days in 5min intervals' ))} }) # plot A   #paste0('Past ', round((nrow(pairA.df)/288),1), 'days in 5min intervals')
  
  output$plot2 <- renderPlot({
    n <- input$varselectB
    d <- as.numeric(input$pair.timeB)
    if(n %in% c('raw.sent', 'comment.sent')){
      nlpB.df <- nlpB()
      plot(x=c(1:(d*24)), y=as.data.frame(nlpB.df[[n]])[(nrow(nlpB.df)-(d*24)+1):nrow(nlpB.df),], type = "l", main=input$pairBInput, ylab='', xlab=paste0('Last ', d, ' days in 1hr intervals'))#round((nrow(nlpA.df)/24),1)
    }else{pairB.df <- pairB()
    plot(x=c(1:(d*288)), y=as.data.frame(pairB.df[[n]])[(nrow(pairB.df)-(d*288)+1):nrow(pairB.df),], type = "l", main=input$pairBInput, ylab='', xlab=paste0('Last ', d, ' days in 5min intervals' ))} }) # plot A   #paste0('Past ', round((nrow(pairA.df)/288),1), 'days in 5min intervals')
  
  output$modellistA <- renderUI({
    #look into folder for models, make choices vector
    selectInput('modelInputA', label='', choices=c('Support vector machine (radial sigma)'='svmRadialSigma', 'Random Forest'='rf', 'XGBoosted tree'='xgbTree', 'glm', 'Bayes glm'='bayesglm', 'rpart'), selected='glm')#'Ensemble: SVMrbf, rf, XGBtree, glm, Bayes glm, rpart'='ensemble'
  })  #model list A
  
  output$modellistB <- renderUI({
    #look into folder for models
    selectInput('modelInputB', label='', choices=c('Support vector machine (radial sigma)'='svmRadialSigma', 'Random Forest'='rf', 'XGBoosted tree'='xgbTree', 'glm', 'Bayes glm'='bayesglm', 'rpart'), selected='glm')#'Ensemble: SVMrbf, rf, XGBtree, glm, Bayes glm, rpart'='ensemble'
  })   #model list B
  
  output$textout1 <- renderText({
    pairA.df <- pairA()
    n <- input$varselectA
    if(n %in% c('raw.sent','comment.sent')){
      nlpA.df <- nlpA()
      Atext <- paste0(input$pairAInput, ': ', n, ' currently at ', as.character(nlpA.df[nrow(nlpA.df),n]))
    }else{    Atext <- paste0(input$pairAInput, ': ', n, ' currently at ', as.character(pairA.df[nrow(pairA.df),n]))}  })    # current text A
  
  output$textout2 <- renderText({
    pairB.df <- pairB()
    n <- input$varselectB
    if(n %in% c('raw.sent','comment.sent')){
      nlpB.df <- nlpB()
      Btext <- paste0(input$pairBInput, ': ', n, ' currently at ', as.character(nlpB.df[nrow(nlpB.df),n]))
    }else{    Btext <- paste0(input$pairBInput, ': ', n, ' currently at ', as.character(pairB.df[nrow(pairB.df),n]))}  })    # current text A
  
  output$NLPplot1 <- renderPlot({
    #exUIA <- 'Binance'#exchangeA()
    if(input$pairAInput %in% c('IOTABTC', 'NANOBTC')){
      coin <- substring(input$pairAInput, 0, 4)
    }else{coin <- substring(input$pairAInput, 0, 3)}
    aNLPcloud.df <- data.frame(fread(paste0(input$pairAInput, "/sync/WC.", coin, ".csv")))#rawToChar(get_object(paste0("s3://polabs-datasets/", exUIA, "-", input$pairAInput, "/sync/WC.", coin, ".csv")))))
    aNLP.rownames <- data.frame(aNLPcloud.df[,-1], row.names=aNLPcloud.df[,1])
    aNLP.matrix <- as.matrix(aNLP.rownames)
    oldw <- getOption("warn")
    options(warn=-1)
    par(mar=c(.1,.1,.1,.1))
    comparison.cloud(aNLP.matrix, colors = c('red','darkgreen'), scale = c(3.5,.5), random.order = FALSE, title.size = 1.5, max.words = 200)    
    options(warn = oldw)
  })    #word cloud A
  
  output$NLPplot2 <- renderPlot({
    #exUIB <- 'Binance'#exchangeB()
    if(input$pairBInput %in% c('IOTABTC', 'NANOBTC')){
      coin <- substring(input$pairBInput, 0, 4)
    }else{coin <- substring(input$pairBInput, 0, 3)}
    bNLPcloud.df <- data.frame(fread(paste0(input$pairBInput, "/sync/WC.", coin, ".csv")))#rawToChar(get_object(paste0("s3://polabs-datasets/", exUIB, "-", input$pairBInput, "/sync/WC.", coin, ".csv")))))
    bNLP.rownames <- data.frame(bNLPcloud.df[,-1], row.names=bNLPcloud.df[,1])
    bNLP.matrix <- as.matrix(bNLP.rownames)
    oldw <- getOption("warn")
    options(warn=-1)
    par(mar=c(.1,.1,.1,.1))
    comparison.cloud(bNLP.matrix, colors = c('red','darkgreen'), scale=c(3.5, .5), title.size = 1.5, random.order = FALSE, max.words = 200 )
    options(warn = oldw)
  })     #word cloud B
  
  observeEvent(input$modelActionA, {
    Alist <- list()
    pairnameA <- nameA()
    pairA.df <- pairA()
    vectorA.df <- pairA.df[((nrow(pairA.df)-750):nrow(pairA.df)),1:27]
    vectorA.df <- vectorA.df[nrow(vectorA.df):1,]#reverse
    veclist <- list(vectorA.df, vectorA.df)
    n <- 12
    veclist2 <- lapply(veclist, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
    for (dset in 1:2){
      v.ntrade <- aggregate(veclist[[dset]][,7], list(rep(1:(nrow(veclist[[dset]])%/%n+1), each=n, len=nrow(veclist[[dset]]))), sum)
      v.volume <- aggregate(veclist[[dset]][,5], list(rep(1:(nrow(veclist[[dset]])%/%n+1), each=n, len=nrow(veclist[[dset]]))), sum)
      veclist2[[dset]][,7] <- v.ntrade[1:nrow(veclist2[[dset]]),2]    #replace vol and ntrades
      veclist2[[dset]][,5] <- v.volume[1:nrow(veclist2[[dset]]),2]      }
    vectorA.df <- veclist2[[1]][nrow(veclist2[[1]]):1,]#un-reverse
    pred.vectorA <- feat.engineer(vectorA.df)
    pred.vectorA <- pred.vectorA[nrow(pred.vectorA),]
    
    #if in x COIN then add sent data
    if(pairnameA %in% c('BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')){
      #1. read BTC dataset, aggr, feat eng
      BTCdata.df <- as.data.frame(fread(paste0('/home/ubuntu/crypto/', 'BTCUSDT', '/small', 'BTCUSDT', '.csv')))
      BvectorA.df <- BTCdata.df[nrow(BTCdata.df):1,]#reverse
      Bveclist <- list(BvectorA.df, BvectorA.df)
      n <- 12
      Bveclist2 <- lapply(Bveclist, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
      for (dset in 1:2){
        v.ntrade <- aggregate(Bveclist[[dset]][,7], list(rep(1:(nrow(Bveclist[[dset]])%/%n+1), each=n, len=nrow(Bveclist[[dset]]))), sum)
        v.volume <- aggregate(Bveclist[[dset]][,5], list(rep(1:(nrow(Bveclist[[dset]])%/%n+1), each=n, len=nrow(Bveclist[[dset]]))), sum)
        Bveclist2[[dset]][,7] <- v.ntrade[1:nrow(Bveclist2[[dset]]),2]    #replace vol and ntrades
        Bveclist2[[dset]][,5] <- v.volume[1:nrow(Bveclist2[[dset]]),2]      }
      BvectorA.df <- Bveclist2[[1]][nrow(Bveclist2[[1]]):1,]#un-reverse
      Bpred.vectorA <- feat.engineer(BvectorA.df)
      Bpred.vectorA <- Bpred.vectorA[complete.cases(Bpred.vectorA),]

      #2. read in PAIR and BTC sentiment data
      btc.sent <- fread('/home/ubuntu/crypto/BTCUSDT/sync/tracker.csv')
      btc.sent <- btc.sent[,2:3]
      names(btc.sent) <- c('raw.sent.btc', 'comment.sent.btc')
      pair.sent <- fread(paste0('/home/ubuntu/crypto/', pairnameA, '/sync/tracker.csv'))
      pair.sent <- pair.sent[,2:3]
      #3. Combine sentiment x2,   btc sentiment x2
      btc.df <- Bpred.vectorA [,c(5,6,8,30,32,47)]  ########this is fully aggred from intialization
      names(btc.df) <- c('btc.price', 'btc.volume', 'btc.ntrade', 'btc.price.12h', 'btc.volume.12h', 'btc.RSI.12h')
      smallest <- min(c(nrow(btc.df), nrow(pair.sent), nrow(btc.sent), nrow(pred.vectorA)))
      pred.vectorA <- cbind(pred.vectorA[((nrow(pred.vectorA)-smallest)+1):nrow(pred.vectorA),], pair.sent[((nrow(pair.sent)-smallest)+1):nrow(pair.sent),], btc.sent[((nrow(btc.sent)-smallest)+1):nrow(btc.sent),])
      #btc price  #btc volume  ntrade #12hr btc price  #12 hr btc volume  #12hr btc RSI
      pred.vectorA <- cbind(pred.vectorA, btc.df[((nrow(btc.df)-smallest)+1):nrow(btc.df),])    }
    
    modelnameA <- input$modelInputA
    if(modelnameA %in% c('ensemble', 'stack')){
      resultsA.df <- as.data.frame(fread(paste0(pairnameA, '/models/results-', pairnameA, '.csv')))
      for(m in 1:6){        modelloaderA <- paste0(pairnameA, '/models/A', m, '-', pairnameA, '-ensemble' )
      Alist[[m]] <- readRDS(modelloaderA)      }
    }else{
      resultsA.df <- as.data.frame(fread(paste0(pairnameA, '/models/results-', pairnameA, '.csv')))
      for(m in 1:6){        modelloaderA <- paste0(pairnameA, '/models/A', m, '-', pairnameA, '-', modelnameA )
      Alist[[m]] <- readRDS(modelloaderA)      }   }
    for(c in 1:6){                                                                  #execute model
      predA <- predict(Alist[[c]], pred.vectorA)
      resultsA.df[c,1] <- ifelse(as.character(predA) %in% c("yes","Yes"),1,0)
      predA.raw <- predict(Alist[[c]], pred.vectorA, type='prob')
      if(modelnameA %in% c('ensemble', 'stack')){
        resultsA.df[c,2] <- as.double(predA.raw)
      }else{resultsA.df[c,2] <- as.double(predA.raw[1,2]) }
       if(modelnameA %in% c('ensemble', 'stack')){
         resultsA.df[c,3] <- as.double(max(Alist[[c]]$error$ROC))
       }else{resultsA.df[c,3] <- as.double(max(Alist[[c]]$results$ROC))   }  
    }
    
    resultsA.df <- cbind(c('24hr 0.5% increase:  ','24hr 1% increase:  ','24hr 2% increase:  ','24hr 0.5% decrease:  ','24hr 1% decrease:  ','24hr 2% decrease:  '), resultsA.df)
    names(resultsA.df) <- c("Classification", "Binary", "Raw probability", "Train ROC*", "Test accuracy")
    testvector <- as.data.frame(fread(file=paste0(pairnameA, '/models/test-', pairnameA, '.csv')))
    testvector <- testvector[,which(colnames(testvector) %in% c(modelnameA, modelnameA))]
    resultsA.df[,2] <- as.integer(resultsA.df[,2])
    resultsA.df[,5] <- testvector
    output$modelTableA <- renderTable(resultsA.df)#display
    remove(Alist)#clean
  })    #model A go
  
  observeEvent(input$modelActionB, {
    Blist <- list()
    pairnameB <- nameB()
    pairB.df <- pairB()
    vectorB.df <- pairB.df[((nrow(pairB.df)-750):nrow(pairB.df)),1:27]
    # fwrite(as.data.frame(c(dim(vectorA.df), "fuck")), file='vectorAdf-dims.csv')#debug
    vectorB.df <- vectorB.df[nrow(vectorB.df):1,]#reverse
    veclist <- list(vectorB.df, vectorB.df)
    n <- 12
    veclist2 <- lapply(veclist, function(x) aggregate(.~ cbind(grp = as.integer(gl(nrow(x), n, nrow(x)))), x, mean)[-1])
    for (dset in 1:2){
      v.ntrade <- aggregate(veclist[[dset]][,7], list(rep(1:(nrow(veclist[[dset]])%/%n+1), each=n, len=nrow(veclist[[dset]]))), sum)
      v.volume <- aggregate(veclist[[dset]][,5], list(rep(1:(nrow(veclist[[dset]])%/%n+1), each=n, len=nrow(veclist[[dset]]))), sum)
      veclist2[[dset]][,7] <- v.ntrade[1:nrow(veclist2[[dset]]),2]    #replace vol and ntrades
      veclist2[[dset]][,5] <- v.volume[1:nrow(veclist2[[dset]]),2]      }
    vectorB.df <- veclist2[[1]][nrow(veclist2[[1]]):1,]#un-reverse
    pred.vectorB <- feat.engineer(vectorB.df)
    pred.vectorB <- pred.vectorB[nrow(pred.vectorB),]
    
    modelnameB <- input$modelInputB
    if(modelnameB %in% c('ensemble', 'stack')){
      resultsB.df <- as.data.frame(fread(paste0(pairnameB, '/models/results-', pairnameB, '.csv')))
      for(m in 1:6){        modelloaderB <- paste0(pairnameB, '/models/A', m, '-', pairnameB, '-ensemble' )
      Blist[[m]] <- readRDS(modelloaderB)      }
    }else{
      resultsB.df <- as.data.frame(fread(paste0(pairnameB, '/models/results-', pairnameB, '.csv')))
      for(m in 1:6){        modelloaderB <- paste0(pairnameB, '/models/A', m, '-', pairnameB, '-', modelnameB )
      Blist[[m]] <- readRDS(modelloaderB)      }   }
    
    for(c in 1:6){                                                                  #execute model
      # fwrite(as.data.frame(c(dim(pred.vectorA), "fuck")), file='pred.vector-dims.csv')#debug
      predB <- predict(Blist[[c]], pred.vectorB)
      resultsB.df[c,1] <- ifelse(as.character(predB) %in% c("yes","Yes"),1,0)
      predB.raw <- predict(Blist[[c]], pred.vectorB, type='prob')
      if(modelnameB %in% c('ensemble', 'stack')){
        resultsB.df[c,2] <- as.double(predB.raw)
      }else{resultsB.df[c,2] <- as.double(predB.raw[1,2]) }
      if(modelnameB %in% c('ensemble', 'stack')){
        resultsB.df[c,3] <- as.double(max(Blist[[c]]$error$ROC))
      }else{resultsB.df[c,3] <- as.double(max(Blist[[c]]$results$ROC))    }  
    }
    resultsB.df <- cbind(c('24hr 0.5% increase:  ','24hr 1% increase:  ','24hr 2% increase:  ','24hr 0.5% decrease:  ','24hr 1% decrease:  ','24hr 2% decrease:  '), resultsB.df)
    names(resultsB.df) <- c("Classification", "Binary", "Raw probability", "Train ROC*", "Test accuracy")
    testvector <- as.data.frame(fread(file=paste0(pairnameB, '/models/test-', pairnameB, '.csv')))
    testvector <- testvector[,which(colnames(testvector) %in% c(modelnameB, modelnameB))]
    resultsB.df[,2] <- as.integer(resultsB.df[,2])
    resultsB.df[,5] <- testvector
    output$modelTableB <- renderTable(resultsB.df)#display
    remove(Blist)#clean
  })     #model B go
})

