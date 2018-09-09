
#this splits caret model lists into singles and saves as RDS
setwd('C:/Users/Po/Desktop/crypto data/9.02.2018')#'TRXBTC', 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'XLMBTC','ETHUSDT', 'BNBUSDT', 'BNBBTC', 'ETHBTC', 
pairs <- c( 'BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')

currentList <- list()
for(pair in pairs){
  if(pair %in% c('BNBBTC','bnbbtc')){
    for(t in 1:3){
      nameE <- paste0("lists/A", t, "-", pair, "-modellist")
      currentList[[t]] <- readRDS(nameE)  
      for(m in 1:6){
        nameM <- paste0("A", t, "-", pair, '-', currentList[[t]][[m]]$method)
        print(nameM)
        saveRDS(currentList[[t]][[m]], nameM)
      }
    }
  }else{
    for(t in 1:6){#read in 6 ensemble times
      nameE <- paste0("lists/A", t, "-", pair, "-modellist")
      currentList[[t]] <- readRDS(nameE)
      for(m in 1:6){
        nameM <- paste0("A", t, "-", pair, '-', currentList[[t]][[m]]$method)
        print(nameM)
        saveRDS(currentList[[t]][[m]], nameM)
      } 
    }
  }
}