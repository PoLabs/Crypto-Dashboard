
#this splits caret model lists into singles and saves as RDS
setwd('C:/Users/Po/Desktop/crypto data/8.11.2018')
pairs <- c('ADABTC', 'BNBBTC', 'BNBUSDT', 'EOSBTC', 'ETHBTC', 'ETHUSDT', 'NEOBTC', 'TRXBTC', 'XLMBTC') 

for(pair in pairs){
  if(pair %in% c('BNBBTC','bnbbtc')){
    for(t in 1:3){
      nameE <- paste0("lists/A", t, "-", pair, "-modellist")
      currentList <- readRDS(nameE)  
      for(i in 1:6){
        nameM <- paste0("A", t, "-", pair, '-', currentList[[i]]$method)
        print(nameM)
        saveRDS(currentList$models[[i]], nameM)
      }
    }
  }else{
    for(t in 1:6){#read in 6 ensemble times
      nameE <- paste0("lists/A", t, "-", pair, "-modellist")
      currentList <- readRDS(nameE)
      for(i in 1:6){
        nameM <- paste0("A", t, "-", pair, '-', currentList[[i]]$method)
        print(nameM)
        saveRDS(currentList$models[[i]], nameM)
      } 
    }
  }
}