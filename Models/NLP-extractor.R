setwd("/home/ubuntu/crypto")
pcklibs <- c("dplyr", "caTools", "Metrics", "data.table", "TTR")
lapply(pcklibs, require, character.only=TRUE)

dfname.vector <- c('ETHUSDT', 'BNBUSDT', 'BTCUSDT', 'ADABTC', 'EOSBTC', 'IOTABTC', 'NEOBTC', 'XLMBTC', 'TRXBTC', 'BCCBTC', 'ICXBTC', 'LTCBTC', 'NANOBTC', 'OMGBTC', 'ONTBTC', 'VENBTC', 'XMRBTC', 'XRPBTC', 'XEMBTC')#19

for(pair in dfname.vector){
  #go into fodler and extract tracker
  tracker.df <- fread(paste0(pair, '/sync/tracker.csv'))
  print(tracker.df[1,1])
  #save tracker in new fodlerfor later download
  savepath <- paste0('NLPdump/', pair, '-NLP.csv')
  fwrite(tracker.df, file=savepath)
}
