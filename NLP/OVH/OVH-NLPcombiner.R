
setwd("/root/cryptoAPI/NLP")#setwd('C:/Users/Po/Sync/Crypto/dashboard/NLP')
pcklibs <- c('tidytext', 'data.table', 'tm', 'tidyverse', 'SnowballC', 'wordcloud', 'sentimentr')#'RColorBrewer'topicmodels'
lapply(pcklibs, require, character.only=TRUE)

pair.vector <- c('BNB','BTC','ADA','EOS','ETH','IOTA','NEO','TRX','XLM')

for (Cpair in pair.vector){
  print(Cpair)
  ovh.path <- paste0('', Cpair, '/sync/', Cpair, '-tracker.csv')
  ovh.data <- as.data.frame(fread(ovh.path))
  aws.path <- paste0('aws-api/', Cpair, '-tracker.csv')
  aws.data <- as.data.frame(fread(aws.path))
  #read in OVH current (-33 off top)
  #read in AWS old
  #rbind(AWS, OVH)
  combined.data <- rbind(aws.data, ovh.data[33:nrow(ovh.data),])
  #write
  combined.path <- paste0(Cpair, '/sync/', Cpair, '-tracker.csv')#'aws-api/combined/', Cpair, '-tracker.csv')
  fwrite(combined.data, file=combined.path)
  print(combined.path)
}