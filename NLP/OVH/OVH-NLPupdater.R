setwd("/root/cryptoAPI/NLP")#setwd('C:/Users/Po/Sync/Crypto/dashboard/NLP')#
library(RedditExtractoR)#install.packages('RedditExtractoR')
library('httr')
library(data.table)

pair.vector <- c('BNB','BTC','ETH','NEO','TRX','IOTA','ADA','EOS','XLM')

for(Cpair in pair.vector){
Cpath <- paste0(Cpair, '/raw')
print(Cpath)
ss.new.Reddit <- fileSnapshot(path=Cpath, file.info = F)
Reddit.num <- nrow(ss.new.Reddit[[1]])-1

#2. load CCtop100 snapshot from cronjob
print(paste0(Cpair, '/raw/hot.json@limit=50.', Reddit.num))
Reddit.urls <- rjson::fromJSON(file=paste0(Cpair, '/raw/hot.json@limit=50.', Reddit.num))#"raw/hot.json@limit=100.", Reddit.num))

#3. extract urls from list
urlvector <- character(50)    #to 100
for(i in 1:50){               #also to 102
  urlvector[i] <- Reddit.urls$`data`$children[[i]]$data$permalink }

#4. combine w formatting for reddict extractor # add 'http://www.reddit.com' 
urlvector.long <- paste0('http://www.reddit.com', urlvector)

#5. run redditextractor
Reddit.comments <- reddit_content(urlvector.long)

#6. save new csv
NLPcsv <- paste0(Cpair, '/CSV/', Cpair, '-nlp-', Reddit.num, '.csv' )
print(NLPcsv)
fwrite(Reddit.comments, file=NLPcsv)
# fwrite(Reddit.comments, file='current/currentNLP.csv')
}
