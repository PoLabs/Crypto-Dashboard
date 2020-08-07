setwd("/home/ubuntu/cryptoAPI/NLP")#setwd('C:/Users/Po/Sync/Crypto/dashboard/NLP')#
library(RedditExtractoR)#install.packages('RedditExtractoR')
library('httr')
library(data.table)
# 
# #1. check for most recent reddit urls
# ss.new.Reddit <- fileSnapshot(path="/home/ubuntu/cryptoAPI/NLP/raw", file.info = F)
# Reddit.num <- nrow(ss.new.Reddit[[1]])-1
# 
# #2. load CCtop100 snapshot from cronjob
# Reddit.urls <- rjson::fromJSON(file=paste0('raw/hot.json@limit=50.', Reddit.num))#"raw/hot.json@limit=100.", Reddit.num))
# 
# #3. extract urls from list
# urlvector <- character(50)    #to 100
# for(i in 1:52){               #also to 102
#   urlvector[i] <- Reddit.urls$`data`$children[[i]]$data$permalink }
# 
# #4. combine w formatting for reddict extractor # add 'http://www.reddit.com' 
# urlvector.long <- paste0('http://www.reddit.com', urlvector)
# 
# #5. run redditextractor
# Reddit.comments <- reddit_content(urlvector.long)
# 
# #6. save new csv
# NLPcsv <- paste0("CSV/reddit-nlp-",Reddit.num,".csv" )
# fwrite(Reddit.comments, file=NLPcsv)
# fwrite(Reddit.comments, file='current/currentNLP.csv')




pair.vector <- c('BCC','LTC','OMG','ONT','ICX','VEN','XMR','XRP','NANO','XEM')

for(Cpair in pair.vector){
Cpath <- paste0("/home/ubuntu/cryptoAPI/NLP/", Cpair, '/raw')
ss.new.Reddit <- fileSnapshot(path=Cpath, file.info = F)
Reddit.num <- nrow(ss.new.Reddit[[1]])-1

#2. load CCtop100 snapshot from cronjob
Reddit.urls <- rjson::fromJSON(file=paste0(Cpair, '/raw/hot.json@limit=50.', Reddit.num))#"raw/hot.json@limit=100.", Reddit.num))
#3. extract urls from list
urlvector <- character(50)    #to 100
for(i in 1:52){               #also to 102
  urlvector[i] <- Reddit.urls$`data`$children[[i]]$data$permalink }
#4. combine w formatting for reddict extractor # add 'http://www.reddit.com' 
urlvector.long <- paste0('http://www.reddit.com', urlvector)
#5. run redditextractor
Reddit.comments <- reddit_content(urlvector.long)
#6. save new csv
NLPcsv <- paste0(Cpair, '/CSV/', Cpair, '-nlp-', Reddit.num, '.csv' )
fwrite(Reddit.comments, file=NLPcsv)
# fwrite(Reddit.comments, file='current/currentNLP.csv')
}










# dnm.corpus <- corpus(market.df$description) 
# summary(dnm.corpus)
# docvars(dnm.corpus, 'price') <- market.df$price
# docvars(dnm.corpus, 'price') <- market.df$price
# docvars(dnm.corpus, 'price') <- market.df$price
# summary(dnm.corpus)
# 
# #compute overall + each crypto 
#   #sentiment
#   #content
#   
