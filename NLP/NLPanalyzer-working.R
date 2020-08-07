setwd("/home/ubuntu/cryptoAPI/NLP")#setwd('C:/Users/Po/Sync/Crypto/dashboard/NLP')
pcklibs <- c('tidytext', 'data.table', 'tm', 'tidyverse', 'SnowballC', 'wordcloud', 'sentimentr')#'RColorBrewer'topicmodels'
lapply(pcklibs, require, character.only=TRUE)

pair.vector <- c('BNB', 'BTC','ADA','EOS','ETH','IOTA','NEO','TRX','XLM')

for (Cpair in pair.vector){

Cpath <- paste0('/home/ubuntu/cryptoAPI/NLP/', Cpair, '/CSV')
ss.new.Reddit <- fileSnapshot(path=Cpath, file.info = F)
print(Cpair)
print(nrow(ss.new.Reddit[[1]]))
if(Cpair %in% c('BNB','BNB')){
Reddit.num <- nrow(ss.new.Reddit[[1]])
}else{Reddit.num <- nrow(ss.new.Reddit[[1]])+25}

print(Reddit.num)
if(Cpair %in% c('BTC','BTC')){
  Reddit.num <- (Reddit.num-25)
}

#1. read in reddit comment csv
read.location <- paste0(Cpair, '/CSV/', Cpair, '-nlp-', Reddit.num, '.csv')
print(read.location)
R.comment <- fread(file=read.location)
R.comment$comment <- gsub("[^[:alnum:][:blank:]+?.!,&/\\-]", "", R.comment$comment)

#2. convert to corpus
R.corpus <- Corpus(VectorSource(R.comment$comment))
R.corpus

#3. clean text: lowercase, removal of stop words
R.corpus <- tm_map(R.corpus, tolower)               # R.corpus <- tm_map(R.corpus, removePunctuation) 
R.corpus <- tm_map(R.corpus, removeNumbers)         #R.corpus <- tm_map(R.corpus, removeWords, stopwords("english"))
R.corpus <- tm_map(R.corpus, stripWhitespace)

#4. get sentiment of each element (post)
df <- data.frame(text = get("content", R.corpus))
polarity_table <- update_key(lexicon::hash_sentiment_jockers_rinker,
                             x = data.frame(
                               x = c('bust', 'centralized', 'bear', 'fud', 'fomo', 'dump', 'bottleneck', 'shitcoin', 'short', 'rekt', 'vapourware', 'decentralized', 'moon', 'lambo', 'bull', 'hodl', 'long', 'pump'),
                               y = c(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0)    ))
mytest <- get_sentences(df)
mysent <- sentiment_by(mytest, polarity_dt=polarity_table)

#5. create DF of interest + sentiment 
DF.interest <- cbind(R.comment$comment, R.comment[,c(5,7,8,11,12)], mysent[,4])

#6. normalize comment score and compute
DF.interest$comment_score.n <- (DF.interest$comment_score-min(DF.interest$comment_score))/(max(DF.interest$comment_score)-min(DF.interest$comment_score))
DF.interest$comment_score.n1 <- DF.interest$comment_score.n * 100 
DF.interest$comment.score.sent <- DF.interest$comment_score.n1 * DF.interest$ave_sentiment

#8. save CSV for pair with new raw/computed sentiment 
#save CSV dated for raw
# interest.name <- paste0(Cpair, '/CSV/interest.', Reddit.num, '.csv')
# fwrite(DF.interest, file=interest.name)
#add to CSV for current
track.location <- paste0(Cpair, '/sync/tracker.csv')
tracking <- fread(file=track.location)
tracking.new <- data.frame(n=Reddit.num, raw.sent=mean(DF.interest$ave_sentiment), comment.sent=mean(DF.interest$comment.score.sent))
tracking <- rbind(tracking, tracking.new)
fwrite(tracking, file=track.location)
print(track.location)

#9. binary sent > LDA > save graph 
DF.binary <- DF.interest[which(DF.interest$comment.score.sent != 0),]
DF.binary$binary.sent <- ifelse(DF.binary$comment.score.sent > 0, 1, 0)

#10. word cloud separated by sentiment
sents <- levels(factor(DF.binary$binary.sent))  #emos_label <- emos
# get the labels and percents
labels <-  lapply(sents, function(x) paste(x,format(round((length((DF.binary[DF.binary$binary.sent ==x,])$V1)/length(DF.binary$binary.sent)*100),2),nsmall=2),"%"))
nemo <- length(sents)
emo.docs <- rep("", nemo)
for (i in 1:nemo){
  tmp <- DF.binary[DF.binary$binary.sent == sents[i],]$V1
  emo.docs[i] <- paste(tmp,collapse=" ")}
# remove stopwords
emo.docs <- removeWords(emo.docs, stopwords("english"))
emo.docs <- removeWords(emo.docs, c('https', 'www', 'com', 'and', 'for', 'wiki', 'reddit','cryptowikis', 'also','000','edit','since','dont','got','every','this','will','can','ampsortnew','search','amprestrictsron','amprestrictsroff','ampsyntaxcloudsearch','q28and','isself3a()'))

corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

wc.name <- paste0(Cpair, '/sync/WC.', Cpair, '.csv')
print(wc.name)
fwrite(as.data.frame(tdm), file=wc.name, row.names = TRUE)


# comparison word cloud
# pdf.name <- paste0(Cpair, '/sync/WC.', Cpair, '.pdf')
# pdf(file=pdf.name)
# comparison.cloud(tdm, colors = c('red','green'), scale = c(3,.5), random.order = FALSE, title.size = 1.5)
# dev.off()

}




# Cpath <- paste0("/home/ubuntu/cryptoAPI/NLP/CSV")
# ss.new.Reddit <- fileSnapshot(path=Cpath, file.info = F)
# print(nrow(ss.new.Reddit[[1]]))
# Reddit.num <- nrow(ss.new.Reddit[[1]])
# Reddit.num <- Reddit.num-1
# print(Reddit.num)
# 
# #1. read in reddit comment csv
# R.comment <- fread(file='current/currentNLP.csv')
# R.comment$comment <- gsub("[^[:alnum:][:blank:]+?.!,&/\\-]", "", R.comment$comment)
# 
# #2. convert to corpus
# R.corpus <- Corpus(VectorSource(R.comment$comment))
# # summary(R.corpus, showmeta=TRUE)
# 
# #3. clean text: lowercase, removal of stop words
# R.corpus <- tm_map(R.corpus, tolower)               # R.corpus <- tm_map(R.corpus, removePunctuation) 
# R.corpus <- tm_map(R.corpus, removeNumbers)         #R.corpus <- tm_map(R.corpus, removeWords, stopwords("english"))
# R.corpus <- tm_map(R.corpus, stripWhitespace)
# 
# #4. get sentiment of each element (post)
# df <- data.frame(text = get("content", R.corpus))
# polarity_table <- update_key(lexicon::hash_sentiment_jockers_rinker,
#                              x = data.frame(
#                                x = c('bust', 'centralized', 'bear', 'fud', 'fomo', 'dump', 'bottleneck', 'shitcoin', 'short', 'rekt', 'vapourware', 'decentralized', 'moon', 'lambo', 'bull', 'hodl', 'long', 'pump'),
#                                y = c(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0)    ))
# mytest <- get_sentences(df)
# mysent <- sentiment_by(mytest, polarity_dt=polarity_table)
# 
# #5. create DF of interest + sentiment 
# DF.interest <- cbind(R.comment$comment, R.comment[,c(5,7,8,11,12)], mysent[,4])
# 
# #6. normalize comment score and compute
# DF.interest$comment_score.n <- (DF.interest$comment_score-min(DF.interest$comment_score))/(max(DF.interest$comment_score)-min(DF.interest$comment_score))
# DF.interest$comment_score.n1 <- DF.interest$comment_score.n * 100 
# DF.interest$comment.score.sent <- DF.interest$comment_score.n1 * DF.interest$ave_sentiment
# 
# #8. save CSV for pair with new raw/computed sentiment 
# #save CSV dated for raw
# interest.name <- paste0('CSV/interest.', Reddit.num, '.csv')
# fwrite(DF.interest, file=interest.name)
# #add to CSV for current
# tracking <- fread(file='current/tracker.csv')
# tracking.new <- data.frame(n=Reddit.num, raw.sent=mean(DF.interest$ave_sentiment), comment.sent=mean(DF.interest$comment.score.sent))
# tracking <- rbind(tracking, tracking.new)
# fwrite(tracking, file='current/tracker.csv')
# 
# #9. binary sent > LDA > save graph 
# DF.binary <- DF.interest[which(DF.interest$comment.score.sent != 0),]
# DF.binary$binary.sent <- ifelse(DF.binary$comment.score.sent > 0, 1, 0)
# 
# #10. word cloud separated by sentiment
# sents = levels(factor(DF.binary$binary.sent))  #emos_label <- emos
# # get the labels and percents
# labels <-  lapply(sents, function(x) paste(x,format(round((length((DF.binary[DF.binary$binary.sent ==x,])$V1)/length(DF.binary$binary.sent)*100),2),nsmall=2),"%"))
# nemo = length(sents)
# emo.docs = rep("", nemo)
# for (i in 1:nemo){
#   tmp = DF.binary[DF.binary$binary.sent == sents[i],]$V1
#   emo.docs[i] = paste(tmp,collapse=" ")}
# # remove stopwords
# emo.docs = removeWords(emo.docs, stopwords("english"))
# emo.docs = removeWords(emo.docs, c('https', 'www', 'com', 'and', 'for', 'wiki', 'reddit','cryptowikis'))
# 
# corpus = Corpus(VectorSource(emo.docs))
# tdm = TermDocumentMatrix(corpus)
# tdm = as.matrix(tdm)
# colnames(tdm) = labels
# 
# # comparison word cloud
# #Cwc <- comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5)
# 
# fwrite(as.data.frame(tdm), file='current/WC.all.csv', row.names = TRUE)
# 
# #11. save word cloud
# # pdf(file='current/WC.overall.pdf')
# # comparison.cloud(tdm, colors = c('red','green'), scale = c(3,.5), random.order = FALSE, title.size = 1.5)
# # dev.off()
# 




