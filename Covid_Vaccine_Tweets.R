if (!require(rtweet)) {install.packages('rtweet')}
if (!require(magrittr)) {install.packages('magrittr')}
if (!require(data.table)) {install.packages('data.table')}
if (!require(ggplot2)) {install.packages('ggplot2')}
if (!require(graphics)) {install.packages('graphics')}
if (!require(topicmodels)) {install.packages('topicmodels')}
if (!require(quanteda)) {install.packages('quanteda')}
if (!require(stats)) {install.packages('stats')}
if (!require(grDevices)) {install.packages('grDevices')}
if (!require(utils)) {install.packages('utils')}
if (!require(methods)) {install.packages('methods')}
if (!require(utf8)) {install.packages('utf8')}
install.packages("vosonSML")
library(rtweet)
library(data.table)
library(quanteda)
library(ggplot2)
?search_tweets
setwd("/Users/ritikakeshri/Documents/TBS-Classes/Social Media analytics-Jonas/My Project")
number_tweets = 3000 # search 3000 tweets every time
query = "COVID-19 vaccines OR Coronavirus vaccine OR Covid-19 vaccine OR #COVIDVaccine"
Vaccines_tweets1.df = search_tweets(query, number_tweets)
setDT(Vaccines_tweets1.df)
for (i in 1:6) {
  nrow_a = dim(Vaccines_tweets1.df)[1]
  oldest_id = Vaccines_tweets1.df$status_id[nrow_a]
  print("waiting 120s....")
  Sys.sleep(120)
  print(paste("downloading",number_tweets,"new tweets"))
  Vaccines_tweets2.df = search_tweets(query, number_tweets, max_id = oldest_id)
  Vaccines_tweets1.df = rbind(Vaccines_tweets1.df,Vaccines_tweets2.df)
  Vaccines_tweets1.df = setDT(Vaccines_tweets1.df)
  Vaccines_tweets1.df = Vaccines_tweets1.df[!duplicated(status_id)]
}
rm(Vaccines_tweets2.df)

#Changed datafile name 
Vaccines_tweets1.df=Covid19_Vaccine_Tweets.df
Covid19_Vaccine_Tweets.df <- Vaccines_tweets1.df[52000:74431]
# Dimension
dim(Covid19_Vaccine_Tweets.df)

#73431 rows by 90 columns. A complete list of column names:
colnames(Covid19_Vaccine_Tweets.df)

#In data.frames single columns like the ‘screen_name’ columns are accessed like this:
Covid19_Vaccine_Tweets.df$screen_name

#for the vector of all values in the column screen_name and
Covid19_Vaccine_Tweets.df$screen_name[c(2,4,5)]

#Equivalently we can write
#data.frame[ rows, cols] to display
Covid19_Vaccine_Tweets.df[c(2,4,5) ,'screen_name']

#or
Covid19_Vaccine_Tweets.df[c(2,4,5) ,4]


setDT(Covid19_Vaccine_Tweets.df) 

Covid19_Vaccine_Tweets.df[1:3, c(screen_name, source)]

#the data is returned as one long vector. However by quoting them as a list:
Covid19_Vaccine_Tweets.df[1:3, .(screen_name, source)]

#To see all rows where the column “country” is identical to "United States" or identical to "United Kingdom":
Covid19_Vaccine_Tweets.df[country == "United States" | country == "United Kingdom" | country == "India"| country == "Canada"]

#To see all rows where the column “country” is identical to "United States" or identical 
#to "United Kingdom" and the column source is “Instagram” do
Covid19_Vaccine_Tweets.df[(country == "United States" | country == "United Kingdom" | country == "India"| country == "Canada") & source == "Instagram"]

#We can compute functions directly on columns like:
#This add the value of the retweet_count and favorite_count columns to any tweet 
#that has retweet_count>10 and returns it as a vector.
Covid19_Vaccine_Tweets.df[retweet_count>10, (retweet_count+favorite_count)]

#.N is an integer containing the number of rows in the group. So if we want to group by 
# first country we write by=country in the grouping-slot.
Covid19_Vaccine_Tweets.df[,.(.N), by = .(country)] [order(-N)]

#to add up the retweet_count, quotes and like by country we can do
Covid19_Vaccine_Tweets.df[,.(TotalTweets = .N, 
                                      total_reactions=sum(retweet_count, na.rm = TRUE) + 
                                        sum(favorite_count, na.rm = TRUE)+
                                        sum(reply_count, na.rm = TRUE)+
                                        sum(quote_count, na.rm = TRUE)), 
                                   by = .(country)] [order(-total_reactions)]

#To group by country and verified columns, then count in each group:
Covid19_Vaccine_Tweets.df[,.(.N), by = .(country, verified)] 

#The operator := is used to create new columns in a data.table. This creates a new variable 
#“chunk” that is the result of sending the variable created_at to the function cut that 
#split the time in chunks of 5 minutes each, then letting each time chunk be a factor.
library(magrittr)
Covid19_Vaccine_Tweets.df[, chunk:= created_at %>% cut(breaks = "5 min") %>% as.factor ]

#Much of our analysis for this case will be done using the library quanteda.

dim(Covid19_Vaccine_Tweets.df)
#Types refers the number of distinct tokens
ggplot(Covid19_Vaccine_Tweets.df, aes(x=created_at, y=(friends_count+1))) +
  geom_point() +
  scale_x_datetime(name = "Time") +
  scale_y_log10(name = "Potential Reach Nov(17-18)", breaks = c(10,100,1000,10000) ) +
  theme_minimal()
#Let us make a histogram:

ggplot(Covid19_Vaccine_Tweets.df, aes(x=created_at)) +
  geom_histogram(aes(y=..count..), #make histogram
                 binwidth=600, #each bar contains number of tweets during 60 s
                 colour="blue", #colour of frame of bars
                 fill="blue", #fill colour for bars
                 alpha=0.8) + # bars are semi transparant
  ggtitle(paste0("Activity ","22432 " ,"tweets")) + #title
  scale_y_continuous(name="Number of Tweets per minute") + 
  scale_x_datetime(name = "Time") +
  theme_minimal(base_family="Times New Roman")

# In below graph each dot is a tweet matching'COVID-19 vaccines OR Coronavirus vaccine OR Covid-19 vaccine OR #COVIDVaccine

ggplot(Covid19_Vaccine_Tweets.df, aes(
  x=created_at, 
  y=(friends_count+1), 
  size = favorite_count + reply_count + quote_count + retweet_count )
) +
  geom_point(aes(size = retweet_count), alpha = 0.5) +
  ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
  scale_y_log10(name="Potential Reach",breaks = c(10,100,1000,10000) ) +
  scale_x_datetime(name = "Time") +
  scale_size_continuous(name="Retweets") +
  theme_minimal()

tok_tweets <- Covid19_Vaccine_Tweets.df$text %>% 
  gsub("#","", . ) %>% 
  corpus %>% 
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE)
head(tok_tweets,n=2)

#We can also remove stopwords, words we do not want to include in our analysis. 
#The first ten stopwords of english from the library “stopwords” are
stopwords(language = "en")[1:10]
tok_tweets <- tokens_remove(tok_tweets,stopwords(language = "en"))
head(tok_tweets,n=2)

mini_corpus = corpus(c("I like R, like", "I like Python"))
mini_docTermMatrix = dfm(mini_corpus, remove_punct=TRUE)
print(mini_docTermMatrix)


words.to.remove <- c(stopwords("english"),'Covid',"vaccine","#covid19","covid-19","vaccin","coronavirus")
dfmat_corp_twitter <- Covid19_Vaccine_Tweets.df$text %>% corpus() %>% 
  dfm(remove = words.to.remove,
      what = "word",
      stem = TRUE, 
      remove_punct = TRUE,
      remove_url=TRUE)

dfFreq <- textstat_frequency(dfmat_corp_twitter) %>% as.data.table
ggplot(dfFreq[1:20,], aes(x=feature, y=frequency)) + 
  geom_col() +
  coord_flip() +
  theme_minimal()

dfFreq[1:7,]
ggplot(dfFreq[1:20,], aes(x=reorder(feature, -rank), y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Stemmed word", y = "Count") +
  theme_minimal(base_family="Times New Roman")

#Supppose I did not know what bts_bighit refered to, I would then check the word in it’s 
#context in the tweets:
Covid19_Vaccine_Tweets.df[grepl("pfizer",text), list(text) ]
#A more elegant but perhaps less useful way of showing the word-frequencies are with
textplot_wordcloud(dfmat_corp_twitter, min_count = 6, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

dfFreq_long_top20 = dfFreq[rank <= 20] %>% 
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )

ggplot(dfFreq_long_top20, aes(x=reorder(feature,-rank), y=value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() + 
  labs(x = "", y = "Occurances in Nov 17-18", fill = "") +
  coord_flip() +
  theme_minimal()
#When we created tokens from the tweets, we set the option ngrams = 1 for counting 
#every word as a token. If we want every two words after each other to be a unit set ngrams = 2.
TokensStemmed <- tokens_remove(tok_tweets, words.to.remove)

dfm2 <- dfm(tokens_ngrams(TokensStemmed,n=2))

dfFreq2 <- textstat_frequency(dfm2)

ggplot(dfFreq2[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(name = "2 gram") +
  theme(text=element_text(size=12, family="Times New Roman"))

#Topic modelling
require(topicmodels)

dtm <- convert(dfmat_corp_twitter, to = "topicmodels")
lda <- LDA(dtm, k = 6, control=list(seed=12))
#To print the 8 most frequent term per topic we pipe the result through 
#utf8::utf8_print since there are some emojis that will not be printed otherwise:
terms(lda, 8) %>% utf8::utf8_print()

#The topic for the first four tweets:
topics(lda)[1:4]
#Let us make the vector of topics more tidy, so that we have a table with two rows, 
#the first giving the number of the tweet, the second the topic
topicAssignment = 
  data.table(
    index = lda %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda %>% topics
  )

topicAssignment %>% head(4)

#Some tweets do not have a topic (too short) so I will do the following hack to 
#assign the topic “NA” to those tweets.
Covid19_Vaccine_Tweets.df$Topic = NA # creates a new col ‘topic’, assign it to NA
Covid19_Vaccine_Tweets.df$Topic[topicAssignment$index] = topicAssignment$topic
#A time-line of tweets after topic, since colours are displayed on a continuous colour 
#scale if the value in the column is not a factor, we make the Topic a factor.
Covid19_Vaccine_Tweets.df$Topic = Covid19_Vaccine_Tweets.df$Topic %>% as.factor
Covid19_Vaccine_Tweets.df$Topic
ggplot(Covid19_Vaccine_Tweets.df, aes(x=created_at, y=Topic, col=Topic)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = FALSE) + 
  scale_size_continuous(name="Retweets")
#Topic 4 seems especially prone to retweets. Topic 2 has almost no retweets
Covid19_Vaccine_Tweets.df[,list(Total.Retweets = sum(retweet_count)),by=Topic] %>% 
  ggplot(aes(x = Topic, y = Total.Retweets)) + 
  geom_col()

Covid19_Vaccine_Tweets.df[!is.na(Topic),
          list(
            TotalTweets = .N, 
            TotalReactions=sum(retweet_count, na.rm = TRUE) + 
              sum(favorite_count, na.rm = TRUE)+
              sum(reply_count, na.rm = TRUE)+
              sum(quote_count, na.rm = TRUE),
            Reach = sum(followers_count)/10000
          ), 
          by = Topic] %>% 
  melt(id.vars = "Topic") %>% 
  ggplot(aes(x = Topic, y = value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), 
                      labels = c("Tweets","Reactions","Reach in 10,000s")) + 
  scale_y_continuous(name = "Count")

#Checking bi-grams. I will try to model my topic with 2-grams. Again I use k=6 topics

dfm2 <- dfm(tokens_ngrams(TokensStemmed,n=2))
dfm2 <- convert(dfm2, to = "topicmodels")
lda2 <- LDA(dfm2, k = 6, control=list(seed=123))
terms(lda2, 8)

topicAssignment2grams = 
  data.table(
    index = lda2 %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda2 %>% topics
  )
Covid19_Vaccine_Tweets.df$Topic2gram = NA # creates a new col ‘topic’, assign it to NA
Covid19_Vaccine_Tweets.df$Topic2gram[topicAssignment2grams$index] = topicAssignment2grams$topic
Covid19_Vaccine_Tweets.df$Topic2gram = Covid19_Vaccine_Tweets.df$Topic2gram %>% as.factor
ggplot(Covid19_Vaccine_Tweets.df, aes(x=created_at, y=Topic2gram, col=Topic2gram)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = FALSE) + 
  scale_size_continuous(name="Retweets")

Covid19_Vaccine_Tweets.df[!is.na(Topic2gram),
          list(
            TotalTweets = .N, 
            TotalReactions=sum(retweet_count, na.rm = TRUE) + 
              sum(favorite_count, na.rm = TRUE)+
              sum(reply_count, na.rm = TRUE)+
              sum(quote_count, na.rm = TRUE),
            Reach = sum(followers_count)/10000
          ), 
          by = Topic2gram] %>% 
  melt(id.vars = "Topic2gram") %>% 
  ggplot(aes(x = Topic2gram, y = value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), labels = c("Tweets","Reactions","Reach in 10,000s")) + 
  scale_y_continuous(name = "Count")
# How much does the topic overlap?
noOfTopics1gram = Covid19_Vaccine_Tweets.df$Topic %>% levels %>% length
noOfTopics2gram = Covid19_Vaccine_Tweets.df$Topic2gram %>% levels %>% length
topics1gram = matrix(0, nrow = dim(Covid19_Vaccine_Tweets.df)[1], ncol = noOfTopics1gram)
colnames(topics1gram) = paste("Topic",1:noOfTopics1gram)
topics2gram = matrix(0, nrow = dim(Covid19_Vaccine_Tweets.df)[1], ncol = noOfTopics2gram)
colnames(topics2gram) = paste("Topic",1:noOfTopics2gram)
for (i in 1:noOfTopics1gram) {
  topics1gram[,i] = as.integer(Covid19_Vaccine_Tweets.df$Topic == i)
}
for (i in 1:noOfTopics2gram) {   
  topics2gram[,i] = as.integer(Covid19_Vaccine_Tweets.df$Topic2gram == i)
}
topics1gram[is.na(topics1gram)] = 0
topics2gram[is.na(topics2gram)] = 0

diffMatrix = matrix(NA,nrow = noOfTopics1gram, ncol = noOfTopics2gram )
for (i in 1:noOfTopics1gram) {
  for (j in 1:noOfTopics2gram) {
    diffMatrix[i,j] = 
      sum(topics1gram[,i]!=topics2gram[,j])
  }
}
rownames(diffMatrix) = paste("1gram Topic",1:noOfTopics1gram)
colnames(diffMatrix) = paste("2gram Topic",1:noOfTopics2gram)

diffMatrix

# Let us check the text of the first 10 tweets…
Covid19_Vaccine_Tweets.df[Topic == 1][1:10,.(text)]

#Ok, so this is a campaign where several tweet accounts tweet almost exactly the 
#same text without retweeting. Normally I would think that these accounts are likely 
#bots and not gowern by humans. However:
ggplot(Covid19_Vaccine_Tweets.df[Topic == 1], aes(x = followers_count)) + geom_histogram(binwidth = 10) + 
  xlim(c(0,300))

#and many account are also fairly old
ggplot(Covid19_Vaccine_Tweets.df[Topic == 1], aes(x = account_created_at)) +
  geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

dim(Covid19_Vaccine_Tweets.df[Topic==1])[1]

Covid19_Vaccine_Tweets.df[Topic==1, .(hashtags)] %>% 
  unlist %>% table %>% 
  sort(decreasing = T)
#To inspect “Topic 2” I can do the two operations above in a single command Let us
#check the text of the first 10 tweets in topic == 2
Covid19_Vaccine_Tweets.df[Topic==2][1:10,.(text)]



library(tidytext)
tweet_topics <- tidy(lda, matrix = "beta") %>% as.data.table

tweet_topics[order(-beta),.SD[1:3],by = topic][order(topic)]

library(tidytext)
tweet_topics[order(-beta),.SD[1:10],by = topic] %>% 
  ggplot(aes(x = reorder_within(term,beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_reordered() + 
  coord_flip() + 
  theme_minimal()

#Hierarical clustering

similar_wrds <- textstat_simil(dfmat_corp_twitter, 
                               dfmat_corp_twitter[,c("vaccin", "effect")], 
                               margin="features")

head(as.matrix(similar_wrds), 10)
as.list(similar_wrds, n = 6)


fstat <- dfmat_corp_twitter %>% 
  dfm_trim(min_termfreq = 0.995, termfreq_type = "quantile") %>%
  textstat_dist(margin="features")

plot(hclust(as.dist(fstat)))
hc <- hclust(as.dist(fstat))
plot(hc, hang = -1, cex = 0.6)
library(dendextend)
hcd = as.dendrogram(hc)
d2 <- color_branches(hcd, k= 5, groupLabels = TRUE)
plot(d2)
hcd %>% cutree(k=5) %>% sort
kmc = kmeans(fstat,5)
kmc[["cluster"]]

topicTexts = Covid19_Vaccine_Tweets.df[,paste(text, collapse = "\n"),by=Topic]
# change name of the column for the text
colnames(topicTexts)[2] = "text"
#remove the row with the text without topic (topic == NA)
topicTexts = na.omit(topicTexts, cols = "Topic")


dfmat_corp_topics <- corpus(topicTexts,  text_field = "text") %>%
  dfm(remove = words.to.remove,
      stem = TRUE, remove_punct = TRUE, 
      what = 'word',
      remove_url=TRUE)
fstat <- dfmat_corp_topics %>% 
  textstat_simil(margin="documents")

# Network Analysis

library(vosonSML)
?Create.activity.twitter
class(Covid19_Vaccine_Tweets.df)
class(Covid19_Vaccine_Tweets.df) <- c(class(Covid19_Vaccine_Tweets.df),"datasource","twitter")
class(Covid19_Vaccine_Tweets.df)
## actor network - nodes are users who have tweeted
actorGraph <- Covid19_Vaccine_Tweets.df[] %>%      # tweets data table
  Create("actor") %>%             # Actor graph 
  Graph()            
source("graphHelpFunctions.R")
get_igraph_attrs(actorGraph)
V(actorGraph)$name %>% head(4)
V(actorGraph)$screen_name %>% head(4)
V(actorGraph)$label %>% head(4)
E(actorGraph)$edge_type %>% as.factor() %>% levels
actorGraph.simplyfied = simplify.actor.network(actorGraph, remove.loops = TRUE, delete.zero.degree = TRUE)
grep("^layout_with_.*[^[sugiyama]]*", ls("package:igraph"), value = TRUE) %>%  print
plot.actor.Graph(actorGraph.simplyfied, 
                 vertex.label = NA, 
                 layout = layout_with_kk)
top.ranked.users(actorGraph.simplyfied)[1:15]
named.users = top.ranked.users(actorGraph.simplyfied)[1:15]
actorGraph.named = label.user.network(actorGraph.simplyfied,
                                      named.users)
plot.actor.Graph(actorGraph.named,layout = layout_with_kk)

k = length(named.users)
color_vector = rainbow(k)
V(actorGraph.simplyfied)$color = rgb(0.33,0.33,0.33,0.5) # set color to grey and opacity to 50% 
V(actorGraph.simplyfied)$frame.color = rgb(1.00,1.00,1.00,1) # set color to white and opacity to 100%
V(actorGraph.simplyfied)$color[which(V(actorGraph.simplyfied)$screen_name %in% named.users)] = color_vector

V(actorGraph.simplyfied)$label = NA # remove the label 

plot.igraph(actorGraph.simplyfied, 
            layout = layout_with_kk,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.color = rgb(0.5,0.5,0.5,0.5),      ## darkgrey with opacity 30%
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0.15           ## 0 to 1 or TRUE (0.5)
) 
layout(matrix(1:2,1,2,byrow = TRUE), widths = c(5,2))

plot.igraph(actorGraph.simplyfied, 
            layout = layout_with_kk,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.color = rgb(0.5,0.5,0.5,0.5),      ## darkgrey with opacity 30%
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0.15           ## 0 to 1 or TRUE (0.5)
) 

plot.new()
legend(x = "left",      ## position, also takes x,y coordinates
       legend = named.users,
       pch = 19,              ## legend symbols see ?points
       col = color_vector,
       bty = "n",
       cex=0.5, #font size 50%
       title = "Users")
noOfTopics1gram
edge_colors = rainbow(noOfTopics1gram, alpha = 0.5) # 6 topics

E(actorGraph.simplyfied)$color <- rgb(0.33,0.33,0.33,0.5)
for (i in 1:noOfTopics1gram) {
  index <- which(
    V(actorGraph.simplyfied)$name %in% Covid19_Vaccine_Tweets.df[ Topic == i, user_id])
  E(actorGraph.simplyfied)$color[index] = edge_colors[i]
}

plot.igraph(actorGraph.simplyfied, 
            layout = layout_with_kk,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0           ## 0 to 1 or TRUE (0.5)
) 
plot.new()
legend(x = "left",      ## position, also takes x,y coordinates
       legend = paste("Topic", 1:noOfTopics1gram),
       pch = 19,              ## legend symbols see ?points
       col = edge_colors,
       bty = "n",
       cex=0.5, #font size 50%
       title = "Users")
neigbGraph = neighborhood.to.user(actorGraph, 
                                  "drsanjaygupta",
                                  k.nearest.neighbours=1)
plot.actor.Graph(neigbGraph, 
                 layout = layout_with_kk)
actorGraph %>%  neighborhood.to.user("CNN",  k.nearest.neighbours=1) %>%  
  plot.actor.Graph(vertex.label = NA, layout = layout_with_kk)
actorGraph %>% 
  simplify.actor.network %>%  
  top.ranked.users() %>% 
  head(5)
actorGraph %>% 
  simplify.actor.network %>%   
  neighborhood.to.user("CNN",  k.nearest.neighbours=2) %>% 
  plot.actor.Graph(vertex.label = NA, layout = layout_with_drl)
ranked.users = actorGraph %>% 
  simplify.actor.network %>%   
  neighborhood.to.user("CNN",  k.nearest.neighbours=2) %>% 
  top.ranked.users() %>% head(8)

actorGraph %>% 
  simplify.actor.network %>%   
  neighborhood.to.user("CNN",  k.nearest.neighbours=2) %>% 
  label.user.network(ranked.users) %>% 
  plot.actor.Graph(layout = layout_with_fr) 

#Sentiment Analysis

if (!require(sentimentr)) {install.packages('sentimentr')}
df <- Covid19_Vaccine_Tweets.df[,.(created_at,text,Topic)]
df$roundTime <- as.POSIXct(cut(df$created_at, breaks = "4 hours"))
df$roundTime <- df$created_at %>% # select created_at column
  cut(breaks = "4 hours") %>%     # cut every 5 min and group
  as.POSIXct                     # as posix clock time
?cut.POSIXt
df$text[1]
df$text[1] %>% get_sentences
df$text[1] %>% get_sentences %>% sentiment
df$text[1] %>% get_sentences %>% sentiment_by
sentiment_by_tweet = 
  df[,
     list(text %>% get_sentences %>% sentiment_by(),
          Topic)]
# In df:
#   select all rows
#          send text column to function get_sentences, then to
#          sentiment_by as above

sentiment_by_tweet
sentiment_by_Topic = 
  sentiment_by_tweet[, list(Tweets = .N,
                            ave_sentiment = mean(ave_sentiment),
                            sd_sentiment = sd(ave_sentiment),
                            Total_word_count = sum(word_count)),
                     by = Topic]
sentiment_by_Topic
t.test(sentiment_by_tweet[Topic ==1,ave_sentiment], sentiment_by_tweet[Topic ==2,ave_sentiment])
mean(sentiment_by_tweet$ave_sentiment)
df$sentiment = sentiment_by_tweet$ave_sentiment
df
ggplot(df,aes(x=roundTime, y=sentiment, fill=roundTime)) + 
  geom_boxplot(fill= "light blue")+ labs(subtitle= "Sentiment After Announcement of the vaccine")
df$roundTime <- as.factor(df$roundTime)
ggplot(df,aes(x=roundTime, y=sentiment, fill = roundTime)) + 
  geom_boxplot()+
  guides(fill=FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))+labs(subtitle= "Sentiment Score")
ggplot(df,aes(x=created_at, y=sentiment,col=roundTime)) + 
  geom_point(size=0.4, alpha=0.9) + labs(subtitle= "Sentiment Score")
  theme(legend.position="none")
#  My code
  negative=Covid19_Vaccine_Tweets.df[,list(text),] %>% 
    get_sentences() %>%              # get sentences
    extract_sentiment_terms() %>%    # extract negative terms
    .[,negative] %>%                 # select the negative colum
    unlist %>%                       # unlist
    table  %>%                     # create freq table
    sort(decreasing = TRUE) %>% 
    as.data.frame.table

negative_score =sum(negative[,2])
  negative_score
  
  postive=Covid19_Vaccine_Tweets.df[Topic == 2,.(text),] %>% 
    get_sentences() %>%              # get sentences
    extract_sentiment_terms() %>%    # extract postive terms
    .[,positive] %>%                 # select the positive colum
    unlist %>%                       # unlist
    table  %>%                       # create freq table
    sort(decreasing = TRUE) %>% 
    head(10) %>% 
    as.data.frame.table
  postive_score =sum(postive[,2])
 postive_score

#  Ends here
  

Covid19_Vaccine_Tweets.df[Topic == 1,.(text)] %>% 
  head #show the first 6 lines
Covid19_Vaccine_Tweets.df[Topic == 1,.(text)] %>% 
  get_sentences() %>% #extract all sentences
  head  # show the first 6

Covid19_Vaccine_Tweets.df[Topic == 1,.(text)] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  head                             # show first six elements 

Covid19_Vaccine_Tweets.df[Topic == 1,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE)


Covid19_Vaccine_Tweets.df[,list(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table


Covid19_Vaccine_Tweets.df[Topic == 2,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract postive terms
  .[,positive] %>%                 # select the positive colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table
