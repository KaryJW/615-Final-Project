library(knitr)
library(tm)
library(kableExtra)
library(devtools)
library(twitteR)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(ggmap)

#twitter setup 
api_key <- 	"eObfQ5EtrT0ryjSTrPFoGC7kM"
api_secret <- "asm7zEQwiiWB9DZzYeUfUSB8gUeJzknCnLvD1sVzdaDmlJlcSQ"
access_token <- "938520753326247937-s5l1XCbZ4LFCdSGB7syLXpT50xR91br"
access_token_secret <- "C8aHFFL8JCZwWUSyb7CQGOt5hkEKxotlOGJcTsVGiKNZu"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Search Topic
a11 <- searchTwitter("#Rihanna+#rihanna",n=5000)
a12 <-twListToDF(a11)
write.csv(a12,file = "Rihanna.csv",row.names=TRUE)
a10<- read.csv("Rihanna.csv",header = T)
a13 <- -1*is.na(a10$longitude)+1
sum(a13)
a14 <- which(a13==1)
locationa11 <-data.frame(a12$latitude[a14],a12$longitude[a14])
colnames(locationa11) <- c("latitude","longitude")
locationa11$longitude<-as.numeric(locationa11$longitude)
locationa11$latitude <-as.numeric(locationa11$latitude)  
map.world <- map_data(map = "world")
world1 <-ggplot() + geom_polygon(data = map.world, aes(x=long, y = lat, group = group),fill="rosybrown",color="rosybrown") + 
  coord_fixed(1.3)+geom_point(data= locationa11,
                              aes(x = longitude, y = latitude),
                              colour = "salmon",
                              alpha = 0.5,
                              size = 0.5)



# The count of tweets by platform.
a12$statusSource =substr(a12$statusSource,
                                     regexpr('>',a12$statusSource) + 1,
                                     regexpr('</a>',a12$statusSource)- 1)
a12_platform <- a12 %>% group_by(statusSource) %>% 
  summarize(n = n()) %>%
  mutate(percent_of_tweets = n/sum(n)) %>% arrange(desc(n))
a12_platform$percent_of_tweets <- round(a12_platform$percent_of_tweets, digits = 3)
a12_plat_5 <- a12_platform %>% top_n(5)
kable(a12_plat_5)


#Barplot of the count of the source
ggplot(a12_plat_5,aes(statusSource,y=n))+geom_bar(stat = "identity",width = 0.5,fill="lightpink")+
  coord_flip()+geom_text(aes(label=n),vjust=-.3,size=3.5)+
  theme_minimal()

#Clean the data
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
a15 <- a12 %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)

#Sentiment of total
a15_sentiment <- a15 %>% inner_join(nrc, by = "word")
a15_sent <- a15_sentiment %>% group_by(sentiment) %>% summarize(n = n()) %>% arrange(desc(n))

ggplot(a15_sent,aes(sentiment,y=n))+geom_bar(stat = "identity",fill="lightpink1")+geom_text(aes(label=n))+
  theme_minimal()

a16_words <- a15 %>% group_by(word) %>% summarize(n = n()) %>% arrange(desc(n)) %>% top_n(10)

#sentiment of top 5 data source

DS <- c("Twitter for iPhone","Twitter for Android","Twitter Web Client","TweetDeck","Twitter Lite")
a15_Source0 <- a15 %>% filter(statusSource %in% DS)
a15_Source <- a15_Source0 %>%
  group_by(statusSource) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, statusSource, total_words)

a15_Source5 <- a15_Source0 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  tidyr::complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(a15_Source) %>%
  group_by(statusSource, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

a16_Source <- a15_Source5 %>% group_by(statusSource) %>% mutate(tot_sentiment = sum(words))

a16_Source <- a16_Source %>% mutate(percent_of_tweets = (words / tot_sentiment) * 100)
a16_Source$percent_of_tweets <- round(a16_Source$percent_of_tweets, digits = 2)


a17 <- a16_Source %>% select(statusSource, sentiment, percent_of_tweets)

ggplot(a17, aes(x=statusSource, y=percent_of_tweets, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "stack",width = 0.5) +
  scale_fill_brewer(palette = "RdBu") + xlab("Platform") +
  ylab("Percent of Tweets") + theme(axis.text.x =element_text(angle = 60,hjust = 1))+
  ggtitle("Sentiment of Different Source of Tweets")


#Wordclouds Analysis
str(a15$word) 
a15word <- as.character(a15$word)
a15text <- Corpus(VectorSource(a15word))
a15text <- a15text %>%
  tm_map(removePunctuation)%>% 
  tm_map(removeNumbers)%>% 
  tm_map(stripWhitespace)%>%
  tm_map(tolower)%>% 
  tm_map(removeWords, stopwords("english"))

a15text <- tm_map(a15text,stemDocument)
a15count <- as.matrix(TermDocumentMatrix(a15text))
a15fre <- sort(rowSums(a15count),decreasing = T)
a15fd <- data.frame(word=names(a15fre),freq=a15fre)
head(a15fre)

ggplot(a15fd[1:10,],aes(x=reorder(factor(word), -freq),y=freq))+geom_bar(stat="identity",aes(fill=factor(word)))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

set.seed(200)
png("wordcloud_riri.png", width=700,height=580)
wordcloud(words = names(a15fre),freq = a15fre,max.words=100, min.freq =2,scale=c(6, .6), 
          random.order=FALSE, rot.per=0.35,random.color = F, 
          colors= c("violetred","slateblue1","tomato"))
dev.off()





################################# Taylor Swift
aT1 <- searchTwitter("#TaylorSwift+#taylorswift",n=5000)
aT2 <-twListToDF(aT1)
write.csv(aT2,file = "TS.csv",row.names=TRUE)
aT0 <-read.csv("TS.csv",header = T)
aT3 <- -1*is.na(aT0$longitude)+1
sum(aT3)
aT4 <- which(aT3==1)
locationaT1 <-data.frame(aT2$latitude[aT4],aT2$longitude[aT4])
colnames(locationaT1) <- c("latitude","longitude")
locationaT1$longitude<-as.numeric(locationaT1$longitude)
locationaT1$latitude <-as.numeric(locationaT1$latitude)  
map.world <- map_data(map = "world")
worldT2 <-ggplot() + geom_polygon(data = map.world, aes(x=long, y = lat, group = group),color="slategray",fill="slategray") + 
  coord_fixed(1.3)+geom_point(data= locationaT1,
                              aes(x = longitude, y = latitude),
                              colour = "sienna1",
                              alpha = 0.5,
                              size = 0.5)

#Count difference source
aT2$statusSource =substr(aT2$statusSource,
                         regexpr('>',aT2$statusSource) + 1,
                         regexpr('</a>',aT2$statusSource)- 1)
aT2_platform <- aT2 %>% group_by(statusSource) %>% 
  summarize(n = n()) %>%
  mutate(percent_of_tweets = n/sum(n)) %>% arrange(desc(n))
aT2_platform$percent_of_tweets <- round(aT2_platform$percent_of_tweets, digits = 3)
aT2_plat_5 <- aT2_platform %>% top_n(5)
kable(aT2_plat_5)

#Barplot of the count of the source
ggplot(aT2_plat_5,aes(statusSource,y=n))+geom_bar(stat = "identity",width = 0.5,fill="khaki")+
  coord_flip()+geom_text(aes(label=n),vjust=-.3,size=3.5)+
  theme_minimal()

#Clean the data
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
aT5 <- aT2 %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)

#Sentiment of total
aT5_sentiment <- aT5 %>% inner_join(nrc, by = "word")
aT5_sent <- aT5_sentiment %>% group_by(sentiment) %>% summarize(n = n()) %>% arrange(desc(n))

ggplot(aT5_sent,aes(sentiment,y=n))+geom_bar(stat = "identity",fill="khaki1")+geom_text(aes(label=n))+
  theme_minimal()+coord_flip()

aT6_words <- aT5 %>% group_by(word) %>% summarize(n = n()) %>% arrange(desc(n)) %>% top_n(10)

#sentiment of top 5 data source
DS <- c("Twitter for iPhone","Twitter for Android","Twitter Web Client","TweetDeck","Twitter Lite")
aT5_Source0 <- aT5 %>% filter(statusSource %in% DS)
aT5_Source <- aT5_Source0 %>%
  group_by(statusSource) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, statusSource, total_words)


aT5_Source5 <- aT5_Source0 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  tidyr::complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(aT5_Source) %>%
  group_by(statusSource, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

aT6_Source <- aT5_Source5 %>% group_by(statusSource) %>% mutate(tot_sentiment = sum(words))

aT6_Source <- aT6_Source %>% mutate(percent_of_tweets = (words / tot_sentiment) * 100)
aT6_Source$percent_of_tweets <- round(aT6_Source$percent_of_tweets, digits = 2)


aT7 <- aT6_Source %>% select(statusSource, sentiment, percent_of_tweets)

ggplot(aT7, aes(x=statusSource, y=percent_of_tweets, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "stack",width = 0.5) +
  scale_fill_brewer(palette = "RdBu") + xlab("Platform") +
  ylab("Percent of Tweets") + theme(axis.text.x =element_text(angle = 60,hjust = 1))+
  ggtitle("Sentiment of Different Source of Tweets")




#Wordclouds Analysis
str(aT5$word) 
aT5word <- as.character(aT5$word)
aT5text <- Corpus(VectorSource(aT5word))
aT5text <- aT5text %>%
  tm_map(removePunctuation)%>% 
  tm_map(removeNumbers)%>% 
  tm_map(stripWhitespace)%>%
  tm_map(tolower)%>% 
  tm_map(removeWords, stopwords("english"))

aT5text <- tm_map(aT5text,stemDocument)
aT5count <- as.matrix(TermDocumentMatrix(aT5text))
aT5fre <- sort(rowSums(aT5count),decreasing = T)
aT5fd <- data.frame(word=names(aT5fre),freq=aT5fre)
head(aT5fre)

ggplot(aT5fd[1:10,],aes(x=factor(word),y=freq))+geom_bar(stat="identity",aes(fill=factor(word)))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

set.seed(200)
png("wordcloud_R.png", width=780,height=480)
wordcloud(words = names(aT5fre),freq = aT5fre,max.words=200, min.freq =2,scale=c(6, .6), 
          random.order=FALSE, rot.per=0.35,random.color = F, 
          colors= c("royalblue1","palegreen1","violetred3"))
dev.off()






