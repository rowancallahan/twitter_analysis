library(twitteR)
library(leaflet)
library(maps)


#using code and expertise from:
#https://www.r-bloggers.com/twitter-sentiment-analysis-with-r/
#https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
#https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e
login_info <- read.csv("~/Projects/twitter_analysis/login_info.txt", row.names=1)
consumer_key <-    login_info$key_value[1] %>% as.character
consumer_secret <- login_info$key_value[2] %>% as.character
access_token <-    login_info$key_value[3] %>% as.character
access_secret <-   login_info$key_value[4] %>% as.character
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

search_term <- "#Tide OR ocean OR waves"
location_term <- '0.0,0.0,10000mi'
tw = twitteR::searchTwitter(search_term, n = 10000,lang="en",retryOnRateLimit = 5)
d = twitteR::twListToDF(tw)
d$latitude <- as.numeric(d$latitude)
d$longitude <- as.numeric(d$longitude)

d<- d[!is.na(d$longitude),]

#Its now time to do some sentiment analysis, package into rshiny
#and to make things look good
#will probably have to add something for the stupid oauth

#first we parse the data on positive and negative sentiments
#https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
#This is the file that contains the words that are positive and neg
#We will parse this file and then match different words to it to 
#get our final sentiment analysis

pos_words <- as.character(read.csv("positive-words.txt",header=FALSE, comment.char = ";")$V1)
neg_words <- as.character(read.csv("negative-words.txt",header=FALSE, comment.char = ";")$V1)


score_sentiment <- function(sentence){
    require(stringr)

    sentence <- gsub('[[:punct:]]', "", sentence) %>% 
      gsub('[[:cntrl:]]', "", .) %>%
      gsub('\\d+', "", .) %>%
      tolower

    word_list <- str_split(sentence, '\\s+') %>% unlist
    pos_match <- match(word_list, pos_words)
    neg_match <- match(word_list, neg_words)

    pos <- word_list %in% pos_words %>% sum %>% ifelse(is.na(.),0, .)
    neg <- word_list %in% neg_words %>% sum %>% ifelse(is.na(.),0, .)
    return(pos - neg)

}

d$sentiment <- sapply(d$text,score_sentiment)


#lets add a color pallette so we can actually see whats going on
#coloring explained by http://rstudio.github.io/leaflet/colors.html
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = d$sentiment)


m <- leaflet(d) %>% addTiles()
m %>% addCircleMarkers(lng = ~longitude, lat = ~latitude,
                 popup = d$text, weight = 5, radius = 5,
                 color = ~pal(d$sentiment), stroke = TRUE,
                 opacity = 0.99,fillOpacity = 0.9) %>%
      addLegend(pal = pal, values = ~d$sentiment,
                 group = "circles",title ="Comment Sentiment",
                 position = "bottomleft")

#Congratulations! you now have sentiment analysis and leaflet working!
#There is much more you could do with the sentiment analysis and this could be just a starting point
