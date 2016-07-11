library(devtools)
library(rjson)
library(twitteR)
api_key <- "tRgTEVGgGsOkoJWAydtza9x07"
api_secret <- "FFpQxKB8AqITpuw3YfgpmlKdGCiozMgBf3MLU1MCZv6YgaCQeS"
access_token <- "29677130-YA85NYmorCuw7Et9bZVNkF0gDCoFBRHJPnOun0gPP"
access_token_secret <- "ikcvYMdd71IpwVKmT4BjUMAOJwpgX1abnqit9jy0p95Ys"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#What people are talking about datascience

DStweet <- searchTwitter("datascience")
dfTweet <- twListToDF(DStweet)
tweetText <- dfTweet$text
View(dfTweet)

user <- getUser("abbass_sharif")
abbass <- userTimeline(user,n=500)
abbassDF <- twListToDF(abbass)
abbassText <- abbassDF$text

library(stringr)

abbassHashTags <- str_extract_all(abbassText,"#\\w+")
freq <- table(unlist(abbassHashTags))

library(wordcloud)
wordcloud(words=names(freq),freq,random.order = FALSE, colors = "green")
