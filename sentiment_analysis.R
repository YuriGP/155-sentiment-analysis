install.packages(c("devtools", "rjson", "tm", "tau", "stringr", "plyr"),repos="http://cran.rstudio.com/")

library(devtools)
library(tm)
library(tau)
library(stringr)
library(stringi)
require(plyr)

install_github("twitteR",username="geoffjentry")
library(twitteR)

apiKey <- ""
apiSecret <- ""
accessToken <- ""
accessSecret <- ""

setup_twitter_oauth(apiKey, apiSecret, accessToken, accessSecret)


########################################################################################################################
#   Tweets are obtained with the 155 term. From the received tweets, they are sorted by those who have more retweets and by those who have been saved as favorites more times.
#   Across from the retrieved tweets, the hashtags and the words that appear most often are provided.
########################################################################################################################

SanitizeTweets <- function(tweets, removeTexts = c(), removeHashtags = c()) {
  # Sanitizes a vector of tweets. Includes the removal of undesired texts and hashtags
  #
  # Args:
  #   tweets: character vector.
  #   removeTexts: character vector with texts which tweets should be sanitized.
  #   removeHashtags: character vector with hashtags which tweets should be sanitized.
  #
  # Returns:
  #   A vector of sanitized tweets

  # Converts tweets to latin1 encoding to ease the emojis removal.
  tweets <- iconv(tweets, 'UTF-8', 'latin1', ' ')

  # Lower case
  tweets <- stri_trans_tolower(tweets)

  # Sanitizes the vector of hashtags. Converts them to lower case and removes duplicates.
  removeHashtags <- stri_trans_tolower(removeHashtags)
  removeHashtags <- removeHashtags[!duplicated(removeHashtags)]

  # Sanitizes the vector of texts. Converts them to lower case and removes duplicates.
  removeTexts <- stri_trans_tolower(removeTexts)
  removeTexts <- removeTexts[!duplicated(removeTexts)]

  # Removes hashtags and texts.
  tweets <- str_replace_all(tweets, paste0("(",paste(removeHashtags, collapse="|"),")\\b"), "")
  tweets <- str_replace_all(tweets, paste0("\\b(",paste(removeTexts, collapse="|"),")\\b"), "")

  # Replaces RT (retweets appendix), user mentions, urls, punctuations ans symbols and control characters.
  tweets <- str_replace_all(tweets, "^rt "," ")
  tweets <- str_replace_all(tweets, "@([a-z0-9_]+)"," ")
  tweets <- str_replace_all(tweets, "http(s)?://t.co/[a-z,0-9]+"," ")
  tweets <- str_replace_all(tweets, "[[:punct:]]", " ")
  tweets <- str_replace_all(tweets, "[[:cntrl:]]", " ")

  # Removes digits which are alone, we want to keep words containing numbers.
  tweets <- str_replace_all(tweets, " [[:digit:]] ", " ")

  return(tweets)
}

SanitizeHashtags <- function(hashtags) {
  # Sanitizes a vector of hashtags.
  #
  # Args:
  #   hashtags: character vector.
  #
  # Returns:
  #   A vector of sanitized hashtags

  hashtags <- iconv(hashtags, 'UTF-8', 'latin1', ' ')

  # Lower case
  hashtags <- stri_trans_tolower(hashtags)

  return (hashtags)
}

GetTweets <- function(searchString, n=10000) {
  # Searches for tweets given a search string using the Twitter API.
  # The function will strip retweets because they only add noise to the main purpose of the study.
  # Note: the language of the tweets will be on Spanish.
  #
  # Args:
  #   searchString: can be either a valid string or a hashtag.
  #   n: number of tweets to retrieve.
  #
  # Returns:
  #   A list with the tweets ($tweets), a list of found hashtags ($hashtagsCounts) and a list of the most common words on the tweets ($textCounts)

  tweets <- searchTwitter(searchString, n=n, lang="es", until='2017-11-16')
  tweets <- strip_retweets(tweets)
  tweetsDF <- twListToDF(tweets)

  # Obtains all the hashtags from the tweets and sanitizes them
  hashtags <- unlist(str_extract_all(tweetsDF$text, "#(\\d|\\w)+"))
  hashtags <- SanitizeHashtags(hashtags)

  stopWords <- stopwords("spanish")

  # A part from sanitizing the tweets, remove all the Spanish stopwords and hashtags
  sanitizedTweets <- SanitizeTweets(tweetsDF$text, stopWords, hashtags)

  # Obtain the hashtags from the tweets
  hashtagsCounts <- textcnt(hashtags, n = 1, split = NULL, method = "string", tolower = TRUE)
  hashtagsCountsDF <- data.frame(word = names(hashtagsCounts), count = c(hashtagsCounts))
  hashtagsCountsDF <- hashtagsCountsDF[order(-hashtagsCountsDF$count), ]

  # Obtian the most common words on the tweets
  textCounts <- textcnt(sanitizedTweets, n = 1, method = "string", tolower = TRUE)
  textCountsDF <- data.frame(word = names(textCounts), count = c(textCounts))
  textCountsDF <- textCountsDF[order(-textCountsDF$count), ]

  result <- list()
  result$tweets <- tweetsDF
  result$hashtagsCounts <- hashtagsCountsDF
  result$textCounts <- textCountsDF

  return(result)
}

tweets155 <- GetTweets("155", n=200000)
tweets <- tweets155$tweets
hashtags <- tweets155$hashtagsCounts
textCounts <- tweets155$textCounts

# Ordered tweets by favorite count
favOrder <- tweets[order(-tweets$favoriteCount), ]

# Ordered tweets by retweet count
rtOrder <- tweets[order(-tweets$retweetCount), ]

########################################################################################################################
#   From the retrieved tweets, a sentiment analysis is done to know the general feeling towards the 155
########################################################################################################################

ScoreSentiment <- function(tweets, positiveWords, negativeWords, .progress = 'none') {
  # Evaluates all the sentences and adds them a sentiment score depending on the matches with positiveWords and negativeWords
  #
  # Args:
  #   tweets: character vector of tweets to analyze.
  #   positiveWords: character vector of positive words.
  #   negativeWords: character vector of negative words.
  #   .progress: name of the progress bar. Refer to https://www.rdocumentation.org/packages/plyr/versions/1.8.4/topics/create_progress_bar
  #
  # Returns:
  #   A dataframe with all the tweets ($tweet) associated with the sentiment score ($score).

  # Sanitizes all the tweets
  sanitizedTweets <- SanitizeTweets(tweets)

  # Evaluate each tweet to obtain their sentiment scores
  # Positive and negative words are cleaned to be more accurate. Some of them are a set of words (pe. mancha_de_barro)
  scores <- laply(sanitizedTweets, function(tweet, positiveWords, negativeWords) {
    # Split tweet into words
    wordsList <- str_split(tweet, '\\s+')
    words <- unlist(wordsList)

    # Compare our words to the dictionaries of positive & negative terms
    positiveMatches <- match(words, positiveWords)
    negativematches <- match(words, negativeWords)

    # We obtain the TRUE/FALSE of the matches
    positiveMatches <- !is.na(positiveMatches)
    negativematches <- !is.na(negativematches)

    # Obtain the score depending the positive matches and negative matches - TRUE is treated as 1 and FALSE as 0
    score <- sum(positiveMatches) - sum(negativematches)

    return(score)
  }, str_replace_all(positiveWords, "_", " "), str_replace_all(negativeWords, "_", " "), .progress = .progress)

  scoresDF = data.frame(score = scores, tweet = tweets)
  return(scoresDF)
}

positiveWords <- scan('path/to/positive.txt', what='character')
negativeWords <- scan('path/to/negative.txt', what='character')

sentiment <- ScoreSentiment(tweets$text ,positiveWords, negativeWords, .progress = "text")
# hist(sentiment$score)

########################################################################################################################
#   From the lists of hashtags and most repeated words, it gets all the tweets that include the most repeated terms from above
#   From all the retrieved tweets, a ranking of users is made according to their popularity, the popularity of their tweets and
#    their tweets regarding the 155 article.
#   A sentiment analysis is done for each user and for all their tweets in order to know their feelings by
#    the application of the 155 article
########################################################################################################################

totalTweets <- tweets

# Get tweets from the most common words associated to 155
topWords <- as.character(head(textCounts$word))
i <- 0
totalTweets <- data.frame()
for (i in 1:length(topWords)) {
  retrievedTweets <- GetTweets(topWords[i], n=10000)
  totalTweets <- merge(totalTweets, retrievedTweets$tweets, all=TRUE)
}

# Get tweets from the most common hashtags associated to 155
topHashtags <- as.character(head(hashtags$word))
i <- 0
for (i in 1:length(topHashtags)) {
  retrievedTweets <- GetTweets(topHashtags[i], n=10000)
  totalTweets <- merge(totalTweets, retrievedTweets$tweets, all=TRUE)
}

# Total unique tweets from the exercise 1 and from the associated hashtags and words
totalTweets <- totalTweets[!duplicated(totalTweets), ]

# Number of tweets by user from our tweets collection
groupTweets <- count(totalTweets, "screenName")
# Number of retweets from our tweets collection
groupRetweets <- aggregate(retweetCount ~ screenName, data=totalTweets, sum)
# Number of favorites from our tweets collection
groupFavorites <- aggregate(favoriteCount ~ screenName, data=totalTweets, sum)
# Min date when we found a tweet for each user from our tweets collection
minDates <- aggregate(created ~ screenName, data=totalTweets, function(x) min(x))
# First tweet of each user from our tweets collection
minIds <- aggregate(id ~ screenName, data=totalTweets, function(x) min(x))

usersList <- list()
i <- 0
# For each user we obtain the most important stats and calculate the score in order to know their popularity
for (i in 1:length(totalTweets[ ,1])) {
  user <- list()
  user$name <- groupTweets[i, 1]
  user$numTweetsOnCorpus <- groupTweets[i, 2]
  user$numRetweetsOnCorpus <- groupRetweets[i, 2]
  user$numFavoritesOnCorpus <- groupFavorites[i, 2]
  user$minDateOnCorpus <- minDates[i, 2]
  user$minIdOnCorpus <- minIds[i, 2]

  # Obtain tweets from timeline
  userTimeline <- userTimeline(user$name, sinceID = user$minIdOnCorpus)
  if ( is.list(userTimeline) && length(userTimeline) > 0 ){
    tweetsDF <- twListToDF(userTimeline)
    user$tweets = tweetsDF
    user$tweetsFromMinDate <- length(tweetsDF) + 1
  }else{
    user$tweetsFromMinDate = 1
  }

  # User stats
  twitterUser <- getUser( user$name )
  user$favoritesCount <- twitterUser$favoritesCount
  user$followersCount <- twitterUser$followersCount
  user$tweetsCount <- twitterUser$statusesCount

  # Percentage of tweets over all the tweets we have
  user$percentageTweetsOnCorpus <- ( as.numeric(user$numTweetsOnCorpus) /
                                             as.numeric(user$tweetsFromMinDate) ) * 100

  # Calculate the score: percentage * (retweets + favorites) + user followers + user favorites
  user$userScore <- user$percentageTweetsOnCorpus *
    ( user$numRetweetsOnCorpus + user$numFavoritesOnCorpus ) + user$followersCount +
    user$favoritesCount

  usersList[[user$name]] <- user

}

ranking <- data.frame()
i <- 0
for (i in 1:length(usersList)) {
 ranking <- rbind(ranking,data.frame(name=usersList[[i]]$name,position=usersList[[i]]$score))
}
