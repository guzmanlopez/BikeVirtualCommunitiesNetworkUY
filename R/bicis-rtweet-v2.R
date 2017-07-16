
# Load libraries ----------------------------------------------------------

library('rtweet') # twitter API
# library('Rfacebook') # facebook API
library('RNeo4j') # Neo4j graph db
library('igraph') # plot graph
library('ggplot2') # plots
library(stringr)

# OAauth ------------------------------------------------------------------

# For rtweet
# twitter_token <- create_token(
# app = "bikes_rr",
# consumer_key = "iVO77igHSTuy7JJ6bjDf8i6Yx",
# consumer_secret = "yQsEBplxsbZm34vlPdfLMhdvrL2BMYxL81gR5aUdivweKnDhtL"
# )

## First time only
# source(file = "twitter-OAuth.R")
## path of home directory
# home_directory <- path.expand("~")
## combine with name for token
# file_name <- file.path(home_directory, "twitter_token.rds")
## save token to home directory
# saveRDS(twitter_token, file = file_name)

# create empty .Renviron text file in home, after that:
# cat(paste0("TWITTER_PAT=", file_name),
#     file = file.path(home_directory, ".Renviron"),
#     append = TRUE)

# read user and pass from external R script
#source(file = "neo4j-auth.R")


getHashtags = function(htags) {
  hashtags = unlist(strsplit(htags, " ", TRUE))
  hashtags = tolower(hashtags)
  
  if(length(hashtags) > 0) {
    return(hashtags)
  } else {
    return(NULL)
  }
}

getMentions = function(mtions) {
  mentions = unlist(strsplit(mtions, " ", TRUE))
  
  if(length(mentions) > 0) {
    return(mentions)
  } else{
    return(NULL)
  }
}

get_tweeter_user_name=function(user_id){
  return("unknow")
}

# Neo4j graph database ----------------------------------------------------
graph = startGraph("http://localhost:7474/db/data/")
clear(graph) # Remove all

# Add constraints to db
addConstraint(graph, label = "Tweet", key = "status_id")
addConstraint(graph, label = "User", key = "user_id")
addConstraint(graph, label = "Hashtag", key = "name")
addConstraint(graph, label = "Link", key = "url")
addConstraint(graph, label = "Source", key = "name")

# Create graph DB
create_db = function(x) {

  tweet = getOrCreateNode(graph, "Tweet", status_id = x$status_id, text = x$text)
  user = getOrCreateNode(graph, "User", user_id = x$user_id, screen_name = x$screen_name)
  createRel(user, "POSTS", tweet)

  reply_to_sn = x$in_reply_to_status_screen_name

  if (length(reply_to_sn) > 0) {
    reply_user = getOrCreateNode(graph, "User", user_id = x$in_reply_to_status_user_id, username = reply_to_sn)
    createRel(tweet, "REPLY_TO", reply_user)
  }

  

  if (x$is_retweet) {
    retweet_status_user_name=get_tweeter_user_name(x$retweet_status_user_id)
    retweet_user = getOrCreateNode(graph, "User", user_id=x$retweet_status_user_id, username = retweet_status_user_name)
    createRel(tweet, "RETWEETS", retweet_user)
  }
  

  # hashtags = getHashtags(x$hashtags)
  # 
  # if (!is.null(hashtags)) {
  #   hashtag_nodes = lapply(hashtags, function(h) getOrCreateNode(graph, "Hashtag", hashtag = h))
  #   lapply(hashtag_nodes, function(h) createRel(tweet, "HASHTAG", h))
  # }
  # 
  # mentions = getMentions(x$mentions_screen_name)
  # 
  # if (!is.null(mentions)) {
  #   mentioned_users = lapply(mentions, function(m) getOrCreateNode(graph, "User", username = m))
  #   lapply(mentioned_users, function(u) createRel(tweet, "MENTIONED", u))
  # }

}

# Get Tweets related to bikes promotion (Ciclovias, Transito seguros, Ciclistas, ) 

cv_t = search_tweets("Ciclovias", n = 100, lang = "es") # Run on 
ts_t = search_tweets("'Transito seguros'", n = 100, lang = "es") # Run on 
cc_t = search_tweets("Ciclistas", n = 100, lang = "es") # Run on 
mc_t = search_tweets("MasaCriticaMvd", n = 100, lang = "es") # Run on 

#ciclo_tweets=c(cv_t, ts_t, cc_t, mc_t)
#lapply(cv_t, create_db)

for (i in 1:nrow(cv_t)) {
  create_db(cv_t[i,])
}

for (i in 1:nrow(ts_t)) {
  create_db(ts_t[i,])
}

for (i in 1:nrow(cc_t)) {
  create_db(cc_t[i,])
}

for (i in 1:nrow(mc_t)) {
  create_db(mc_t[i,])
}

summary(graph)

