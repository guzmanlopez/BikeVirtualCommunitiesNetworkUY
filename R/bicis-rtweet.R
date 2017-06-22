
# Load libraries ----------------------------------------------------------

library('rtweet')
library('Rfacebook')
library('RNeo4j')
library('igraph')
library('ggplot2') # plots

# OAauth ------------------------------------------------------------------

## First time only

# source(file = "twitter-OAuth.R")

## path of home directory
# home_directory <- path.expand("~")

## combine with name for token
# file_name <- file.path(home_directory, "twitter_token.rds")

## save token to home directory
# saveRDS(twitter_token, file = file_name)

# create .Renviron text file in home

# cat(aste0("TWITTER_PAT=", file_name),
    # file = file.path(home_directory, ".Renviron"),
    # append = TRUE)

# Neo4j graph database ----------------------------------------------------

source(file = "neo4j-auth.R")

graph = startGraph("http://localhost:7474/db/data/",
                   username = neo4jUser, password = neo4jPass)
clear(graph) # Remove all

# Add constraints
addConstraint(graph, label = "Tweet", key = "id")
addConstraint(graph, label = "User", key = "screen_name")
addConstraint(graph, label = "Hashtag", key = "name")
addConstraint(graph, label = "Link", key = "url")
addConstraint(graph, label = "Source", key = "name")

create_db = function(x) {

  tweet = getOrCreateNode(graph, "Tweet", id = x$id, text = x$text)
  user = getOrCreateNode(graph, "User", username = x$screenName)
  createRel(user, "POSTS", tweet)

  reply_to_sn = x$replyToSN

  if (length(reply_to_sn) > 0) {
    reply_user = getOrCreateNode(graph, "User", username = reply_to_sn)
    createRel(tweet, "REPLY_TO", reply_user)
  }

  retweet_sn = getRetweetSN(x$text)

  if (!is.null(retweet_sn)) {
    retweet_user = getOrCreateNode(graph, "User", username = retweet_sn)
    createRel(tweet, "RETWEETS", retweet_user)
  }

  hashtags = getHashtags(x$text)

  if (!is.null(hashtags)) {
    hashtag_nodes = lapply(hashtags, function(h) getOrCreateNode(graph, "Hashtag", hashtag = h))
    lapply(hashtag_nodes, function(h) createRel(tweet, "HASHTAG", h))
  }

  mentions = getMentions(x$text)

  if (!is.null(mentions)) {
    mentioned_users = lapply(mentions, function(m) getOrCreateNode(graph, "User", username = m))
    lapply(mentioned_users, function(u) createRel(tweet, "MENTIONED", u))
  }
}


# piping example
#The pipe (%>%) simply passes along what’s on the left-hand side to the right-hand side of the line of code. Once it goes through the pipe, the output is assigned to the period (.), and it is by default plugged into the first argument of the function on the right-hand side of the pipe.

c(1:20) %>%
  sample(1) %>%
  paste0("your lucky number is ", .)


# Get Followers/Follows data ----------------------------------------------

# Look up for and get users
MasaCriticaMvd <- search_users(q = "MasaCriticaMvd", n = 1)
MasaCriticaMvd <- lookup_users(users = MasaCriticaMvd$user_id)

# Get data from the users
MasaCriticaMvd$created
MasaCriticaMvd$description
MasaCriticaMvd$followers_count # 296
MasaCriticaMvd$friends_count # 15
MasaCriticaMvd$location

# Friends
MasaCriticaMvdFriends <- get_friends(user = MasaCriticaMvd$user_id) # who this user follows

## lookup data on MasaCriticaMvdFriends friends
MasaCriticaMvdFriendsData <- lookup_users(users = unlist(MasaCriticaMvdFriends))

# Followers
MasaCriticaMvdFollowers <- get_followers(user = MasaCriticaMvd$user_id, n = "all") # who this user follows
MasaCriticaMvdFollowersData <- lookup_users(users = unlist(MasaCriticaMvdFollowers))


# Write data to file ------------------------------------------------------

write.csv(MasaCriticaMvdFriends.df, "MasaCriticaMvdFriends.csv", sep = ",")
write.csv(laDiariaFriends.df, "laDiariaFriends.csv", sep = ",")
write.csv(MasaCriticaMvdFollowers.df, "MasaCriticaMvdFollowers.csv", sep = ",")
write.csv(laDiariaFollowers.df, "laDiariaFollowers.csv")

# a follower's followers
# followers2 <- followers[[1]]$getFollowers()

# Relations
relations <- merge(data.frame('User' = MasaCriticaMvd$MasaCriticaMvduy$screenName,
                              'Follower' = MasaCriticaMvdFriends.df$screenName),
                   data.frame('User' = MasaCriticaMvdFollowers.df$screenName,
                              'Follower' = MasaCriticaMvd$MasaCriticaMvduy$screenName), all = TRUE)

# Create graph from relations df
g <- graph.data.frame(relations[1:50,], directed = TRUE)

# Remove loops
g <- simplify(g)

# set labels and degrees of vertices
V(g)$label <- V(g)$name

g # print

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)

plot(g, layout = layout1)

# Better plot
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree) + 0.2
V(g)$label.color <- rgb(0, 0, 0.2, 0.8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight) + 0.4) / max(log(E(g)$weight) + 0.4)
E(g)$color <- rgb(0.5, 0.5, 0, egam)
E(g)$width <- egam

# plot the graph in layout1
plot(g, layout = layout1)


# Get tweets data ---------------------------------------------------------

# Get tweets (3200 is the maximum to retrieve)
tweetsMasaCriticaMvd <- userTimeline("MasaCriticaMvduy", n = 3200)
tweetsLaDiaria <- userTimeline("laDiaria", n = 3200)
tweetsElObservador <- userTimeline("ElObservador", n = 3200)

# Convert to data frame
tweetsMasaCriticaMvd.df <- twListToDF(tweetsMasaCriticaMvd)
tweetsLaDiaria.df <- twListToDF(tweetsLaDiaria)
tweetsElObservador.df <- twListToDF(tweetsElObservador)

# Check head
head(tweetsMasaCriticaMvd.df)



