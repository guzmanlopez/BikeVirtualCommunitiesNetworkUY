
# Load libraries ----------------------------------------------------------

library('rtweet') # twitter API
# library('Rfacebook') # facebook API
library('RNeo4j') # Neo4j graph db
library('igraph') # plot graph
library('ggplot2') # plots

# Set Working directory ---------------------------------------------------

setwd("~/AnacondaProjects/BikeVirtualCommunitiesNetworkUY/R")

# OAauth ------------------------------------------------------------------

# For rtweet
# twitter_token <- create_token(
#   app = "app",
#   consumer_key = "XXXX",
#   consumer_secret = "XXXX"
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
    # file = file.path(home_directory, ".Renviron"),
    # append = TRUE)

# Get followers/friends data ----------------------------------------------

# Function to get followers and friends data from user
GetFollowersFriendsDataFromUser <- function(UserScreenName = NA, userId = NA, summary = TRUE) {

  if (!is.na(userId)) {

    user = userId
    message(paste("Getting data for user id = ", user, "...", sep = ""))
    user = lookup_users(users = user)

  } else {

    message(paste("Getting user id for ", UserScreenName, "...", sep = ""))
    # Look up for and get user id
    user = search_users(q = UserScreenName, n = 1)[1,]

  }

  if (is.na(user$user_id)) {

    message("User not found!")

  } else {

    message("User found!")

    # Path to downloaded data
    path = paste(getwd(), "/data/", sep = "")

    # Check if data alredy exists
    if (file.exists(paste(path, user$screen_name, ".csv", sep = ""))) {

      message("Data alredy exists, exiting...")

    } else {

      # Print user summary information
      if (summary) {

        cat(paste("--------------------------------------------------------- \n"))
        cat(paste("User_id: ", user$user_id, " \n"))
        cat(paste("Name: ", user$name, " \n"))
        cat(paste("Screen_name: ", user$screen_name, " \n"))
        cat(paste("Location: ", user$location, " \n"))
        cat(paste("Created_at: ", user$created_at, " \n"))
        cat(paste("Followers_count: ", user$followers_count, " \n"))
        cat(paste("Friends_count: ", user$friends_count, " \n"))
        cat(paste("Favourites_count: ", user$favourites_count, " \n"))
        cat(paste("Description: ", user$description, " \n"))
        cat(paste("--------------------------------------------------------- \n"))
        cat(" \n")

      }

      # User Data + Friends Data + Followers Data
      userFriendsFollowersData <- user
      userFriendsFollowersData$user_reference <- NA
      userFriendsFollowersData$user_relation <- NA

      # Get user friends
      message("Getting user's friends...")
      userFriendsIds <- get_friends(user = user$user_id) # who this user follows
      userFriendsData <- lookup_users(users = unlist(userFriendsIds))

      # Add references to friends data
      userFriendsData$user_reference <- rep(user$screen_name, nrow(userFriendsData))
      userFriendsData$user_relation <- rep("friend", nrow(userFriendsData))
      message("Finished!")

      # get user followers
      message("Getting user's followers...")
      userFollowersIds <- get_followers(user = user$user_id, n = "all") # who this user follows
      userFollowersData <- lookup_users(users = unlist(userFollowersIds))

      # Add references to followers data
      userFollowersData$user_reference <- rep(user$screen_name, nrow(userFollowersData))
      userFollowersData$user_relation <- rep("follower", nrow(userFollowersData))
      message("Finished!")

      # Bind followers and friends
      message("Binding data...")
      userFriendsFollowers <- rbind(userFriendsData, userFollowersData)

      # Bind followers and friends to user
      userFriendsFollowersData <- rbind(userFriendsFollowersData, userFriendsFollowers)
      message("Finished!")

      # Write data to file
      message("Writing data to file...")
      write.csv(userFriendsFollowersData, paste("data/", user$screen_name, ".csv", sep = ""),
                row.names = FALSE)

      # return
      return(userFriendsFollowersData)

    }

  }

}

# Example
MasaCriticaMvd <- GetFollowersFriendsDataFromUser(UserScreenName = "MasaCriticaMvd") # Using screenname
MasaCriticaMvd <- GetFollowersFriendsDataFromUser(userId = '326757328') # Using id

# Get followers and friend of every follower and friend of MasaCriticaMvd
MasaCriticaMvd.user_id <- MasaCriticaMvd$user_id[-1] # first row is the head user

listFollowersFriendsData.MasaCriticaMvd <-
  lapply(
    X = 1:length(MasaCriticaMvd.user_id),
    FUN = function(x) {
      GetFollowersFriendsDataFromUser(userId = MasaCriticaMvd.user_id[x])
      print(paste(x, "/", length(MasaCriticaMvd.user_id), sep = ""))
    }
  )


# Check followers and friends number for each file and return errors
CheckFollowersAndFriendsNumber <- function(UserScreenName = NA, userId = NA, summary = TRUE) {

}

# Example
f1 <- get_followers("potus", n = 75000)
page <- next_cursor(f1)
Sys.sleep(15*60)
f2 <- get_followers("potus", n = 75000, page = page)



# Global variables:
ids <- 75000 # Max number of ids per token every 15 minutes
f <- list() # List where followers will be appended
page <- -1

# Function to get all the followers from a user with pagination
GetFollowersRecursivePagination <- function(userId, followers, page) {

  if (followers < ids) {

    message(paste("Followers < ids ", followers, sep = ""))
    ftemp <- get_followers(user = userId, n = followers, page = page)
    f <<- append(f, list(ftemp))
    ids <<- ids - followers
    message("Finish!")
    return(f)

  }

  else if (followers == ids) {

    message(paste("Followers == ids ", followers, sep = ""))
    ftemp <- get_followers(user = userId, n = ids, page = page)
    f <<- append(f, list(ftemp))
    message("Waiting 15 mins...")
    Sys.sleep(time = 15*60)
    message("Go!")
    ids <<- 75000
    return(f)

  }

  else if (followers > ids) {

    message(paste("Followers > ids ", followers, sep = ""))
    ftemp <- get_followers(user = userId, n = ids, page = page)
    ids <<- 0
    f <<- append(f, list(ftemp))
    page <<- next_cursor(ftemp)
    message("Waiting 15 mins...")
    Sys.sleep(time = 15*60)
    message("Go!")
    ids <<- 75000

    # Recursive
    f <<- append(f, list(GetFollowersRecursivePagination(userId = userId, followers = followers - 75000, page = page)))

  }
}

# Test
# Get user
user <- lookup_users(users = "146620155")

FAOClimate <- GetFollowersRecursivePagination(userId = user$user_id, followers = user$followers_count, page = -1)
fabveggievegan <- GetFollowersRecursivePagination(userId = "3240204569", followers = 32285, page = -1)


nrow(FAOClimate[[1]]) + length(FAOClimate[[2]])

nrow(fabveggievegan[[1]]) + class(fabveggievegan[[2]])

str(FAOClimate)

length(FAOClimate[[1]]$user_id) + length(FAOClimate[[2]])



head(FAOClimate[[1]]$user_id)










# Look up for and get users
MasaCriticaMvd <- search_users(q = "MasaCriticaMvd", n = 1)
MasaCriticaMvd <- lookup_users(users = MasaCriticaMvd$user_id)

# Summary for user
print(MasaCriticaMvd[,c(1,2,3,4,10,6)])
print(MasaCriticaMvd[,c(7,8,11)])
print(MasaCriticaMvd[,c(5)])

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

MasaCriticaMvdFriendsData$user_reference <- rep(MasaCriticaMvd$screen_name, nrow(MasaCriticaMvdFriendsData))
MasaCriticaMvdFriendsData$user_relation <- rep("friend", nrow(MasaCriticaMvdFriendsData))

# Followers
MasaCriticaMvdFollowers <- get_followers(user = MasaCriticaMvd$user_id, n = "all") # who this user follows
MasaCriticaMvdFollowersData <- lookup_users(users = unlist(MasaCriticaMvdFollowers))

MasaCriticaMvdFollowersData$user_reference <- rep(MasaCriticaMvd$screen_name, nrow(MasaCriticaMvdFollowersData))
MasaCriticaMvdFollowersData$user_relation <- rep("follower", nrow(MasaCriticaMvdFollowersData))

# Bind followers and friends
MasaCriticaMvdFriendsFollowers <- rbind(MasaCriticaMvdFriendsData, MasaCriticaMvdFollowersData)

rbind(MasaCriticaMvd, MasaCriticaMvdFriendsFollowers)

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

# Convert to data frame
tweetsMasaCriticaMvd.df <- twListToDF(tweetsMasaCriticaMvd)

# Check head
head(tweetsMasaCriticaMvd.df)





# Neo4j graph database ----------------------------------------------------

# read user and pass from external R script
source(file = "neo4j-auth.R")

# Start Neo4j service first from command line (sudo systemctl start neo4j.service)
graph = startGraph("http://localhost:7474/db/data/",
                   username = neo4jUser, password = neo4jPass)
clear(graph) # Remove all

# Add constraints to db
addConstraint(graph, label = "Tweet", key = "id")
addConstraint(graph, label = "User", key = "screen_name")
addConstraint(graph, label = "Hashtag", key = "name")
addConstraint(graph, label = "Link", key = "url")
addConstraint(graph, label = "Source", key = "name")

# Create graph DB
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

