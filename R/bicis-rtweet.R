
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

# Get followers with pagination -------------------------------------------

# Global variables:
ids <- 75000 # Max number of ids per token every 15 minutes
f <- list() # Vector where user_ids from followers will be appended

# Function to get all the followers from a user with pagination
GetFollowersRecursivePagination <- function(userId, followers, page, allFollowers, screen_name, protected) {

  # Print screenname of User
  message(paste(" | ", screen_name, " | ", sep = ""))

  if (!protected) {

    if (allFollowers > 225000) {
      warning(paste("User ", screen_name, " with more than 225000 Followers", sep = ""))
      return(NA)
    }

    if (rate_limit(token = NULL, query = "followers/ids")$remaining == 0) {

      # API Twitter Limit reached - Wait
      message("Waiting 15 mins...")
      total <- 15*60 # Total time = 15 min ~ 900 sec
      pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar

      for (i in 1:total) {
        Sys.sleep(time = 1) # 1 second interval
        setTxtProgressBar(pb, i) # update progress bar
      }
      close(pb)
      message("Go!")
      ids <<- 75000

    }

    f <- paste("data/followers/", screen_name, "-", userId, "-followers-user_id.csv", sep = "")
    offset <- 20
    fileExists <- file.exists(f)
    gotAllFollowers <- FALSE

    if (fileExists) {
      if (allFollowers > 500) {
        gotAllFollowers <- ((length(readLines(f)) - 1) >= allFollowers - offset)
        print(length(readLines(f)))
      } else {
        gotAllFollowers <- ((length(readLines(f)) - 1) >= allFollowers)
        print(length(readLines(f)))
      }
    }

    if (!gotAllFollowers) {

      if (ids == 0) {

        # API Twitter Limit reached - Wait
        message("Waiting 15 mins...")
        total <- 15*60 # Total time = 15 min ~ 900 sec
        pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar

        for (i in 1:total) {
          Sys.sleep(time = 1) # 1 second interval
          setTxtProgressBar(pb, i) # update progress bar
        }
        close(pb)
        message("Go!")
        ids <<- 75000
      }

      if (followers <= ids) {

        message(paste("Followers <= ids | Number of Followers: ",
                      followers, " | Number of resting ids: ",  ids, sep = ""))
        ftemp <- get_followers(user = userId, n = followers, page = page)

        f <<- append(f, list(ftemp)) # append followers ids

        ids <<- ids - followers
        rtemp <- f
        f <<- list()

        # Write data to file
        message("Writting data to files...")

        # Make data frames
        df_user_id <- data.frame('user_id' = list(ftemp)[[1]])
        df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

        # Open connections
        con1 <- file(f, "a")  # open an output file connection
        con2 <- file(paste("data/followers/", screen_name, "-", userId, "-followers-next_cursor.csv", sep = ""), "a")  # open an output file connection

        # Write data
        write.table(x = df_user_id, file = con1, append = TRUE, row.names = FALSE, col.names = FALSE) # user_id
        write.table(x = df_cursor, file = con2, append = TRUE, row.names = FALSE, col.names = FALSE) # next_cursor

        # Close connections
        close(con1)
        close(con2)

        message("Finished!")
        gc() # Release memory
        return(rtemp)
      }

      else if (followers > ids) {

        message(paste("Followers > ids | Number of Followers: ",
                      followers, " | Number of resting ids: ",  ids, sep = ""))
        ftemp <- get_followers(user = userId, n = ids, page = page)

        f <<- append(f, list(ftemp)) # append followers ids

        # Write data to file
        message("Writting data to files...")

        # Make data frames
        df_user_id <- data.frame('user_id' = list(ftemp)[[1]])
        df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

        # Open connections
        con1 <- file(f, "a")  # open an output file connection
        con2 <- file(paste("data/followers/", screen_name, "-", userId, "-followers-next_cursor.csv", sep = ""), "a")  # open an output file connection

        # Write data
        write.table(x = df_user_id, file = con1, append = TRUE, row.names = FALSE, col.names = FALSE) # user_id
        write.table(x = df_cursor, file = con2, append = TRUE, row.names = FALSE, col.names = FALSE) # next_cursor

        # Close connections
        close(con1)
        close(con2)

        n <- ids # n = count of followers ids already acquired

        pageTemp <- next_cursor(ftemp) # Pagination

        # API Twitter Limit reached - Wait
        message("Waiting 15 mins...")
        total <- 15*60 # Total time = 15 min ~ 900 sec
        pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar

        for (i in 1:total) {
          Sys.sleep(time = 1) # 1 second interval
          setTxtProgressBar(pb, i) # update progress bar
        }
        close(pb)
        message("Go!")
        ids <<- 75000

        # Recursive function call
        GetFollowersRecursivePagination(userId = userId,
                                        followers = followers - n,
                                        page = pageTemp,
                                        screen_name = screen_name,
                                        allFollowers = allFollowers,
                                        protected = protected)
      }
    } else {

      message("User's followers already downloaded!")
      return(NA)
    }
  } else {

    warning(paste("The user ", screen_name, " has a protected account", sep = ""))
    return(NA)
  }
}

# Test ( > 75000 followers )
# Get user
# user1 <- lookup_users(users = "146620155")
# Sys.sleep((as.numeric(as.character(60 * rate_limit(token = NULL)[38,]$reset))) + 10)
# FAOClimate <- GetFollowersRecursivePagination(userId = user1$user_id,
                                              # followers = user1$followers_count,
                                              # page = '-1',
                                              # screen_name = user1$screen_name,
                                              # allFollowers = user1$followers_count)

# Followers of followers
FollowersOfFollowers <- function(userIdx) {

  user <- lookup_users(users = userIdx) # Get user

  if (!is.na(user$user_id)) {

    f <- paste("data/users/", user$screen_name, "-", user$user_id, "-user.csv", sep = "") # file

    # Write data to file
    message("Writing User data to file...")
    # Open connections
    con <- file(f, "w")  # open an output file connection
    # Write data
    write.table(x = user, file = con, append = FALSE, row.names = FALSE, col.names = TRUE) # user
    # Close connections
    close(con)

    # Get Followers
    message("Getting followers...")
    followers <- GetFollowersRecursivePagination(userId = user$user_id,
                                                 followers = user$followers_count,
                                                 page = '-1',
                                                 screen_name = user$screen_name,
                                                 allFollowers = user$followers_count,
                                                 protected = user$protected)

    return(followers)
  }
}

# Get user MasaCriticaMvd
user <- lookup_users(users = "326757328")

MasaCriticaMvd_followers <- GetFollowersRecursivePagination(userId = user$user_id,
                                                            followers = user$followers_count,
                                                            page = '-1',
                                                            screen_name = user$screen_name,
                                                            allFollowers = user$followers_count,
                                                            protected = user$protected)

# Get followers ids of every follower of a user
MasaCriticaMvd_followers_2 <- lapply(
  X = do.call("rbind", MasaCriticaMvd_followers)$user_id,
  FUN = function(x) FollowersOfFollowers(x)
)


# Check limits
rate_limit(token = NULL, query = "followers/ids")
rate_limit(token = NULL, query = "lookup/users")


# Example
a <- lookup_users(users = "826862643415752704")
b <- GetFollowersRecursivePagination(userId = a$user_id,
                                     followers = a$followers_count,
                                     page = '-1',
                                     screen_name = a$screen_name,
                                     allFollowers = a$followers_count,
                                     protected = a$protected)






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

