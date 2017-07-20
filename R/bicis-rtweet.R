
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
times <- 15 # Max times get_followers can be called
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

    if (times == 0) {

      # rate_limit(token = NULL, query = "followers/ids")$remaining == 0

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
      times <<- 15
    }

    file <- paste("data/followers/", screen_name, "-", userId, "-followers-user_id.csv", sep = "")
    fileExists <- file.exists(file)
    gotAllFollowers <- FALSE
    thresholdDifference <- allFollowers * 0.03 # 3% tolerance

    if (fileExists) {
      gotAllFollowers <- ((length(readLines(file)) - 1) >= (allFollowers - thresholdDifference))
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
        times <<- 15
      }

      if (followers <= ids) {

        message(paste("Followers <= ids | Number of Followers: ",
                      followers, " | Resting ids: ",  ids, " | Resting times: ", times, sep = ""))
        ftemp <- get_followers(user = userId, n = followers, page = page)

        f <<- append(f, list(ftemp)) # append followers ids

        ids <<- ids - followers
        times <<- times - 1
        rtemp <- f
        f <<- list()

        # Write data to file
        message("Writting data to files...")

        # Make data frames
        df_user_id <- data.frame('user_id' = list(ftemp)[[1]])
        df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

        # Open connections
        con1 <- file(file, "a")  # open an output file connection
        con2 <- file(paste("data/followers/cursors/", screen_name, "-", userId, "-followers-next_cursor.csv", sep = ""), "a")  # open an output file connection

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

        message(paste("Followers <= ids | Number of Followers: ",
                      followers, " | Resting ids: ",  ids, " | Resting times: ", times, sep = ""))
        ftemp <- get_followers(user = userId, n = ids, page = page)
        times <<- times - 1
        f <<- append(f, list(ftemp)) # append followers ids

        # Write data to file
        message("Writting data to files...")

        # Make data frames
        df_user_id <- data.frame('user_id' = list(ftemp)[[1]])
        df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

        # Open connections
        con1 <- file(file, "a")  # open an output file connection
        con2 <- file(paste("data/followers/cursors/", screen_name, "-", userId, "-followers-next_cursor.csv", sep = ""), "a")  # open an output file connection

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
        times <<- 15

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
      return(list('user_id' = read.csv(file)))
    }
  } else {

    warning(paste("The user ", screen_name, " has a protected account", sep = ""))
    return(NA)
  }
}

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
    write.table(x = user, file = con, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",") # user
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
user1 <- lookup_users(users = "MasaCriticaMvd")
MasaCriticaMvd_followers <- GetFollowersRecursivePagination(userId = user1$user_id,
                                                            followers = user1$followers_count,
                                                            page = '-1',
                                                            screen_name = user1$screen_name,
                                                            allFollowers = user1$followers_count,
                                                            protected = user1$protected)

# Get followers ids of every follower of a user
MasaCriticaMvd_followers_2 <- lapply(
  X = unique(as.character(unlist(MasaCriticaMvd_followers))),
  FUN = function(x) FollowersOfFollowers(x)
)

# Get user MibielaMVD
user2 <- lookup_users(users = "MibielaMVD")
MibielaMVD_followers <- GetFollowersRecursivePagination(userId = user2$user_id,
                                                        followers = user2$followers_count,
                                                        page = '-1',
                                                        screen_name = user2$screen_name,
                                                        allFollowers = user2$followers_count,
                                                        protected = user2$protected)

# Get followers ids of every follower of a user
MibielaMVD_followers_2 <- lapply(
  X = unique(as.character(unlist(MibielaMVD_followers))),
  FUN = function(x) FollowersOfFollowers(x)
)

# Get user BicitourM
user3 <- lookup_users(users = "BicitourM")
BicitourM_followers <- GetFollowersRecursivePagination(userId = user3$user_id,
                                                       followers = user3$followers_count,
                                                       page = '-1',
                                                       screen_name = user3$screen_name,
                                                       allFollowers = user3$followers_count,
                                                       protected = user3$protected)

# Get followers ids of every follower of a user
BicitourM_followers_2 <- lapply(
  X = unique(as.character(unlist(BicitourM_followers$user_id))),
  FUN = function(x) FollowersOfFollowers(x)
)

# Get user BikeHouseUY
user4 <- lookup_users(users = "BikeHouseUY")
BikeHouseUY_followers <- GetFollowersRecursivePagination(userId = user4$user_id,
                                                         followers = user4$followers_count,
                                                         page = '-1',
                                                         screen_name = user4$screen_name,
                                                         allFollowers = user4$followers_count,
                                                         protected = user4$protected)

# Get followers ids of every follower of a user
BikeHouseUY_followers_2 <- lapply(
  X = unique(as.character(BikeHouseUY_followers$user_id[,1])),
  FUN = function(x) FollowersOfFollowers(x)
)

# Get user FIMBici
user5 <- lookup_users(users = "FIMBici")
FIMBici_followers <- GetFollowersRecursivePagination(userId = user5$user_id,
                                                     followers = user5$followers_count,
                                                     page = '-1',
                                                     screen_name = user5$screen_name,
                                                     allFollowers = user5$followers_count,
                                                     protected = user5$protected)

# Get followers ids of every follower of a user
FIMBici_followers_2 <- lapply(
  X = unique(as.character(unlist(FIMBici_followers$user_id))),
  FUN = function(x) FollowersOfFollowers(x)
)

# Get user ciclovidaonline
user6 <- lookup_users(users = "ciclovidaonline")
ciclovidaonline_followers <- GetFollowersRecursivePagination(userId = user6$user_id,
                                                             followers = user6$followers_count,
                                                             page = '-1',
                                                             screen_name = user6$screen_name,
                                                             allFollowers = user6$followers_count,
                                                             protected = user6$protected)

# Get followers ids of every follower of a user
ciclovidaonline_followers_2 <- lapply(
  X = unique(as.character(unlist(ciclovidaonline_followers))),
  FUN = function(x) FollowersOfFollowers(x)
)


# Check limits
rate_limit(token = NULL, query = "followers/ids")


# Check followers ---------------------------------------------------------

# Check total followers and remove duplicates
CheckAndRemoveFollowersDuplicates <- function(user) {

  message(paste("| Checking and removing duplicates for ", user, " |", sep = ""))

  # Get user and followers files path from user screen name
  userFile <- list.files(path = "data/users/", pattern = user)
  followersFile <- list.files(path = "data/followers/", pattern = user)

  if (length(userFile) > 1 | length(followersFile) > 1) {

    warning("More than one file matched!")
    warning("Try use user_id instead of user_screen_name.")
    return("-1")

  } else {

    if (length(userFile) != 0 & length(followersFile) != 0) {

      # Get user data
      user_data <- read.csv(paste("data/users/", userFile, sep = ""))

      # Get followers data
      followers_data <- read.csv(paste("data/followers/", followersFile, sep = ""), header = FALSE, colClasses = "character")

      # Check followers duplicates
      duplicatedCells <- which(duplicated(followers_data))

      if (length(duplicatedCells) != 0) {

        message("Duplicated followers found... Yes!")
        message("Removing duplicates...")
        followers_data <- followers_data[-duplicatedCells,]
        message("Done!")
        message("Writing new file...")
        # Open connections
        con1 <- file(paste("data/followers/", followersFile, sep = ""), "w")  # open an output file connection
        # Write data
        write.table(x = followers_data, file = con1, append = FALSE, row.names = FALSE, col.names = FALSE) # user_id
        # Close connections
        close(con1)
        # Get followers data
        followers_data <- read.csv(paste("data/followers/", followersFile, sep = ""), header = FALSE, colClasses = "character")
        message("Done!")
      } else {
        message("Duplicated followers found... No!")
      }

      # Check followers count
      countFollowersInFile <- length(followers_data[,1])
      followersDifference <- abs(user_data$followers_count - countFollowersInFile)
      differenceThreshold <- user_data$followers_count * 0.03 # 3% followers count difference tolerance

      if (followersDifference > differenceThreshold) {

        message("Less count of followers in file found... Yes!")
        message("Getting followers again...")
        GetFollowersRecursivePagination(userId = user_data$user_id,
                                        followers = user_data$followers_count,
                                        page = '-1',
                                        screen_name = user_data$screen_name,
                                        allFollowers = user_data$followers_count,
                                        protected = user_data$protected)
        message("Done!")

      } else {
        message("Less count of followers in file found... No!")
      }

    } else {
      warning("User file or followers file don't exist!")
      return("-1")
    }
    message('Finished OK.')
    return('OK')
  }
}

u1 <- CheckAndRemoveFollowersDuplicates(user = "MasaCriticaMvd") # OK
u2 <- CheckAndRemoveFollowersDuplicates(user = "MibielaMVD") # OK
u3 <- CheckAndRemoveFollowersDuplicates(user = "BicitourM") # OK
u4 <- CheckAndRemoveFollowersDuplicates(user = "BikeHouseUY") # OK
u5 <- CheckAndRemoveFollowersDuplicates(user = "FIMBici") # OK
u6 <- CheckAndRemoveFollowersDuplicates(user = "ciclovidaonline") # OK



# Get friends with pagination -------------------------------------------

# Global variables:
ids <- 75000 # Max number of ids per token every 15 minutes
times <- 15 # Max times get_friends can be called
f <- list() # Vector where user_ids from friends will be appended

# Function to get all the friends from a user with pagination
GetFriendsRecursivePagination <- function(userId, friends, page, allfriends, screen_name, protected) {

  # Print screenname of User
  message(paste(" | ", screen_name, " | ", sep = ""))

  if (!protected) {

    if (allfriends > 225000) {
      warning(paste("User ", screen_name, " with more than 225000 friends", sep = ""))
      return(NA)
    }

    if (times == 0) {

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
      times <<- 15
    }

    file <- paste("data/friends/", screen_name, "-", userId, "-friends-user_id.csv", sep = "")
    fileExists <- file.exists(file)
    gotAllfriends <- FALSE
    thresholdDifference <- allfriends * 0.03 # 3% tolerance

    if (fileExists) {
      gotAllfriends <- ((length(readLines(file)) - 1) >= (allfriends - thresholdDifference))
    }

    if (!gotAllfriends) {

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
        times <<- 15
      }

      if (friends <= ids) {

        message(paste("friends <= ids | Number of friends: ",
                      friends, " | Resting ids: ",  ids, " | Resting times: ", times, sep = ""))
        ftemp <- get_friends(user = userId, page = page)

        f <<- append(f, list(ftemp)) # append friends ids

        ids <<- ids - friends
        times <<- times - 1
        rtemp <- f
        f <<- list()

        # Write data to file
        message("Writting data to files...")

        # Make data frames
        df_user_id <- data.frame('user_id' = list(ftemp)[[1]])
        df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

        # Open connections
        con1 <- file(file, "a")  # open an output file connection
        con2 <- file(paste("data/friends/cursors/", screen_name, "-", userId, "-friends-next_cursor.csv", sep = ""), "a")  # open an output file connection

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

      else if (friends > ids) {

        message(paste("friends <= ids | Number of friends: ",
                      friends, " | Resting ids: ",  ids, " | Resting times: ", times, sep = ""))
        ftemp <- get_friends(user = userId, page = page)
        times <<- times - 1
        f <<- append(f, list(ftemp)) # append friends ids

        # Write data to file
        message("Writting data to files...")

        # Make data frames
        df_user_id <- data.frame('user_id' = list(ftemp)[[1]])
        df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

        # Open connections
        con1 <- file(file, "a")  # open an output file connection
        con2 <- file(paste("data/friends/cursors/", screen_name, "-", userId, "-friends-next_cursor.csv", sep = ""), "a")  # open an output file connection

        # Write data
        write.table(x = df_user_id, file = con1, append = TRUE, row.names = FALSE, col.names = FALSE) # user_id
        write.table(x = df_cursor, file = con2, append = TRUE, row.names = FALSE, col.names = FALSE) # next_cursor

        # Close connections
        close(con1)
        close(con2)

        n <- ids # n = count of friends ids already acquired

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
        times <<- 15

        # Recursive function call
        GetFriendsRecursivePagination(userId = userId,
                                      friends = friends - n,
                                      page = pageTemp,
                                      screen_name = screen_name,
                                      allfriends = allfriends,
                                      protected = protected)
      }
    } else {

      message("User's friends already downloaded!")
      return(list('user_id' = read.csv(file)))
    }
  } else {

    warning(paste("The user ", screen_name, " has a protected account", sep = ""))
    return(NA)
  }
}

# Friends of friends
FriendsOfFriends <- function(userIdx) {

  user <- lookup_users(users = userIdx) # Get user

  if (!is.na(user$user_id)) {

    f <- paste("data/users/", user$screen_name, "-", user$user_id, "-user.csv", sep = "") # file

    # Write data to file
    message("Writing User data to file...")
    # Open connections
    con <- file(f, "w")  # open an output file connection
    # Write data
    write.table(x = user, file = con, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",") # user
    # Close connections
    close(con)

    # Get friends
    message("Getting friends...")
    friends <- GetFriendsRecursivePagination(userId = user$user_id,
                                             friends = user$friends_count,
                                             page = '-1',
                                             screen_name = user$screen_name,
                                             allfriends = user$friends_count,
                                             protected = user$protected)

    return(friends)
  }
}

# Get user MasaCriticaMvd
user1 <- lookup_users(users = "MasaCriticaMvd")
MasaCriticaMvd_friends <- GetFriendsRecursivePagination(userId = user1$user_id,
                                                        friends = user1$friends_count,
                                                        page = '-1',
                                                        screen_name = user1$screen_name,
                                                        allfriends = user1$friends_count,
                                                        protected = user1$protected)

# Get friends ids of every friend of a user
MasaCriticaMvd_friends_2 <- lapply(
  X = unique(as.character(unlist(MasaCriticaMvd_friends))),
  FUN = function(x) FriendsOfFriends(x)
)

# Get user MibielaMVD
user2 <- lookup_users(users = "MibielaMVD")
MibielaMVD_friends <- GetFriendsRecursivePagination(userId = user2$user_id,
                                                    friends = user2$friends_count,
                                                    page = '-1',
                                                    screen_name = user2$screen_name,
                                                    allfriends = user2$friends_count,
                                                    protected = user2$protected)

# Get friends ids of every follower of a user
MibielaMVD_friends_2 <- lapply(
  X = unique(as.character(unlist(MibielaMVD_friends))),
  FUN = function(x) FriendsOfFriends(x)
)

# Get user BicitourM
user3 <- lookup_users(users = "BicitourM")
BicitourM_friends <- GetFriendsRecursivePagination(userId = user3$user_id,
                                                   friends = user3$friends_count,
                                                   page = '-1',
                                                   screen_name = user3$screen_name,
                                                   allfriends = user3$friends_count,
                                                   protected = user3$protected)

# Get friends ids of every follower of a user
BicitourM_friends_2 <- lapply(
  X = unique(as.character(unlist(BicitourM_friends$user_id))),
  FUN = function(x) FriendsOfFriends(x)
)

# Get user BikeHouseUY
user4 <- lookup_users(users = "BikeHouseUY")
BikeHouseUY_friends <- GetFriendsRecursivePagination(userId = user4$user_id,
                                                     friends = user4$friends_count,
                                                     page = '-1',
                                                     screen_name = user4$screen_name,
                                                     allfriends = user4$friends_count,
                                                     protected = user4$protected)

# Get friends ids of every follower of a user
BikeHouseUY_friends_2 <- lapply(
  X = unique(as.character(BikeHouseUY_friends$user_id[,1])),
  FUN = function(x) FriendsOfFriends(x)
)

# Get user FIMBici
user5 <- lookup_users(users = "FIMBici")
FIMBici_friends <- GetFriendsRecursivePagination(userId = user5$user_id,
                                                 friends = user5$friends_count,
                                                 page = '-1',
                                                 screen_name = user5$screen_name,
                                                 allfriends = user5$friends_count,
                                                 protected = user5$protected)

# Get friends ids of every follower of a user
FIMBici_friends_2 <- lapply(
  X = unique(as.character(unlist(FIMBici_friends$user_id))),
  FUN = function(x) FriendsOfFriends(x)
)

# Get user ciclovidaonline
user6 <- lookup_users(users = "ciclovidaonline")
ciclovidaonline_friends <- GetFriendsRecursivePagination(userId = user6$user_id,
                                                         friends = user6$friends_count,
                                                         page = '-1',
                                                         screen_name = user6$screen_name,
                                                         allfriends = user6$friends_count,
                                                         protected = user6$protected)

# Get friends ids of every follower of a user
ciclovidaonline_friends_2 <- lapply(
  X = unique(as.character(unlist(ciclovidaonline_friends))),
  FUN = function(x) FriendsOfFriends(x)
)

# Check limits
rate_limit(token = NULL, query = "friends/ids")

# Check friends ---------------------------------------------------------

# Check total friends and remove duplicates
CheckAndRemoveFriendsDuplicates <- function(user) {

  message(paste("| Checking and removing duplicates for ", user, " |", sep = ""))

  # Get user and friends files path from user screen name
  userFile <- list.files(path = "data/users/", pattern = user)
  friendsFile <- list.files(path = "data/friends/", pattern = user)

  if (length(userFile) > 1 | length(friendsFile) > 1) {

    warning("More than one file matched!")
    warning("Try use user_id instead of user_screen_name.")
    return("-1")

  } else {

    if (length(userFile) != 0 & length(friendsFile) != 0) {

      # Get user data
      user_data <- read.csv(paste("data/users/", userFile, sep = ""))

      # Get friends data
      friends_data <- read.csv(paste("data/friends/", friendsFile, sep = ""), header = FALSE, colClasses = "character")

      # Check friends duplicates
      duplicatedCells <- which(duplicated(friends_data))

      if (length(duplicatedCells) != 0) {

        message("Duplicated friends found... Yes!")
        message("Removing duplicates...")
        friends_data <- friends_data[-duplicatedCells,]
        message("Done!")
        message("Writing new file...")
        # Open connections
        con1 <- file(paste("data/friends/", friendsFile, sep = ""), "w")  # open an output file connection
        # Write data
        write.table(x = friends_data, file = con1, append = FALSE, row.names = FALSE, col.names = FALSE) # user_id
        # Close connections
        close(con1)
        # Get friends data
        friends_data <- read.csv(paste("data/friends/", friendsFile, sep = ""), header = FALSE, colClasses = "character")
        message("Done!")
      } else {
        message("Duplicated friends found... No!")
      }

      # Check friends count
      countfriendsInFile <- length(friends_data[,1])
      friendsDifference <- abs(user_data$friends_count - countfriendsInFile)
      differenceThreshold <- user_data$friends_count * 0.03 # 3% friends count difference tolerance

      if (friendsDifference > differenceThreshold) {

        message("Less count of friends in file found... Yes!")
        message("Getting friends again...")
        GetFriendsRecursivePagination(userId = user_data$user_id,
                                        friends = user_data$friends_count,
                                        page = '-1',
                                        screen_name = user_data$screen_name,
                                        allfriends = user_data$friends_count,
                                        protected = user_data$protected)
        message("Done!")

      } else {
        message("Less count of friends in file found... No!")
      }

    } else {
      warning("User file or friends file don't exist!")
      return("-1")
    }
    message('Finished OK.')
    return('OK')
  }
}

u1 <- CheckAndRemoveFriendsDuplicates(user = "MasaCriticaMvd") # OK
u2 <- CheckAndRemoveFriendsDuplicates(user = "MibielaMVD") # OK
u3 <- CheckAndRemoveFriendsDuplicates(user = "BicitourM") # OK
u4 <- CheckAndRemoveFriendsDuplicates(user = "BikeHouseUY") # OK
u5 <- CheckAndRemoveFriendsDuplicates(user = "FIMBici") # OK
u6 <- CheckAndRemoveFriendsDuplicates(user = "ciclovidaonline") # OK











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

