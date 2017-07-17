# Global variables:
ids <- 75000 # Max number of ids per token every 15 minutes
f <- list() # Vector where user_ids from followers will be appended

# Function to get all the followers from a user with pagination
GetFollowersRecursivePagination <- function(userId, followers, page) {

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

    # Check rate limit followers/ids query
    if (!rate_limit(token = NULL)[38,]$reset > 14.9) {
      message("Waiting 15 seconds more...")
      Sys.sleep(time = 15) # wait 15 seconds more...
    }

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
    message("Writing data to files...")

    df_user_id <- data.frame('user_id' = list(ftemp)[[1]]$user_id)
    df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

    # Followers ids
    write.csv(x = df_user_id,
              file = paste("data/followers/", userId, "_followers_ids.csv", sep = ""),
              row.names = FALSE,
              append = TRUE)

    # Cursors
    write.csv(x = df_cursor,
              file = paste("data/followers/", userId, "_followers_cursors.csv", sep = ""),
              row.names = FALSE,
              append = TRUE)

    message("Finished!")
    return(rtemp)
  }

  else if (followers > ids) {

    message(paste("Followers > ids | Number of Followers: ",
                  followers, " | Number of resting ids: ",  ids, sep = ""))
    ftemp <- get_followers(user = userId, n = ids, page = page)

    f <<- append(f, list(ftemp)) # append followers ids

    # Write data to file
    message("Writing data to files...")

    df_user_id <- data.frame('user_id' = list(ftemp)[[1]]$user_id)
    df_cursor <- data.frame('next_cursor' = attr(list(ftemp)[[1]], "next_cursor"))

    # Followers ids
    write.csv(x = df_user_id,
              file = paste(getwd(), userId, "_followers_ids.csv", sep = ""),
              row.names = FALSE,
              append = TRUE)

    # Cursors
    write.csv(x = df_cursor,
              file = paste(getwd(), userId, "_followers_cursors.csv", sep = ""),
              row.names = FALSE,
              append = TRUE)

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

    # Check rate limit followers/ids query
    if (!rate_limit(token = NULL)[38,]$reset > 14.9) {
      message("Waiting 15 seconds more...")
      Sys.sleep(time = 15) # wait 15 seconds more...
    }

    message("Go!")
    ids <<- 75000

    # Recursive function call
    GetFollowersRecursivePagination(userId = userId,
                                    followers = followers - n,
                                    page = pageTemp)
  }
}
# Test ( > 75000 followers )
# Get user
user1 <- lookup_users(users = "146620155")

# Sys.sleep((as.numeric(as.character(60 * rate_limit(token = NULL)[38,]$reset))) + 10)
FAOClimate <- GetFollowersRecursivePagination(userId = user1$user_id,
                                              followers = user1$followers_count,
                                              page = '-1')
