#nstall.packages("academictwitteR")

library(academictwitteR)


## Copy your bearer token
bearer_token <- ""



htagquery <- c("israel", "hamas", "jews", "jewish",
               "palestine", "palestinian", "gaza","quds","jerusalem",
               "semitism", "tel aviv", "telaviv", "sheikh jarrah", 
               "sheikhjarrah","jarrah")

query <- build_query(query = htagquery,
                     is_retweet = FALSE,
                     bbox=TRUE,
                     remove_promoted = TRUE,
                     country = "GB",
                     lang = "en")

tw <- get_all_tweets(query, bearer_token,
                     start_tweets = "2021-05-01T00:00:00Z",
                     end_tweets = "2021-06-01T00:00:00Z",
                     data_path = "./data")

#tw <- bind_tweet_jsons(data_path = "./data")