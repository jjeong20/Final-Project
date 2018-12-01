## Raw Data

library(rtweet)
library(dplyr)

create_token(
  app = "StatsProjectAmherst",
  consumer_key = "xC2cjGbujenmQyv86xAzAroDp",
  consumer_secret = "Pye14GbWEjUor2mnEq6GfICuf99M7OQajfJBhK6xH1R6mjE3df",
  access_token = "3062172722-dnleJhQfJ1UUBaE2wW6BYJ43tYPcPNSYp8jaRUc",
  access_secret = "w2G2EBRNnUOxdEqRCrHeYOPTE4dFTgT0qWC2OnJVC4uap")

## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "Trump", n = 18000, include_rts = FALSE
)

head(rt, 6)

## Variables to Use

rt_var <- rt %>%
  filter(lang == 'en') %>%
  select (screen_name, text, source, favorite_count, retweet_count, 
          hashtags, media_type, bbox_coords, followers_count)

#rt_var <- rt %>%
 # filter(lang == 'en') %>%
  
#select (rt$screen_name, rt$text, rt$source, rt$favorite_count, rt$retweet_count, 
 #         rt$hashtags, rt$media_type, rt$bbox_coords, rt$followers_count) 
head(rt_var)

## Tab 1: Geographical Distribution

rt1 <- rt_var %>%
  select(bbox_coords) %>%
  mutate(long = (bbox_coords[[1]][1]+bbox_coords[[1]][2]+bbox_coords[[1]][3]+bbox_coords[[1]][4])/4)

head(rt1, 10)


## Tab 2: Top Hashtags

rt2 <- rt_var %>%
  select(hashtags) %>%
  filter(!is.na(hashtags))

hashtag_name <- vector()

for(hashtag in rt2$hashtags){
  for(tag in hashtag){
    hashtag_name = c(hashtag_name, tag)
  }
}

freq <- data.frame(hashtag_name) %>%
  group_by(hashtag_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

head(freq, 10)


## Tab 3: Top Ten Favorite/Most Retweeted Tweets

rt3 <- rt_var %>%
  select(favorite_count, retweet_count, screen_name, text)

rt3 %>%
  select(favorite_count, screen_name, text) %>%
  arrange(desc(favorite_count)) %>%
  head(10)

rt3 %>%
  select(retweet_count, screen_name, text) %>%
  arrange(desc(retweet_count)) %>%
  head(10)

## Tab 4: Top Ten Tweets by Most Followed Users

rt4 <- rt_var %>%
  select(followers_count, screen_name, text) %>%
  arrange(desc(followers_count)) %>%
  head(10)
rt4


## Tab 5: Top 6 Sources

rt5 <- rt_var %>%
  select(source, media_type)

sources_temp <- rt5 %>%
  group_by(source) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_6 <- head(sources_temp, 6)$source

sources <- rt5 %>%
  mutate(source = ifelse(source %in% top_6, source, 'Other')) %>%
  group_by(source) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
sources



