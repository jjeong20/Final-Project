## Raw Data

library(rtweet)
library(dplyr)
library(ggplot2)
library(sp)
library(maps)
library(maptools)

create_token(
  app = "StatsProjectAmherst",
  consumer_key = "xC2cjGbujenmQyv86xAzAroDp",
  consumer_secret = "Pye14GbWEjUor2mnEq6GfICuf99M7OQajfJBhK6xH1R6mjE3df",
  access_token = "3062172722-dnleJhQfJ1UUBaE2wW6BYJ43tYPcPNSYp8jaRUc",
  access_secret = "w2G2EBRNnUOxdEqRCrHeYOPTE4dFTgT0qWC2OnJVC4uap")

## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "MLB", n = 18000,geocode = lookup_coords('usa'), include_rts = FALSE
)

create_token(
  app = "ergqwb12324r3",
  consumer_key = "jnw2qnJPESXlGsrdn2labCW4L",
  consumer_secret = "kKrWvONmIb8muABNgWFFG7A6d6HhtlZNodiXlzb1tRmdClGWsR",
  access_token = "632015603-ASMkADJhXvo7KcLt27Cgt7MIig2IfYtOc75xDBRu",
  access_secret = "BzXbPR6wnzjO9OrdZJskn9U21952gjVcbVMNqcltpZLE4")

rt <- search_tweets(
  input$keyword2, n = 9000, include_rts = FALSE, geocode = lookup_coords('usa')
)

head(rt, 6)

## Variables to Use

rt_var <- rt %>%
  filter(lang == 'en') %>%
  select (screen_name, text, source, favorite_count, retweet_count, 
          hashtags, media_type, bbox_coords, followers_count)
head(rt_var)


## Tab 1: Geographical Distribution
# create a new column that counts the number of missing coordinates
n_coords_na <- sapply(rt_var$bbox_coords, FUN=function(x) sum(is.na(x)))
rt_var$n_missing_coords <- n_coords_na

rt1 <- rt_var %>%
  filter(n_missing_coords == 0) %>%
  select(bbox_coords)
  
long <- vector()
lat <- vector()
for(i in 1:length(rt1$bbox_coords)){
  if(is.numeric(rt1[i,][[1]][1])){
    long = c(long, (rt1[i,][[1]][1]+rt1[i,][[1]][2]+rt1[i,][[1]][3]+rt1[i,][[1]][4])/4)
    lat = c(lat, (rt1[i,][[1]][5]+rt1[i,][[1]][6]+rt1[i,][[1]][7]+rt1[i,][[1]][8])/4)
  }
  else{
    long = c(long, (rt1[i,][[1]][[1]][1]+rt1[i,][[1]][[1]][2]+rt1[i,][[1]][[1]][3]+rt1[i,][[1]][[1]][4])/4)
    lat = c(lat, (rt1[i,][[1]][[1]][5]+rt1[i,][[1]][[1]][6]+rt1[i,][[1]][[1]][7]+rt1[i,][[1]][[1]][8])/4)    
  }
}

rt1$lat <- lat
rt1$long <- long

ggplot(rt1, aes(long, lat)) + borders('state', colour = 'gray50', fill='gray') + geom_point(color = 'red', size = 0.8) 

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

# Tab 6

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
rt_coords <- rt1[,2:3]
rt_coords[,1] <-rt1[,3] 
rt_coords[,2] <- rt1[,2]
names(rt_coords) <- c('long', 'lat')
states <- latlong2state(rt_coords)
rt1$states <- states


rt_state<-rt1 %>%
  filter(!is.na(states))%>%
  group_by(states)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(10)

ggplot(rt_state,aes(reorder(states, -count, sum), count))+
  xlab("Top 10 States")+
  theme(axis.text.x = element_text(angle=60, hjust=1))+
  geom_col()




