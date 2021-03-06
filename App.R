library(shiny)
library(dplyr)
library(rtweet)
library(DT)
library(ggplot2)
library(maps)
library(maptools)

# Define UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Twitter Keyword Analysis"),
  

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      "To search for keywords, type in two keywords",
      "then check 'Search Tweets' box to search",
      checkboxInput("search_tweets", "Search Tweets:",
                    FALSE),
      textInput('keyword1', "First Keyword to Search:", ""),
      textInput("keyword2", "Second Keyword to Search:", ""),
      selectInput('default1', 'Pre-downloaded Keyword 1:', c('Democrat', 'Republican','Apple', 'Samsung', 'MLB','NFL','DCComics', 'Marvel'), selected='Democrat'),
      selectInput('default2', 'Pre-downloaded Keyword 2:', c('Democrat', 'Republican', 'Apple', 'Samsung','MLB', 'NFL','DCComics','Marvel'),selected='Republican')
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel('Maps',
                    fluidRow(
                      column(width = 6,plotOutput("plot1")),
                      column(width = 6, plotOutput("plot2"))
                    )
                  ),
                  tabPanel('Compare By State', plotOutput('comparison', height='600px'), 
                           fluidRow(
                             column(width = 6,plotOutput("comparison2")),
                             column(width = 6, plotOutput("comparison3"))
                           )
                           ),
                  tabPanel('Top Hashtags',
                    fluidRow(
                      column(width = 5,h2('Top Hashtags: Keyword 1'), dataTableOutput("hashtags1")),
                      column(width = 5, h2('Top Hashtags: Keyword 2'),dataTableOutput("hashtags2"))
                    )  
                  ),
                  tabPanel('Favorite Tweets',
                    fluidRow(
                      column(width = 5,h2('Top Favorited Tweets: Keyword 1'), dataTableOutput("favorite1")),
                      column(width = 5, h2('Top Favorited Tweets: Keyword 2'),dataTableOutput("favorite2"))                    
                    )
                  ),
                  tabPanel('Retweeted Tweets',
                    fluidRow(
                      column(width = 5,h2('Top Retweeted Tweets: Keyword 1'), dataTableOutput("retweeted1")),
                      column(width = 5, h2('Top Retweeted Tweets: Keyword 2'),dataTableOutput("retweeted2"))                     
                    )
                  ),
                  tabPanel('Tweets by Most Followed Users',
                    fluidRow(
                      column(width = 5,h2('Tweets by Most Followed Users: Keyword 1'), dataTableOutput("followed1")),
                      column(width = 5, h2('Tweets by Most Followed Users: Keyword 2'),dataTableOutput("followed2"))                       
                    )
                  )
      )
      
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  

  
  ## search for 18000 tweets using the rstats hashtag
  rt_var1 <- reactive({
    if(input$search_tweets){
      create_token(
        # Twitter API Credentials
        app = "StatsProjectAmherst",
        consumer_key = "xC2cjGbujenmQyv86xAzAroDp",
        consumer_secret = "Pye14GbWEjUor2mnEq6GfICuf99M7OQajfJBhK6xH1R6mjE3df",
        access_token = "3062172722-dnleJhQfJ1UUBaE2wW6BYJ43tYPcPNSYp8jaRUc",
        access_secret = "w2G2EBRNnUOxdEqRCrHeYOPTE4dFTgT0qWC2OnJVC4uap")
      
      rt <- search_tweets(
        input$keyword1, n = 9000, include_rts = FALSE, geocode = lookup_coords('usa')
      )
    }
    else{
      #loading a pre-downloaded dataset
      filename <- paste(input$default1, "rds", sep=".")
      rt <- readRDS(filename)
    }
    # select only the variables of interest
    rt <- rt %>%
      filter(lang == 'en') %>%
      select (screen_name, text, source, favorite_count, retweet_count, 
              hashtags, media_type, bbox_coords, followers_count)
    return(rt)
  })

  
  # Create reactive data frame for top hashtags tab
  hashtag1 <- reactive({
    
    rt2 <- rt_var1() %>%
      select(hashtags) %>%
      filter(!is.na(hashtags))
    
    hashtag_name <- vector()
    
    # since entries in hashtag column are vectors and not strings,
    # I created a new vector that appends all the hashtag vectors 
    # into one-dimensional vector
    for(hashtag in rt2$hashtags){
      for(tag in hashtag){
        hashtag_name = c(hashtag_name, tag)
      }
    }
    
    # get each hashtag's frequencies
    freq <- data.frame(hashtag_name) %>%
      group_by(hashtag_name) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    return(head(freq, 10))
    
  })
  
  # Create reactive data frame for top favorited tweets tab
  top_favorite1 <- reactive({
    rt3 <- rt_var1()%>%
      select(favorite_count, retweet_count, screen_name, text)
    
    rt3 <- rt3 %>%
      select(favorite_count, screen_name, text) %>%
      arrange(desc(favorite_count)) %>%
      select(screen_name, text) %>%
      head(10)
    
    return(rt3)

  })
  
  # Create reactive data frame for top retweeted tweets tab
  top_retweet1 <- reactive({
    rt3 <- rt_var1()%>%
      select(favorite_count, retweet_count, screen_name, text)
    
    rt3 <- rt3 %>%
      select(retweet_count, screen_name, text) %>%
      arrange(desc(retweet_count)) %>%
      select(screen_name, text) %>%
      head(10)
    
    return(rt3)
    
  })
  
  # create reactive data frame for tweets by most followed users tab 
  top_followed1 <- reactive({
     rt4 <- rt_var1()  %>%
       select(followers_count, screen_name, text) %>%
       arrange(desc(followers_count)) %>%
       select(screen_name, text) %>%
       head(10)
     return(rt4)
    
  })
  
  # Create reactive data frame for geographical distribution analysis
  rt1 <- reactive({
    rt_var <- rt_var1()
    
    # filter out missing bbox coords in form of (NA, NA, ..., NA)
    n_coords_na <- sapply(rt_var$bbox_coords, FUN=function(x) sum(is.na(x)))
    rt_var$n_missing_coords <- n_coords_na
    rt1 <- rt_var%>%
      filter(n_missing_coords == 0) %>%
      select(bbox_coords)
    
    # average bounding box coordinates to estimate latitude and longitude
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
    
    # add as columns in data frame
    rt1$lat <- lat
    rt1$long <- long
    return(rt1)
  })
  
  # in the data frame for geographical distributions, 
  # identify what state each tweet is from 
  states1 <- reactive({
    rt1 <- rt1()
    
    # function: given a data frame of latitudes and longitudes
    # identify each location's state and return the result
    latlong2state <- function(pointsDF) {
      states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
      IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
      states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
      pointsSP <- SpatialPoints(pointsDF,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
      indices <- over(pointsSP, states_sp)
      stateNames <- sapply(states_sp@polygons, function(x) x@ID)
      stateNames[indices]
    }
    
    # format rt_coords to use latlong2state function
    rt_coords <- rt1[,2:3]
    rt_coords[,1] <-rt1[,3] 
    rt_coords[,2] <- rt1[,2]
    names(rt_coords) <- c('long', 'lat')
    
    # identify states
    states <- latlong2state(rt_coords)
    # add as column
    rt1$state <- states
    return(rt1)
  })
  
  output$hashtags1 <- renderDataTable({
    datatable(hashtag1())
  })
  
  output$favorite1 <- renderDataTable({
    datatable(top_favorite1())
  })
  
  output$retweeted1 <- renderDataTable({
    datatable(top_retweet1())
  })
  
  output$followed1 <- renderDataTable({
    datatable(top_followed1())
  })
  
  output$plot1 <- renderPlot({
    if(input$search_tweets){
      word1 <- input$keyword1
      word2 <- input$keyword2
    } else{
      word1 <- input$default1
      word2 <- input$default2
    }
    #Using GGPLOT, plot the Base World Map
    rt1 <- rt1()
    ggplot(rt1, aes(long, lat)) + borders('state', colour = 'gray50', fill='gray') + geom_point(color = 'blue', size = 0.8) + labs(title=paste('Map:', word1))+ theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'))
  })
  
  # Repeat the above operations for keyword 2
  # Structure is identical to above (omitted redundant comments) until line 416
  
  # Keyword 2
  ## search for 18000 tweets using the rstats hashtag
  rt_var2 <- reactive({
    if(input$search_tweets){
      create_token(
        app = "ergqwb12324r3",
        consumer_key = "jnw2qnJPESXlGsrdn2labCW4L",
        consumer_secret = "kKrWvONmIb8muABNgWFFG7A6d6HhtlZNodiXlzb1tRmdClGWsR",
        access_token = "632015603-ASMkADJhXvo7KcLt27Cgt7MIig2IfYtOc75xDBRu",
        access_secret = "BzXbPR6wnzjO9OrdZJskn9U21952gjVcbVMNqcltpZLE4")
      
      rt <- search_tweets(
        input$keyword2, n = 9000, include_rts = FALSE, geocode = lookup_coords('usa')
      )
    }
    else{
      filename <- paste(input$default2, "rds", sep=".")
      rt <- readRDS(filename)
    }
    
    rt <- rt %>%
      filter(lang == 'en') %>%
      select (screen_name, text, source, favorite_count, retweet_count, 
              hashtags, media_type, bbox_coords, followers_count)
    return(rt)
  })
  
  
  # Create reactive data frame
  hashtag2 <- reactive({
    rt2 <- rt_var2() %>%
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
    
    return(head(freq, 10))
    
  })
  
  # Create reactive data frame
  top_favorite2 <- reactive({
    rt3 <- rt_var2()%>%
      select(favorite_count, retweet_count, screen_name, text)
    
    rt3 <- rt3 %>%
      select(favorite_count, screen_name, text) %>%
      arrange(desc(favorite_count)) %>%
      select(screen_name, text) %>%
      head(10)
    
    return(rt3)
    
  })
  
  # Create reactive data frame
  top_retweet2 <- reactive({
    rt3 <- rt_var2()%>%
      select(favorite_count, retweet_count, screen_name, text)
    
    rt3 <- rt3 %>%
      select(retweet_count, screen_name, text) %>%
      arrange(desc(retweet_count)) %>%
      select(screen_name, text) %>%
      head(10)
    
    return(rt3)
    
  })
  
  top_followed2 <- reactive({
    rt4 <- rt_var2()  %>%
      select(followers_count, screen_name, text) %>%
      arrange(desc(followers_count)) %>%
      select(screen_name, text) %>%
      head(10)
    return(rt4)
    
  })
  
  rt2 <- reactive({
    rt_var <- rt_var2()
    n_coords_na <- sapply(rt_var$bbox_coords, FUN=function(x) sum(is.na(x)))
    rt_var$n_missing_coords <- n_coords_na
    
    rt1 <- rt_var%>%
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
    return(rt1)
  })
  
  states2 <- reactive({
    rt1 <- rt2()
    latlong2state <- function(pointsDF) {
      states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
      IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
      states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
      pointsSP <- SpatialPoints(pointsDF,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
      indices <- over(pointsSP, states_sp)
      stateNames <- sapply(states_sp@polygons, function(x) x@ID)
      stateNames[indices]
    }
    rt_coords <- rt1[,2:3]
    rt_coords[,1] <-rt1[,3] 
    rt_coords[,2] <- rt1[,2]
    names(rt_coords) <- c('long', 'lat')
    states <- latlong2state(rt_coords)
    rt1$state <- states
    return(rt1)
  })
  
  # merging the two geographical distributions
  joined_states<-reactive({
    key1 <- states1()
    key2 <- states2()
    # retrieve keyword1's state distribution and group by state
    key1 <- key1 %>%
      group_by(state) %>%
      summarise(count1 = n())
    # retrieve keyword2's state distribution and group by state
    key2 <- key2 %>%
      group_by(state) %>%
      summarise(count2 = n()) 
    # merge the two datasets by state
    key <- merge(key1, key2, by = 'state', all = TRUE)
    
    key <- key %>%
      filter(!is.na(state))
    # replace missing data with 0
    for(i in 1:length(key$state)){
      if(is.na(key[i,3]) ){
        key[i,3] = 0
      }else if(is.na(key[i,2])){
        key[i,2] = 0
      }
    }
    return(key)
  })
  
  output$hashtags2 <- renderDataTable({
    datatable(hashtag2())
  })
  
  output$favorite2 <- renderDataTable({
    datatable(top_favorite2())
  })
  
  output$retweeted2 <- renderDataTable({
    datatable(top_retweet2())
  })
  
  output$followed2 <- renderDataTable({
    datatable(top_followed2())
  })
  
  output$plot2 <- renderPlot({
    #Using GGPLOT, plot the Base World Map
    if(input$search_tweets){
      word1 <- input$keyword1
      word2 <- input$keyword2
    } else{
      word1 <- input$default1
      word2 <- input$default2
    }
    rt1 <- rt2()
    ggplot(rt1, aes(long, lat)) + borders('state', colour = 'gray50', fill='gray') + geom_point(color = 'red', size = 0.8) +labs(title=paste('Map:',word2)) +theme(plot.title = element_text(hjust = 0.5, size = 16, face='bold'))
  })
  
  output$comparison <- renderPlot({
    if(input$search_tweets){
      word1 <- input$keyword1
      word2 <- input$keyword2
    } else{
      word1 <- input$default1
      word2 <- input$default2
    }
    
    key <- joined_states()
    #state1 to contain names of states that keyword1 was used more than keyword2
    state1 <- vector()
    #state2 to include names of states that keyword2 was used more than keyword1
    state2 <- vector()
    #state3 to include names of states that keyword1 and keyword2 were used the same number of times
    state3 <- vector()
    for(i in 1:length(key$state)){
      if(key[i,2] > key[i,3] ){
        state1 <- c(state1, key[i,1])
      }else if(key[i,3] > key[i,2]){
        state2 <- c(state2, key[i,1])
      }else{
        state3 <- c(state3, key[i, 1])
      }
    }
    
    map(database = 'state')
    if(!identical(state1, logical(0))){
      #color states that used keyword1 more than keyword2 blue
      map(database = "state",regions = state1,col = "blue",fill=T,add=TRUE)
    }
    if(!identical(state2, logical(0))){
      #color states that used keyword2 more than keyword1 red
      map(database = "state",regions = state2,col = "red",fill=T,add=TRUE)
    }
    if(!identical(state3, logical(0))){
      #color neutral states light gray
      map(database = "state",regions = state3,col = "light gray",fill=T,add=TRUE)
    }
    title("Comparative Keyword Distribution By State")
    legend("bottomright", c(word1, word2, 'Neutral'), fill = c('blue', 'red','gray'))
  })
  
  output$comparison2 <- renderPlot({
    if(input$search_tweets){
      word1 <- input$keyword1
      word2 <- input$keyword2
    } else{
      word1 <- input$default1
      word2 <- input$default2
    }
    key <- joined_states()
    # top 10 states keyword 1 was used more than keyword2
    keyword1 <- key %>%
      mutate(diff = count1 - count2) %>%
      arrange(desc(diff)) %>%
      head(10)
    # graph the above
    ggplot(keyword1, aes(reorder(state, -diff, sum), diff))+
      xlab("Top 10 States")+ggtitle(paste('Top 10 States "',word1 ,'" Was Searched More Than "',word2,'"', sep=""))+
      theme(axis.text.x = element_text(angle=60, hjust=1))+
      geom_col()
  })
  
  output$comparison3 <- renderPlot({
    if(input$search_tweets){
      word1 <- input$keyword1
      word2 <- input$keyword2
    } else{
      word1 <- input$default1
      word2 <- input$default2
    }
    key <- joined_states()
    keyword2 <- key %>%
      mutate(diff = count2 - count1) %>%
      arrange(desc(diff)) %>%
      head(10)
    ggplot(keyword2, aes(reorder(state, -diff, sum), diff))+
      xlab("Top 10 States")+ggtitle(paste('Top 10 States "',word2 ,'" Was Searched More Than "',word1,'"', sep=""))+
      theme(axis.text.x = element_text(angle=60, hjust=1))+
      geom_col()
  })
  
}

shinyApp(ui = ui, server = server)

