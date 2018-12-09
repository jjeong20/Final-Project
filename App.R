library(shiny)
library(dplyr)
library(rtweet)
library(DT)
library(ggplot2)


# Define UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Twitter Keyword Analysis"),
  

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      checkboxInput("search_tweets", "Search Tweets:",
                    FALSE),
      textInput("keyword", "Keyword to search:", "")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      
      #1 - map with locations: geo_coords
      #2 - top hashtags: hashtags
      #3 - top ten tweets based on favorite_count and retweet_count: favorite_count, retweet_count, text, screen_name
      #4 - top ten tweets by most followed people: followers_count, screen_name, text
      #5 - top sources
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("plot")),
                  tabPanel("Top Hashtags", dataTableOutput("hashtags")),
                  tabPanel("Top Favorited Tweets", dataTableOutput("favorite")),
                  tabPanel("Top Retweeted Tweets", dataTableOutput("retweeted")),
                  tabPanel("Tweets by Most Followed Users", dataTableOutput("followed")),
                  tabPanel("Top 6 Sources", dataTableOutput("sources"))
      )
      
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  create_token(
    app = "StatsProjectAmherst",
    consumer_key = "xC2cjGbujenmQyv86xAzAroDp",
    consumer_secret = "Pye14GbWEjUor2mnEq6GfICuf99M7OQajfJBhK6xH1R6mjE3df",
    access_token = "3062172722-dnleJhQfJ1UUBaE2wW6BYJ43tYPcPNSYp8jaRUc",
    access_secret = "w2G2EBRNnUOxdEqRCrHeYOPTE4dFTgT0qWC2OnJVC4uap")
  
  ## search for 18000 tweets using the rstats hashtag
  rt_var <- reactive({
    if(input$search_tweets){
      rt <- search_tweets(
        input$keyword, n = 18000, include_rts = FALSE, geocode = lookup_coords('usa')
      )
    }
    else{
      rt <- readRDS('fire.rda')
    }
    
    rt <- rt %>%
      filter(lang == 'en') %>%
      select (screen_name, text, source, favorite_count, retweet_count, 
              hashtags, media_type, bbox_coords, followers_count)
    return(rt)
  })

  
  # Create reactive data frame
  hashtag <- reactive({
    rt2 <- rt_var() %>%
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
  top_favorite <- reactive({
    rt3 <- rt_var()%>%
      select(favorite_count, retweet_count, screen_name, text)
    
    rt3 <- rt3 %>%
      select(favorite_count, screen_name, text) %>%
      arrange(desc(favorite_count)) %>%
      head(10)
    
    return(rt3)

  })
  
  # Create reactive data frame
  top_retweet <- reactive({
    rt3 <- rt_var()%>%
      select(favorite_count, retweet_count, screen_name, text)
    
    rt3 <- rt3 %>%
      select(retweet_count, screen_name, text) %>%
      arrange(desc(retweet_count)) %>%
      head(10)
    
    return(rt3)
    
  })
  
  top_followed <- reactive({
     rt4 <- rt_var()  %>%
       select(followers_count, screen_name, text) %>%
       arrange(desc(followers_count)) %>%
       head(10)
     return(rt4)
    
  })
  
  top_sources <- reactive({
    
    rt5 <- rt_var() %>%
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
    return(sources)
  })
  
  rt1 <- reactive({
    rt_var <- rt_var()
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
  
  output$hashtags <- renderDataTable({
    datatable(hashtag())
  })
  
  output$favorite <- renderDataTable({
    datatable(top_favorite())
  })
  
  output$retweeted <- renderDataTable({
    datatable(top_retweet())
  })
  
  output$followed <- renderDataTable({
    datatable(top_followed())
  })
  
  output$sources <- renderDataTable({
    datatable(top_sources())
  })
  
  output$plot <- renderPlot({
    #Using GGPLOT, plot the Base World Map
    rt1 <- rt1()
    mapWorld <- borders("United States", colour="gray50", fill="gray50") # create a layer of borders
    mp <- ggplot() +   mapWorld
    
    #Now Layer the cities on top
    mp + geom_point(aes(rt1$long, rt1$lat), color = 'blue', size = 1)
  })
  
  
}

shinyApp(ui = ui, server = server)

