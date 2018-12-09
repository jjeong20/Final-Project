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
      textInput('keyword1', "First Keyword to Search:", ""),
      textInput("keyword2", "Second Keyword to Search:", ""),
      selectInput('default1', 'Pre-downloaded Keyword 1:', c('Apple', 'Samsung')),
      selectInput('default2', 'Pre-downloaded Keyword 2:', c('Apple', 'Samsung'))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel('Maps',
                    fluidRow(
                      column(width = 5,h2('Map: Keyword 1'), plotOutput("plot1")),
                      column(width = 5, h2('Map: Keyword 2'),plotOutput("plot2"))
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
                  ),
                  tabPanel('Top Sources',
                    fluidRow(
                      column(width = 5,h2('Top 6 Sources: Keyword 1'), dataTableOutput("sources1")),
                      column(width = 5, h2('Top 6 Sources: Keyword 2'),dataTableOutput("sources2"))                    
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
        app = "StatsProjectAmherst",
        consumer_key = "xC2cjGbujenmQyv86xAzAroDp",
        consumer_secret = "Pye14GbWEjUor2mnEq6GfICuf99M7OQajfJBhK6xH1R6mjE3df",
        access_token = "3062172722-dnleJhQfJ1UUBaE2wW6BYJ43tYPcPNSYp8jaRUc",
        access_secret = "w2G2EBRNnUOxdEqRCrHeYOPTE4dFTgT0qWC2OnJVC4uap")
      
      rt <- search_tweets(
        input$keyword1, n = 18000, include_rts = FALSE, geocode = lookup_coords('usa')
      )
    }
    else{
      filename <- paste(input$default1, "rds", sep=".")
      rt <- readRDS(filename)
    }
    
    rt <- rt %>%
      filter(lang == 'en') %>%
      select (screen_name, text, source, favorite_count, retweet_count, 
              hashtags, media_type, bbox_coords, followers_count)
    return(rt)
  })

  
  # Create reactive data frame
  hashtag1 <- reactive({
    rt2 <- rt_var1() %>%
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
  
  # Create reactive data frame
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
  
  top_followed1 <- reactive({
     rt4 <- rt_var1()  %>%
       select(followers_count, screen_name, text) %>%
       arrange(desc(followers_count)) %>%
       select(screen_name, text) %>%
       head(10)
     return(rt4)
    
  })
  
  top_sources1 <- reactive({
    
    rt5 <- rt_var1() %>%
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
    rt_var <- rt_var1()
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
  
  output$sources1 <- renderDataTable({
    datatable(top_sources1())
  })
  
  output$plot1 <- renderPlot({
    #Using GGPLOT, plot the Base World Map
    rt1 <- rt1()
    ggplot(rt1, aes(long, lat)) + borders('state', colour = 'gray50', fill='gray') + geom_point(color = 'red', size = 0.8) 
  })
  
  
  
  # Keyword 2
  ## search for 18000 tweets using the rstats hashtag
  rt_var2 <- reactive({
    if(input$search_tweets){
      create_token(
        app = "StatsProjectAmherst",
        consumer_key = "xC2cjGbujenmQyv86xAzAroDp",
        consumer_secret = "Pye14GbWEjUor2mnEq6GfICuf99M7OQajfJBhK6xH1R6mjE3df",
        access_token = "3062172722-dnleJhQfJ1UUBaE2wW6BYJ43tYPcPNSYp8jaRUc",
        access_secret = "w2G2EBRNnUOxdEqRCrHeYOPTE4dFTgT0qWC2OnJVC4uap")
      
      rt <- search_tweets(
        input$keyword2, n = 18000, include_rts = FALSE, geocode = lookup_coords('usa')
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
  
  top_sources2 <- reactive({
    
    rt5 <- rt_var2() %>%
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
  
  output$sources2 <- renderDataTable({
    datatable(top_sources2())
  })
  
  output$plot2 <- renderPlot({
    #Using GGPLOT, plot the Base World Map
    rt1 <- rt2()
    ggplot(rt1, aes(long, lat)) + borders('state', colour = 'gray50', fill='gray') + geom_point(color = 'blue', size = 0.8) 
  })
  
}

shinyApp(ui = ui, server = server)

