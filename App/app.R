# load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(shiny)
library(shinythemes)
library(DT)
library(leaflet)
library(plyr)
library(plotly)


# load the datasets
wine_review <- read_csv("winemag-data-130k-v2.csv")
non_alc <- read_csv("grape juice - Sheet1.csv") %>%
  select(-id)
humour_data <- read_csv("shortjokes.csv")
ted_talks <- read_csv("ted_main.csv") 
yelp_reviews <- read_csv("yelp_business.csv")
movie_data <- read_csv("tmdb_5000_movies.csv")
music_data <- read_csv("p4kreviews.csv")

### Data Wrangling and Cleaning ###

# remove values with missing price
wine_clean <- wine_review %>%
  filter(!is.na(price)) %>%
  select(title, variety, price, points, description, country) %>%
  dplyr::rename("score" = "points", "wine" = "title") %>%
  sample_n(30) # pick sample of random wines to avoid slow runtime

# manually select appropriate jokes from Reddit data
humour_clean <- humour_data %>%
  filter(ID %in% c(1, 4, 5, 7, 8, 24, 25, 28, 54, 167, 12, 14, 21, 215, 234, 257, 297, 475))

# store string of 16 tags for user to insert as choices in selection menu later
tag_str <- "food|behavioral economics|bees|blockchain|comedy|performance art|statistics|origami|oceans|meditation|poetry|love|music|film|goal-setting|data"
tag_vector <- c("food", "behavioral economics", "bees", "blockchain", "comedy", "performance art", "statistics", "origami", "oceans", "meditation", "poetry", "love", "music", "film", "goal-setting", "data")
ted_clean <- ted_talks %>%
  select(title, main_speaker, description, url, tags) %>%
  mutate(tags = str_to_lower(tags)) %>%
  filter(str_detect(str = tags, pattern = tag_str)) %>%
  dplyr::rename("speaker" = "main_speaker")

# store in a string the desired types of businesses to retain in yelp dataset
cat_str <- "Restaurants|Nightlife|Bars|Ice Cream"
# filter out stray PA restaurants not near Pittsburgh
unwanted_id <- c("UqxbjYwfuaccOHyP7jMhEQ", "pf2TCiJEZbA6ezSWNIBePg", "c2sDJFcMv-r5V1d85KUM-w", "-MpXo24qYu4DazQXDrs2oA", "hTvGJCInYxnjpzC7dJWR0A", "_Fe6OFmVFAjwAXjkhZtIhw", "6W_od4P2FzRzKMkvQ5P2Ug", "Nrc137v_6TTAfzagdvw7Sw", "Rzw3_l2uDv-OSaHdPCJgjQ", "ShwzcV9YxWc3RsTzTIU0iQ", "wjW6YN9s-iDq7l9PcUsPZw", "mPec-6ARme_8QAHd7tpJ8g", "OEDgpWG0PGNJcY3aK1OXcw")
yelp_clean <- yelp_reviews %>%
  filter(str_detect(string = categories, pattern = cat_str), state == "PA", stars >= 4) %>%
  mutate(name = str_replace_all(pattern = "\"", replacement = "", string = name),
         address = str_replace_all(pattern = "\"", replacement = "", string = address)) %>%
  select(name, stars, address, latitude, longitude, postal_code, city, state, categories, business_id) %>%
  filter(!(business_id %in% unwanted_id)) %>%
  sample_n(42) # choose random sample of restaurants

# data set to be displayed in app data table output
yelp_display <- yelp_clean %>%
  select(-latitude, -longitude, -state, -business_id) %>%
  mutate(categories = gsub(pattern = ";", replacement = ", ", x = categories))

# store desired movie genres in string
genre_str <- "Animation|Comedy|Drama|Horror|Romance"
genre_vector <- c("Animation", "Comedy", "Drama", "Horror", "Romance")
movies_clean <- movie_data %>%
  filter(str_detect(string = genres, pattern = genre_str)) %>%
  select(title, overview, vote_average, vote_count, runtime, genres) %>%
  arrange(desc(vote_count)) # initially sort by proxy for popularity

# store desired music genres in char. vector
music_vector <- c("Folk/Country", "Jazz", "Rock", "Pop/R&B")
music_clean <- music_data %>%
  dplyr::rename("id" = "X1") %>%
  select(-review, -best, -date) %>%
  filter(genre %in% music_vector, score >= 8.0) %>%
  filter(!str_detect(string = artist, pattern =  "�"), !str_detect(string = album, pattern =  "�"))

# store links of datasets and data origins
link1 <- "https://www.kaggle.com/ermoore/pitchfork-reviews-through-12617"
source1 <- "https://pitchfork.com/"
link2 <- "https://www.kaggle.com/yelp-dataset/yelp-dataset"
source2 <- "https://www.yelp.com/"
link3 <- "https://www.kaggle.com/abhinavmoudgil95/short-jokes"
source3 <- "https://www.reddit.com/r/cleanjokes/"
link4 <- "https://www.kaggle.com/rounakbanik/ted-talks"
source4 <- "https://www.ted.com/"
link5 <- "https://www.kaggle.com/zynicide/wine-reviews"
source5 <- "http://www.wineenthusiast.com/"
link6 <- "https://www.kaggle.com/tmdb/tmdb-movie-metadata"
source6 <- "https://www.themoviedb.org/"
link7 <- "https://www.target.com/c/beverages-food-beverage/-/N-5xt0r"
source7 <- "https://www.target.com/"

##### UI Side #####
ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  
  titlePanel("Wherefore Art Thou RShiny? A Widget for Modern Romance"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      wellPanel(radioButtons(inputId = "location",
                             label = "Do you want to plan a night in or a night out?",
                             choices = list("Night in!", "Night out!"),
                             selected = character(0))),
      
      conditionalPanel(condition = "input.location == 'Night out!'",
                       wellPanel(tags$b("Take a look at our list of Pittsburgh-area date spots in the Date Spots tab!"))),
      
      conditionalPanel(condition = "input.location == 'Night in!'",
                       wellPanel(radioButtons(inputId = "legal",
                                              label = "Are you and your date both 21 or older?",
                                              choices = c("Yes", "No"),
                                              selected = character(0)))),
      
      conditionalPanel(condition = "input.legal == 'No' & input.location == 'Night in!'", 
                       wellPanel(tags$b("That's too bad! Check out our selection of wine substitutes in the Wine Alternatives tabs."))
                       ),
     
      conditionalPanel(condition = "input.legal == 'Yes' & input.location == 'Night in!'",
                       wellPanel(tags$b("Congratulations! Check out our selection of wines in the Wines tab."),
                                 br(), br(),
                                 sliderInput(inputId = "budget",
                                             label = "What's your budget range for drinks on this date?",
                                             min = 0, max = max(wine_clean$price),
                                             step = round_any(0.05 * max(wine_clean$price), 5),
                                             value = c(max(wine_clean$price))),
                                 textOutput(outputId = "message", container = span))),
      
      conditionalPanel(condition = "input.location == 'Night in!'",
                       wellPanel(radioButtons(label = "Let's pick a date night activity! Are you in the mood to listen, to watch, or to learn? Feel free to explore each option. You can also click 'Night out!' at the top of the page to explore further.",
                                              inputId = "activity",
                                              choices = c("Listen to some music", "Watch a movie", "Learn with TED talks"),
                                              selected = character(0)))),
      
      conditionalPanel(condition = "input.activity == 'Watch a movie' & input.location == 'Night in!'",
                       wellPanel(selectInput(inputId = "movie_genre",
                                             label = "Choose a movie genre to set the mood. Once you've picked a genre, check out the Movies tab!",
                                             choices = c("", genre_vector),
                                             selected = ""))),
      
      conditionalPanel(condition = "input.activity == 'Learn with TED talks' & input.location == 'Night in!'",
                       wellPanel(selectInput(inputId = "ted_genre",
                                             label = "Choose a TED talk genre to set the mood. Once you've picked a genre, check out the TED talks tab!",
                                             choices = c("", sort(str_to_title(tag_vector))),
                                             selected = ""))),
      
      conditionalPanel(condition = "input.activity == 'Listen to some music' & input.location == 'Night in!'",
                       wellPanel(selectInput(inputId = "music_genre",
                                             label = "Choose a music genre to set the mood. Once you've picked a genre, check out the Music tab!",
                                             choices = c("", sort(str_to_title(music_vector))),
                                             selected = ""))),
    
      conditionalPanel(condition = "input.location == 'Night out!'",
                       wellPanel(tags$b("Humor is important in achieving success while dating. Keep clicking the button below to make your date laugh:"),
                                 br(),
                                 actionButton(inputId = "click",
                                              label = "Get a new joke!"),
                                 br(), 
                                 br(),
                                 tags$b(textOutput(outputId = "joke", container = span)),
                                 br(),
                                 br(),
                                 tags$b("If you don't feel like going out anymore, click the 'Night in!' button to explore other options.")))
      
      
    ),
    
    mainPanel(
      
      navbarPage(
        
        title = "Date Essentials",
        
        tabPanel("Introduction", htmlOutput(outputId = "narrative")),

        tabPanel("Wines", plotlyOutput(outputId = "winePlot"),
                br(),
                br(),
                br(),
                br(),
                DT::dataTableOutput(outputId = "wineTable")),
        
        tabPanel("Wine Alternatives", DT::dataTableOutput(outputId = "grapeTable")),
        
        tabPanel("Music", 
                 htmlOutput(outputId = "musicDirections"),
                 DT::dataTableOutput(outputId = "tunesTable")),
        
        tabPanel("Movies", 
                 htmlOutput(outputId = "movieDirections"),
                 DT::dataTableOutput(outputId = "movieTable")),
        
        tabPanel("TED Talks",
                 htmlOutput(outputId = "TEDDirections"),
                 htmlOutput("hyperlink"),
                 br(),
                 DT::dataTableOutput(outputId = "tedTable")),
        
        tabPanel("Date Spots",
                 htmlOutput("default"), htmlOutput("directions"),
                 br(),
                 leafletOutput("map"),
                 br(),
                 DT::dataTableOutput(outputId = "dateTable")),
        
        tabPanel("Data Sources",
                 htmlOutput("wineSource"),
                 br(),
                 htmlOutput("movieSource"),
                 br(),
                 htmlOutput("tedSource"),
                 br(),
                 htmlOutput("musicSource"),
                 br(),
                 htmlOutput("dateSource"),
                 br(),
                 htmlOutput("jokeSource"),
                 br(),
                 htmlOutput("grapeSource"))
      ))
  )
)

##### Server Side #####
server <- function(input, output){
  
  output$wineSource <- renderText({
    HTML(paste0("<a href='", link5, "' target='_blank'>Wine Data Link</a>", "<br>", "<a href='", source5, "' target='_blank'>Wine Data Origin: Wine Enthusiast</a>"))
    })
  
  output$musicSource <- renderText({
    HTML(paste0("<a href='", link1, "' target='_blank'>Music Data Link</a>", "<br>", "<a href='", source1, "' target='_blank'>Music Data Origin: Pitchfork</a>"))
  })

  output$dateSource <- renderText({
    HTML(paste0("<a href='", link2, "' target='_blank'>Yelp Data Link</a>", "<br>", "<a href='", source2, "' target='_blank'>Yelp Data Origin: Yelp</a>"))
  })

  output$jokeSource <- renderText({
    HTML(paste0("<a href='", link3, "' target='_blank'>Joke Data Link</a>", "<br>", "<a href='", source3, "' target='_blank'>Joke Data Origin: Reddit</a>"))
  })

  output$tedSource <- renderText({
    HTML(paste0("<a href='", link4, "' target='_blank'>TED Talk Data Link</a>", "<br>", "<a href='", source4, "' target='_blank'>TED Talk Data Origin: TED</a>"))
  })

  output$movieSource <- renderText({
    HTML(paste0("<a href='", link6, "' target='_blank'>Movie Data Link</a>", "<br>", "<a href='", source6, "' target='_blank'>Movie Data Origin: The Movie Database</a>"))
  })

  output$grapeSource <- renderText({
    HTML(paste0("<a href='", link7, "' target='_blank'>Non-Alcoholic Beverages Data Link</a>", "<br>", "<a href='", source7, "' target='_blank'>Non-Alcoholic Beverages Data Origin: Target</a>"))
  })
  
  output$musicDirections <- renderText({
    req(input$activity == 'Listen to some music', input$location == 'Night in!', input$music_genre == "")
    HTML(paste0("<b>", "Select a genre in the side panel to view a table of excellent albums!", "</b>"))
  })

  output$movieDirections <- renderText({
    req(input$activity == 'Watch a movie', input$location == 'Night in!', input$movie_genre == "")
    HTML(paste0("<b>", "Select a genre in the side panel to view a table of awesome movies!", "</b>"))
  })

  output$TEDDirections <- renderText({
    req(input$activity == 'Learn with TED talks', input$location == 'Night in!', input$ted_genre == "")
    HTML(paste0("<b>", "Select a genre in the side panel to view a table of great TED talks!", "</b>"))
  })
  
  # text for narrative
  output$narrative <- renderText({
    link1 <- "https://github.com/kvu1/Rose-is-a-Rose"
    link2 <- "http://nautil.us/issue/41/selection/the-problem-with-modern-romance-is-too-much-choice"
    narrative1 <- "While Kyle, one of our authors, has so far followed his relative's advice <a href='https://github.com/kvu1/Rose-is-a-Rose' target='_blank'> not to get married while in college</a>"
    narrative2 <- ", he has still forayed into the 21st century dating scene while at Swarthmore College. In the process of fumbling through multiple dates (more than one date has told him that he talks about `dplyr` too much), Kyle has reached an epiphany about modern romance: he and many of his peers have no idea what they're doing. Living in a world so saturated with options has led to a <a href='http://nautil.us/issue/41/selection/the-problem-with-modern-romance-is-too-much-choice' target='_blank'>paradox of choice for those seeking love.</a> Faced with so many potential partners to meet and so many activities to serve as the backdrops to our dates, a growing number of young adults become so intimidated that they resign to ceasing their pursuit of romance wholesale."
    narrative3 <- "What if dating could be simplified? What if we could hone in on quality experiences and avoid adding to our lists of dating horror stories? Kyle will be spending some time in Pittsburgh between the end of the semester and the start of his internship, so, accompanied by datasets about date essentials and some Yelp hotspots in Pittsburgh, he will set out with his colleague Melissa to create an RShiny app to help streamline the date planning decision tree for some lucky locals in the Pittsburgh area."
    narrative4 <- "Start using the app by choosing between a night in and a night out!"
    HTML(paste0("<p>", narrative1, narrative2, "</p>",
                "<p>", narrative3, "</p>",
                "<b><p>", narrative4, "<b></p>"))
    })
  
  # interactive wine plot
  output$winePlot <- renderPlotly({
    req(input$location == "Night in!", input$legal == "Yes", input$budget)
    custom_wine <- filter(wine_clean, price <= input$budget[1])
    plot_ly(data = custom_wine, 
            x = ~price, 
            y = ~score,
            type = 'scatter', 
            mode = 'markers',
            marker = list(color = "#68228b", size = 15, opacity = 0.55),
            hoverinfo = 'text',
            text = ~paste("Wine:", wine,
                          "<br>Origin: ", country,
                          "<br>Variety: ", variety)) %>%
      layout(title = "Prices and Ratings of Selected Wines", xaxis = list(title = "Wine Price ($)"), yaxis = list(title = "Rating (out of 100)"))
  })
  
  # only render respective tables when conditions in `req()` are met
  output$wineTable <- DT::renderDataTable({
    req(input$location == "Night in!", input$legal == "Yes", input$budget)
    custom_wine <- filter(wine_clean, price <= input$budget[1])
    DT::datatable(data = custom_wine, rownames = FALSE, selection = 'none')
  }, server = FALSE)
  
  output$grapeTable <- DT::renderDataTable({
    req(input$location == "Night in!", input$legal == "No")
    DT::datatable(data = non_alc, rownames = FALSE, selection = 'none')
  }, server = FALSE)
  
  output$tunesTable <- DT::renderDataTable({
    req(input$location == "Night in!", input$activity == "Listen to some music", input$music_genre != "")
    custom_music <- music_clean %>%
      filter(genre == input$music_genre) %>%
      select(-id)
    DT::datatable(data = custom_music, rownames = FALSE, selection = 'none')
  })
  
  output$movieTable <- DT::renderDataTable({
    req(input$location == "Night in!", input$activity == "Watch a movie", input$movie_genre != "") 
    custom_movie <- movies_clean %>%
      filter(str_detect(string = genres, pattern = input$movie_genre)) %>%
      select(-genres)
    DT::datatable(data = custom_movie, rownames = FALSE, selection = 'none')
  }, server = FALSE)
  
  output$tedTable <- DT::renderDataTable({
    req(input$location == "Night in!", input$activity == "Learn with TED talks", input$ted_genre != "")
    custom_ted <- ted_clean %>%
      filter(str_detect(string = tags, pattern = str_to_lower(input$ted_genre))) %>%
      mutate(title = paste0("<a href='", url,"' target='_blank'>", title, "</a>")) %>% # render TED talk titles as HTML hyperlinks
      select(-tags, -url)
    DT::datatable(data = custom_ted, rownames = FALSE, escape = FALSE, selection = 'none')
  }, server = FALSE)
  
  output$default <- renderText({
    req(input$location == "Night out!", is.null(input$dateTable_rows_selected))
    HTML(paste0("<b>", "Click on a restaurant's row in the table to have it appear on the map below.", "</b>"))
  })
  
  output$directions <- renderText({
    req(input$location == "Night out!", !(is.null(input$dateTable_rows_selected)))
    HTML(paste0("<b>", "Hover over a marker to see that restaurant's name. Click the marker to see that restaurant's score out of 5.", "</b>"))
  })
  
  output$hyperlink <- renderText({
    req(input$location == "Night in!", input$activity == "Learn with TED talks", input$ted_genre != "")
    HTML(paste0("<b>","Click on a title to be redirected to the talk!","</b>"))
  })
  
  output$map <- renderLeaflet({
    req(input$location == "Night out!")
    
    # create heart markers
    icons <- awesomeIcons(
      icon = 'ios-heart',
      iconColor = 'white',
      library = 'ion',
      markerColor = "firebrick2")
    
    # map first restaurant as placeholder before row selection
    ifelse(is.null(input$dateTable_rows_selected),
           choice <- 1,
           choice <- input$dateTable_rows_selected)
    
    yelp_choice <- yelp_clean %>%
      filter(row_number() %in% choice)
    
    leaflet(yelp_choice, options = leafletOptions(minZoom = 5, maxZoom = 10, zoom = 5)) %>%
       addProviderTiles(providers$Hydda) %>%
       addAwesomeMarkers(icon = icons,
                         label = yelp_choice$name,
                         popup = paste(as.character(yelp_choice$stars), "stars")) # convert stars rating to character to display it upon user click
  })
  
  output$dateTable <- DT::renderDataTable({
    req(input$location == "Night out!")
    DT::datatable(data = yelp_display, rownames = TRUE, selection = "multiple")
  }, server = FALSE)
  
  # randomly pick joke upon click input; replace so that user can click button more than 18 times
  text <- eventReactive(input$click, {
    req(input$location == "Night out!")
    random_joke <- sample(humour_clean$Joke, 1, replace = TRUE)
  })
  
  output$joke <- renderText({            
    req(input$click)
    text()
  })
}

shinyApp(ui = ui, server = server)