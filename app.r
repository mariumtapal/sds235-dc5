library(shiny)
library(tidyverse)
library(readr)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Bigrams That Appeared Most Frequently in Posts"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", 
                     h4("Date range"), 
                     start = "2020-04-06", end = "2020-04-10"), 
      checkboxGroupInput("checkGroup", 
                         h4("Account"), 
                         choices = list("DerekNolan" = 1, 
                                        "ChloeJohnson" = 2, 
                                        "___3333___" = 3),
                         selected = c("1", "2", "3")), 
      selectInput("select", 
                  h4("Location"), 
                  choices = list("All" = 1, 
                                 "Downtown" = 2,
                                 "Weston" = 3), 
                  selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("freq_bi_plot")
    ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  social_media <- here::here("data/Y*Int Social Media Data/YInt.csv") %>%
    read_csv()
  static_sensor_locations <- here::here("data/Sensor Data and Maps/StaticSensorLocations.csv") %>%
    read_csv()
  static_sensor_readings <- here::here("data/Sensor Data and Maps/StaticSensorReadings.csv") %>%
    read_csv()
  mobile_sensor_readings <- here::here("data/Sensor Data and Maps/MobileSensorReadings.csv") %>%
    read_csv()
  mc1_reports_data <- here::here("data/Damage Reports/mc1-reports-data.csv") %>%
    read_csv()
  
  
  
  
  output$freq_bi_plot <- renderPlot({
      
    bigrams <- social_media %>%
      unnest_tokens(bigram, message, token = "ngrams", n = 2)
    
    # separating bigrams into two columns
    bigrams_separated <- bigrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    # filters out stop words in bigrams
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    
    # common bigrams without stop words
    bigrams_united <- bigrams_filtered %>%
      unite(bigram, word1, word2, sep = " ") %>%
      count(bigram, sort = TRUE) %>%
      slice(1:20) %>%
      filter(rank(desc(n))>0)
    
    # draw plot
    ggplot(data = bigrams_united, aes(x = reorder(bigram, n), y = n)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      labs(title = "Bigrams That Appeared Most Frequently in Posts", x = "Bigram", y = "Number of Occurances Accross All Posts")

    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

