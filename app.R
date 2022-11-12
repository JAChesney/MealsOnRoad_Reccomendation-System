####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(RCurl)
library(ggplot2)

data1 <- read.csv(text = getURL("https://raw.githubusercontent.com/JAChesney/RShiny/main/Meals.csv"))
# data1 <- read.csv('D:/R/R Shiny learn/Mealssales.csv')
# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "EDA",
                  tabPanel("Sales",
                           sidebarPanel(
                             
                             # Input: Slider for the number of bins ----
                             sliderInput(inputId = "bins",
                                         label = "Number of bins:",
                                         min = 1,
                                         max = 50,
                                         value = 30)
                             
                           ), # sidebarPanel 1
                           mainPanel(
                             # Output: Histogram ----
                             plotOutput(outputId = "distPlot"),
                             
                           ) # mainPanel 1
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Correlation",
                           # sidebarPanel(
                           #   
                           #   # Input: Slider for the number of bins ----
                           #   sliderInput(inputId = "r",
                           #               label = "Correlation:",
                           #               min = -1,
                           #               max = 1,
                           #               value = 0,
                           #               step = 0.01,
                           #               ticks = F
                           #               )
                           #   
                           # ), # sidebarPanel 2
                           mainPanel(
                             plotOutput(outputId = 'corrPlot')
                           ) # mainPanel 2
                  ), # Correlation, tabPanel
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$distPlot <- renderPlot({

    x    <- data1$SALES
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Sales",
         main = "Histogram of sales completed")

  })
  
  output$corrPlot <- renderPlot({
    ggplot(data1, aes(x = RATING, y = RESTAURANT.RATING)) +
      geom_point() +
      # xlim(c(-4,4)) +
      # ylim(c(-4,4)) +
      coord_fixed() +
      labs(y = "Resturant Rating", x = "User Rating",
           title = paste("Simulated data with spearman correlation", 
                         cor(data1$RATING, data1$RESTAURANT.RATING, method = 'spearman')))
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
