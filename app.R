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
library(dplyr)

data1 <- read.csv(text = getURL("https://raw.githubusercontent.com/JAChesney/RShiny/main/Meals.csv"))
# data1 <- read.csv('D:/R/R Shiny learn/Mealssales.csv')
# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
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
                           #   )
                           # ), # sidebarPanel 2
                           # sliderInput(inputId = "rescorr",
                           #             label = "Restaurant Rating:",
                           #             min = 0,
                           #             max = 5.0,
                           #             value = c(0, 5),
                           #             step = 0.1),
                           # sliderInput(inputId = "foodcorr",
                           #             label = "Food Rating:",
                           #             min = 0,
                           #             max = 5.0,
                           #             value = c(0, 5),
                           #             step = 0.1),
                           # ),
                           mainPanel(
                             plotOutput(outputId = 'corrPlot')
                           ) # mainPanel 2
                  ), # Correlation, tabPanel 3
                  tabPanel("Best Item",
                           sidebarPanel(width = 4,
                                        selectInput(
                                          inputId = 'OrderItems',
                                          label = 'Food Items',
                                          list("TEA",
                                               "CHOWMIN",
                                               "PASTA","ALOO TIKKY",        
                                               "CHOLEY SAMOSE",    
                                               "CHOLEY BHATUREY",   
                                               "PAPDI CHAAT",       
                                               "PAO BHAJI",        
                                               "DAHI BHALLA",       
                                               "PEPSI",             
                                               "COCACOLA",         
                                               "COFFEE",            
                                               "DOSA",              
                                               "IDLI",             
                                               "MASALA DOSA",       
                                               "UTTAPAM",           
                                               "VEG THALI",        
                                               "SPECIAL VEG THALI", 
                                               "RAJMA RICE",        
                                               "CHOLEY RICE",      
                                               "KADI RICE",         
                                               "VEG PULAO",         
                                               "GULAB JAMUN",      
                                               "RASGULLA",          
                                               "RASMALAI",            
                                               "BESAN LADDU",       
                                               "RASMALAI",         
                                               "BURGER",          
                                               "VEG COMBO"),
                                        ) 
                           ),
                           mainPanel(width = 12,
                                     plotOutput(outputId = 'mostordered')
                           ) # mainPanel 3
                  ), # Barplot2, tabPanel 4
                  tabPanel("Recommendation 1",
                           sidebarPanel(width = 4,
                                        selectInput(
                                          inputId = 'CatNames',
                                          label = 'Food Category:',
                                          list("BEVERAGES",
                                               "FAST FOOD",
                                               "INDIAN SNACKS",
                                               "SOUTH INDIAN",
                                               "NORTH INDIAN",
                                               "SWEETS"),
                                        ),
                                        selectInput(
                                          inputId = 'RestaurantNames',
                                          label = 'Food Points:',
                                          list("SPINGO FOODS",       
                                               "KRISHNA YATRI PLAZA",  
                                               "ANNUPURNA YATRI PLAZA",
                                               "NEHA YATRI PLAZA",     
                                               "SHIVA TOURIST DHABA",
                                               "ARVIND PALACE",      
                                               "YUVRAAJ PALACE",      
                                               "BATI SWEETS",           
                                               "MISHRA YATRI PLAZA"),
                                        ),
                                        sliderInput(inputId = "resbins",
                                                    label = "Restaurant Rating:",
                                                    min = 0,
                                                    max = 5.0,
                                                    value = c(0, 5),
                                                    step = 0.1),
                                        sliderInput(inputId = "userbins",
                                                    label = "Food Rating:",
                                                    min = 0,
                                                    max = 5.0,
                                                    value = c(0, 5),
                                                    step = 0.1),
                           ),
                           mainPanel(width = 8,
                                     plotOutput(outputId = 'recommendations')
                           ) # mainPanel 4
                  ),
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  # Sales
  output$distPlot <- renderPlot({
    
    x    <- data1$SALES
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Sales",
         main = "Histogram of sales completed")
    
  })
  
  # Correaltion
  output$corrPlot <- renderPlot({
    ggplot(data1, aes(x = RATING, y = RESTAURANT.RATING)) +
      geom_point() +
      theme_bw()+
      # coord_cartesian(ylim=input$rescorr, xlim = input$foodcorr)+
      labs(y = "Restaurant Rating", x = "Food Rating",
           title = paste("Simulated data with spearman correlation", 
                         cor(data1$RATING, data1$RESTAURANT.RATING, method = 'spearman')))
  })
  
  # Best Item
  re <- reactive({
    req(input$OrderItems)
    dt <- data1 %>% filter(data1$ORDER.ITEMS %in% input$OrderItems) %>% group_by(FOOD.POINT) %>% summarize(RATING = mean(RATING))
  })
  
  output$mostordered <- renderPlot({
    ggplot(re(), aes(x = FOOD.POINT, y = RATING), fill = FOOD.POINT) +
      geom_bar(stat = "identity") +
      # scale_x_discrete(breaks=item_brks, labels = item_lbls)+
      theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 10, hjust = 0.5, vjust = 0.5),
            text = element_text(size = 16), 
            axis.text.y = element_text(colour = "grey20", size = 12, angle = 10, hjust = 0.5, vjust = 0.5)) +
      labs(y = "Food Rating", x = "Resturants",
           title = paste("Average Food Rating"))
  })
  
  # Recomendation System
  r2 <- reactive({
    # data2 <- data1[data1$RESTAURANT.RATING>=input$resbins[1]&data1$RESTAURANT.RATING<=input$resbins[2],]
    req(input$RestaurantNames)
    d <- data1 %>% filter(FOOD.POINT %in% input$RestaurantNames, CATEGORY %in% input$CatNames) %>% group_by(FOOD.POINT, ORDER.ITEMS, CATEGORY) %>% summarize(RATING = mean(RATING), RESTAURANT = mean(RESTAURANT.RATING))
  })
  
  r3 <- reactive({
    req(input$RestaurantNames)
    d1 <- data1 %>% filter(FOOD.POINT %in% input$RestaurantNames, CATEGORY %in% input$CatNames) %>% group_by(FOOD.POINT, ORDER.ITEMS, CATEGORY) %>% summarize(RATING = mean(RATING), RESTAURANT = mean(RESTAURANT.RATING))
    foodval <- d1$ORDER.ITEMS  
  })
  
  output$recommendations <- renderPlot({
    ggplot(r2(), aes(x = RESTAURANT, y = RATING), fill = data1$ORDER.ITEMS)+
      geom_point() +
      coord_cartesian(ylim=input$userbins, xlim = input$resbins)+
      theme(axis.text.x = element_text(colour = "grey20", size = 12, hjust = 0.5, vjust = 0.5),
            text = element_text(size = 16),
            axis.text.y = element_text(colour = "grey20", size = 12, hjust = 0.5, vjust = 0.5)) +
      theme_bw()+
      labs(y = "Food Rating", x = "Resturant Rating",
           title = paste("Food Recomendation"))+
      geom_text(label = r3(),
                nudge_y = 0.05)
    
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)