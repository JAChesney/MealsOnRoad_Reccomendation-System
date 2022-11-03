shinyServer(function(input, output) {
  
  
    output$distPlot <- renderPlot({
      
      x    <- data1$SALES
      x    <- na.omit(x)
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "#75AADB", border = "black",
           xlab = "Sales",
           main = "Histogram of sales completed")
      
    })
    
})  