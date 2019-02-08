library(shiny)

shinyServer(function(input, output){
  
  output$panoge <- renderPlot({
    
    spr <- input$spremenljivka
    data <- Panoge %>% filter(Panoga == input$panoga) %>% select(input$spremenljivka)
                              
    g <- ggplot(data, aes(x = "Leto", y = spr)) + 
      geom_point() 
    
    g + xlim(2005, 2023) +
      geom_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
      xlab("Leto") + ylab("Spremenljivka") +
      scale_x_discrete()
    
    })
})


