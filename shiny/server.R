library(shiny)

shinyServer(function(input, output){
  
  output$panoge <- renderPlot({
    
    data <- filter(Panoge.tidy, Panoga == input$panoga)
    data <- subset(data, Spremenljivka == input$spremenljivka)
    
    napoved <- Napoved(data)
    
    lin1 <- ggplot(data, aes(x=Leto, y=Število)) +
      geom_line() +
      geom_point(data=napoved, aes(x=Leto, y=Število), color="blue", size=3) + 
      geom_smooth(method = 'lm', se = FALSE) +
      xlab("Leto") + ylab("Spremenljivka") 
    
    
    print(lin1)
    
    })
})

