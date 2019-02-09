library(shiny)

shinyServer(function(input, output){
  
  output$panoge <- renderPlot({
    
    data <- filter(Panoge.tidy, Panoga == input$panoga)
    data <- subset(data, Spremenljivka == input$spremenljivka)
    
    napoved <- Napoved(data)
    
    lin1 <- ggplot(data, aes(x=Leto, y=Stevilo/1e2)) +
      geom_line() +
      geom_point(data=napoved, aes(x=Leto, y=Stevilo/1e2), color="blue", size=3) + 
      geom_smooth(method = 'lm', se = FALSE) +
      xlab("Leto") + ylab("Spremenljivka (deljena s 100 000)") 
    
    
    print(lin1)
    
    })
})

