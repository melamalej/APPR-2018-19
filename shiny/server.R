library(shiny)

shinyServer(function(input, output){
  
  output$panoge <- renderPlot({
    
    spr <- input$spremenljivka
    data <- Panoge %>% filter(Panoga == input$panoga) %>% select(input$spremenljivka)
    
    fit <- lm(data, spr ~ Leto)
    l <- data.frame(Leto=seq(2005, 2023))
    predict(fit, l)
    napoved <- l %>% mutate(spr=predict(fit, .))
    
    lin1 <- ggplot(data, aes(x=Leto, y=spr)) +
      geom_line() +
      geom_point(data=napoved, aes(x=Leto, y=spr), color="blue", size=3) + 
      geom_smooth(method = 'lm', se = FALSE) +
      xlab("Leto") + ylab("Spremenljivka") 
    
    })
})

