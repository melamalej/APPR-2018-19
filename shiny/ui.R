library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Analiza posamezne panoge"),
   
  sidebarLayout(mainPanel(), sidebarPanel('Na spodnjem grafu lahko vidimo predikcijo na podlagi linearne regresije.')),
        
  selectInput(inputId = "panoga", label = "Izberi panogo:", choices = panoge),
    
  selectInput(inputId = "spremenljivka", label = "Izberi spremenljivko:",
              choices = c("Prihodki_od_prodaje_v_tisoc_EUR", "Bruto_poslovni_presezek_v_tisoc_EUR", 
                        "Stevilo_zaposlenih")),
    
  plotOutput("panoge")
    
))
