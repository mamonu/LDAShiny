library(shiny)
library(formattable)
fluidPage(
  # Application title
  titlePanel("Interactive LDA"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
     
     
      hr(),
      sliderInput("clust",
                  "clusters:",
                  min = 1,  max = 5, value = 3),
      
      sliderInput("ialpha",
                  "alpha:",
                  min = 0.01,  max = 0.05, value = 0.02),
      
      sliderInput("ieta",
                  "eta:",
                  min = 0.01,  max = 0.05, value = 0.02),
      
      
      sliderInput("top",
                  "number of top words per topic:",
                  min = 2,  max = 15, value = 5),
      
      sliderInput("iter",
                  "Number of Iterations:",
                  min = 1,  max = 5200,  value = 800)
    ),
    
    
    mainPanel(h4("LDA topics"),
      formattableOutput  ("LDA"),hr(),h4("Text"),
      verbatimTextOutput ("textout")
    )
  )
)

