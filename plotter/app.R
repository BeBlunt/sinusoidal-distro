library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("alphabeta", "alpha, beta:",
                  min = -50, max = 50, value = c(-20,20)),
      sliderInput("omega", "omega:",
                  min = 1, max = 50, value = 30),
      sliderInput("chi", "chi:",
                  min = 1, max = 50, value = 30),
      
      sliderInput("fromto", "from, to:",
                  min = -50, max = 50, value = c(-20,20)),
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
     plotOutput("distPlot"),
     print(h3('Distances'))
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      #input$alpha
    al = input$alphabeta[1]
    de = input$alphabeta[2] - al
    om = input$omega
    ch = input$chi
    
    fromto = input$fromto
    refDist = as.function(input$refDist)
    curve(dnorm(x,0,1), from=fromto[1], to=fromto[2])
    curve(dsinu(x, al, de, om, ch), add=T, col='red')
    })

  output$distances = 
}

# Run the application 
shinyApp(ui = ui, server = server)
