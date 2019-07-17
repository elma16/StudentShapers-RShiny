#----------------------

library(shiny)


#----------------------

# Define UI for application that draws a histogram
ui <- navbarPage("Convolutions",
                 
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("gamplottype", "Choose the plot",
                         list("Poisson" = "po","binomial"="bin"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           p("If X and Y are independent random variables and \\(Z = X + Y)\\ , then 8>< X fX (x)fY (z )")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
