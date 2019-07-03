library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    title = "QQ plots",
    
    # Show a plot of the generated distribution
    plotOutput("qqplot"),
    
    hr(),
    
    # Sidebar with a slider input for the mean, sd, skew and kurtosis 
    fluidRow(
        column(3,
               h4("First variable"),
               selectInput("pdf", "Choose the pdf:",
                           choices = c("Standard Normal" = "rnorm", "Exponential", "Uniform")),
               conditionalPanel(
                   condition="input.pdf==\"rnorm\"",
                   helpText("Contour plot of the pdf of N(0,\\(\\Sigma\\))"),
                   helpText("where \\(\\Sigma=\\begin{pmatrix}\\sigma_1^2&\\rho\\sigma_1\\sigma_2\\\\\\rho\\sigma_1\\sigma_2&\\sigma_2^2\\end{pmatrix}\\)"),
                   sliderInput("mvnorm.sd1","\\(\\sigma_1\\)",min=0.1,max=10,value=1,step=0.1),
                   sliderInput("mvnorm.sd2","\\(\\sigma_2\\)",min=0.1,max=10,value=1,step=0.1),
                   sliderInput("mvnorm.rho","\\(\\rho\\)",min=-0.99,max=0.99,value=0,step=0.01),
                   helpText("(using the package mvtnorm in R)")
               )
        ),
        column(3, offset = 1,
               h4("Second variable"),
               selectInput("pdf", "Choose the pdf:",
                           choices = c("standard normal", "binomial", "poisson")),
               sliderInput("n2",
                           "Number of trials:",
                           min = 1,
                           max = 1000,
                           value = 50),
               
               sliderInput("mean2",
                           "Mean:",
                           min = 0,
                           max = 1,
                           value = 0.5),
               
               sliderInput("sd2", "Standard deviation:",
                           min = 0, max = 5,
                           value = 1, step = 0.1)
        ),
        column(3, offset = 1,
               h4("General Information")

        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$qqplot <- renderPlot({
        # normal dists given the values from sliders
        
        x <- rnorm(n = input$n1, mean = input$mean1,sd = input$sd1)
        y <- rnorm(n = input$n2, mean = input$mean2,sd = input$sd2)
        # draw the qqplot of the two normals
        qqplot(x,y)
        abline(0,1,col=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)