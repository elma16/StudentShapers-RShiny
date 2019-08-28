
library(shiny)

# Define UI for application that draws a histogram

shinyUI(fluidPage(
    
    # title
    titlePanel("Convolutions"),
    
    # plot output 
    plotOutput(outputId = "distPlot"),
    
    # dividing line
    hr(),
    
    fluidRow(
        column(3,
               h4("Random variables"),
               
               # Slider input 
               selectInput("illustration", "Choose Distribution:",choices = c(" " = " ", "Poisson" = "poi","Normal"="norm")),
               selectInput("number","Choose the number of random variables",choices = c("2" = "2"))
        ),
        
        column(4, offset=1,
               h4("Choose the parameters"),
               #multivariable normal
               conditionalPanel(
                   condition="input.illustration==\"norm\"",
                   sliderInput("norm.mu1",withMathJax("\\(\\mu_1\\)"),min=0.1,max=10,value=1,step=0.1),
                   sliderInput("norm.sd1","\\(\\sigma_1\\)",min=0.1,max=10,value=1,step=0.1),
                   sliderInput("norm.mu2","\\(\\mu_2\\)",min=0.1,max=10,value=1,step=0.1),
                   sliderInput("norm.sd2","\\(\\sigma_2\\)",min=0.1,max=10,value=1,step=0.1)
               ),
               #poisson distribution
               conditionalPanel(
                   condition="input.illustration==\"poi\"",
                   sliderInput("poi.lam1",withMathJax("\\(\\lambda_1\\)"),min=1,max=5,value=1,step=1),
                   sliderInput("poi.lam2","\\(\\lambda_2\\)",min=1,max=5,value=1,step=1)
               )
        ),
        column(3, offset = 1,
               #can i do this
               conditionalPanel(
                   condition = "input.illustration==\" \"",
                   p("This is an illustrative web app to show visually how convolutions work.")
               ),
               conditionalPanel(
                   condition = "input.illustration==\"poi\"",
                   p("For discrete distributions, like the Poisson distribution, we have the following formula")
               ),
               conditionalPanel(
                   condition = "input.illustration==\"norm\"",
                   p("For continuous random variables, such as the normal distribution, we have the following convolution formula")
               )
               
               
        )
    )
    
))

