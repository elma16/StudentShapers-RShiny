#library
#-----------------------

library(shiny)


#-----------------------
#UI
ui <- fluidPage(
    
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

)
#SERVER
#-----------------------
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        n <- 200
        maxx <- 5
        minx <- -maxx
        x <- seq(from=minx, to=maxx, length=n)
        
        if (input$illustration=="poi"){
            lam1 <- input$poi.lam1
            lam2 <- input$poi.lam2
            plot(x,qpois(n,lam1),type="l",xlab="",ylab="")
            #plot(x,ppois(n,lam2),type="l",xlab="",ylab="")
        }
        
        if (input$illustration=="norm"){
            par(mar=c(2.1,2.1,0.1,0.1))
            mu <- input$norm.mu
            sd <- input$norm.sd
            plot(x,dnorm(x,mu,sd),type="l",xlab="",ylab="")
        }
        
        
    })
}


#-----------------------
# Run the application 
shinyApp(ui = ui, server = server)
