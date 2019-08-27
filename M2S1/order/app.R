library(shiny)

#---------------------- 
ui <- bootstrapPage(
    headerPanel("Order Statistics"),
    sidebarPanel(
        selectInput("illustration", "Choose Distribution:",choices = c("Binomial" = "binom", "Poisson" = "poi","Normal"="norm")),
        numericInput("rstat", "rth order statistics:", 1, min = 1, max = 100),
        
        conditionalPanel(
            condition="input.illustration==\"binom\"",
            sliderInput("binom.n","Number of attempts",min=1,max=100,value=10),
            sliderInput("binom.p","Success probability p",min=0,max=1,value=0.5),
            sliderInput("binom.size","Size",min = 1,max = 10,value=1),
            sliderInput("binom.seed","Seed for Random Numbers",min=1,max=5000,value=1231,step=1)
        ),
        
        conditionalPanel(
            condition="input.illustration==\"poi\"",
            sliderInput("poi.n","Number of attempts",min=1,max=100,value=10),
            sliderInput("poi.lam",withMathJax("\\(\\lambda\\)"),min=0.1,max=10,value=1,step=0.1)
        ),
        
        conditionalPanel(
            condition="input.illustration==\"norm\"",
            sliderInput("norm.n",withMathJax("\\(n\\)"),min=1,max=100,value=1,step=1),
            sliderInput("norm.mean","\\(\\mu\\)",min=0.1,max=10,value=1,step=0.1),
            sliderInput("norm.sd","\\(\\sigma\\)",min=0.1,max=10,value=1,step=0.1)
        )
        
    ),
    mainPanel(verbatimTextOutput("observations"),plotOutput("likelihood"))
)


#------------------------------------
server <- function(input, output) {
    output$observations <- renderText({
        
        if(input$illustration=="binom"){
            n <- input$binom.n
            p <- input$binom.p
            size <- input$binom.size
            set.seed(input$binom.seed)
            X <- rbinom(n,size,p)
            Xsorted <- sort(X,decreasing = FALSE)
            Out <- paste("Observations:",
                         paste(X,collapse=", "),
                         "Ordered Observations:",paste(Xsorted, collapse =", "),
                         sep="\n")
        }
        
        if(input$illustration=="poi"){
            n <- input$poi.n
            lam <- input$poi.lam
            Y <- rpois(n,lam)
            Ysorted <- sort(Y,decreasing = FALSE)
            Out <- paste("Observations:",
                         paste(Y,collapse=", "),
                         "Ordered Observations:",paste(Ysorted, collapse =", "),
                         sep="\n")
        }
        
        if(input$illustration=="norm"){
            n <- input$norm.n
            mean <- input$norm.mean
            sd <- input$norm.sd
            Z <- rnorm(n,mean,sd)
            Zsorted <- sort(Z,decreasing = FALSE)
            Out <- paste("Observations:",
                         paste(Z,collapse=", "),
                         "Ordered Observations:",paste(Zsorted, collapse =", "),
                         sep="\n")
        }
        Out
    })
    
    output$likelihood <- renderPlot({
        
        
    })
}


shinyApp(ui = ui, server = server)
