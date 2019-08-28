

library(shiny)

shinyUI(bootstrapPage(
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
))
