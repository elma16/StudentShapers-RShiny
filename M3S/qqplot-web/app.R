# all functions in one file, need to load shiny at the beginning.
library (shiny)

# ui.R
ui <- fluidPage(
    # title
    titlePanel("Q-Q Plots: A walkthrough"),
    
    # plot 
    plotOutput(outputId = "distPlot"),
    
    # dividing line
    hr(),
    
    fluidRow(
        column(3,
               h4("Move the slider to change the graph!"),
               
               # Slider input 
               sliderInput("mean","Mean",min = 1, max = 100, step = 1),
               sliderInput("sd","Standard Deviation",min = 1, max = 100, step = 1),
               sliderInput("skw","Skew",min = 1, max = 100, step = 1),
               sliderInput("tls","Heavy Tails",min = 1, max = 100, step = 1),
               sliderInput("size","Sample size",min = 1, max = 100, step = 1)
        ),
        
        column(4, offset=1,
               
               # Maths equations
               withMathJax(),
               uiOutput('tdisteq')
               
        )
        
    )
    
)


# server.R
server <- function(input, output, session) {
    output$distPlot <- renderPlot({
        n <- 200
        maxx <- 5
        minx <- -maxx
        x <- seq(from=minx, to=maxx, length=n)
        
        # first plot standard normal
        mu <- 0
        sigma <- 1
        yn <- dnorm(x, mean=mu, sd=sigma)
        col_norm <- "blue"
        lty_norm <- "dashed"
        label_x <- "Value x"
        label_y <- "Density at x"
        lwd_norm <- 2
        plot(x, yn, 
             type='l', 
             col=col_norm, 
             lty=lty_norm, 
             lwd=lwd_norm,
             xlab=label_x,
             ylab=label_y
        )
        
        
        # t-distribution plot
        yt <- dt(x, df=input$dof)
        col_t <- "black"
        lty_t <- "solid"
        lwd_t <- lwd_norm
        lines(x, yt, type='l', col=col_t, lty=lty_t, lwd=lwd_t)
        
        # add a legend
        leg_normal <- "Std. Normal"
        
        # To get spacing right
        dofstring <- as.character(input$dof)
        if (input$dof < 10){
            dofstring <- paste0(" ", dofstring, " ")
        }
        # To get the spacing consistent for non-integer and integer values
        isInt <- ( (input$dof %% 1) == 0 )
        if (isInt){
            dofstring <- paste0(dofstring, "   ")
        }
        
        leg_t <- paste0("t-distribution with d.o.f. = ", dofstring)
        leg_x <- maxx - 2.2
        leg_y <- max(yn) -0.01
        cexval <- 1.0
        leg_title <- expression(paste(bold("Distributions")))
        legend_text <- c(leg_normal, leg_t)
        op <- par(cex = 1.4)
        legend(leg_x, leg_y, 
               legend=legend_text,
               col=c(col_norm, col_t), 
               lty=c(lty_norm, lty_t),
               lwd=c(lwd_norm, lwd_t),
               title=leg_title,
               cex=cexval
        )
    })
    
    # Some equations for probability density functions
    output$tdisteq <- renderUI({
        withMathJax(
            helpText('The probability density function for the t-distribution with degrees of freedom \\(\\nu > 0\\)  is $$ \\frac{\\Gamma \\left ( \\frac{\\nu + 1}{2} \\right)}{\\sqrt{ \\nu \\pi } \\Gamma ( \\frac{\\nu}{2} ) } \\left( 1 + \\frac{x^2}{\\nu} \\right)^{ - \\frac{ \\nu + 1}{2}  }  , $$'),
            helpText('and the probability density function for the standard normal distribution with mean \\( \\mu=0 \\) and variance \\( \\sigma^2=1\\) is $$ \\frac{1}{ \\sqrt{2 \\pi } }  e^{   - \\frac{x^2}{2 }   } . $$')
        )
    })
    
}

shinyApp(ui=ui, server=server)
