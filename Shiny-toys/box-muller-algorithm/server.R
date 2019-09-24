#this is based on the box-muller algorithm from page 249 of casella and berger's statistical inference (second edition)


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
            par(mar=c(2.1,2.1,0.1,0.1))
            minmax <- input$unif.minmax
            x <- seq(-20,20,length.out=500)
            #performing the transforms
            u1 <- dunif(x,0,1)
            u2 <- dunif(x,0,1)
            theta <- 2*pi*u1
            #base e!
            R <- sqrt(-2*log10(u2))
            u1Trans <- R*cos(theta)
            u2Trans <- R*sin(theta)
            #plot(x,dunif(x,0,1),type="l",xlab="",ylab="")
            #first transform
            plot(x,u1,type="l",xlab="",ylab="")
            #second transform
            #plot(x,dunif(x,0,1),type="l",xlab="",ylab="")

        

    })

})
