
library(shiny)

shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        n <- 200
        maxx <- 5
        minx <- -maxx
        x <- seq(from=minx, to=maxx, length=n)
        
        if (input$illustration=="poi"){
            lam1 <- input$poi.lam1
            #lam2 <- input$poi.lam2
            plot(x,ppois(0.5,lam1),type="l",xlab="",ylab="")
            #plot(x,ppois(n,lam2),type="l",xlab="",ylab="")
        }
        
        if (input$illustration=="norm"){
            par(mar=c(2.1,2.1,0.1,0.1))
            mu <- input$norm.mu
            sd <- input$norm.sd
            plot(x,dnorm(x,mu,sd),type="l",xlab="",ylab="")
        }
        
        
    })
})





