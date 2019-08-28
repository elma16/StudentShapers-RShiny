#


library(shiny)

shinyServer(function(input, output) {
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
})