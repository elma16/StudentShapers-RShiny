
library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
    
    output$qqplot <- renderPlot({
        # normal dists given the values from sliders
        
        x <- rnorm(n = input$n1, mean = input$mean1,sd = input$sd1)
        y <- rnorm(n = input$n2, mean = input$mean2,sd = input$sd2)
        # draw the qqplot of the two normals
        qqplot(x,y)
        abline(0,1,col=2)
    })
})