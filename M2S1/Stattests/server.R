
library(shiny)

shinyServer(function(input, output) {
    
    # Return the requested dataset ----
    # Note that we use eventReactive() here, which depends on
    # input$update (the action button), so that the output is only
    # updated when the user clicks the button
    datasetInput <- eventReactive(input$update, {
        switch(input$dataset,
               "rock" = rock,
               "pressure" = pressure,
               "cars" = cars)
    }, ignoreNULL = FALSE)
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    # The use of isolate() is necessary because we don't want the table
    # to update whenever input$obs changes (only when the user clicks
    # the action button)
    output$view <- renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })
    
    #pearson 
    output$pearsonplot <- renderPlot({
        if (input$illustration=="gamma"){
            par(mar=c(2.1,2.1,0.1,0.1))
            shp <- input$gamma.shp
            rt <- input$gamma.rt
            x <- seq(0,b*(a+ncp)/(a*max(b-2,1))*4,length.out=500)
            plot(x,pbeta(x,a,b,ncp),type="l",xlab="",ylab="")
        }
    })
    
    #spearman 
    output$spearmanplot <- renderPlot({
        if (input$illustration1=="mvnorm"){
            library(mvtnorm)
            rho <- input$mvnorm.rho
            sd1 <- input$mvnorm.sd1
            sd2 <- input$mvnorm.sd2
            x <- y <- seq(-3,3,by=0.01);
            Sigma <- diag(c(sd1,sd2))%*% matrix(c(1,rho,rho,1),nrow=2)%*% diag(c(sd1,sd2))
            par(mar=c(2.1,2.1,0.1,0.1))
            contour(x,y,outer(x,y,FUN=function(x,y) dmvnorm(cbind(x,y),sigma=Sigma)))
        }
    })
    
    #chisqplot
    
    output$chisqplot <- renderPlot({
        
        
    })
})
