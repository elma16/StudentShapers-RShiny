#---------
library(shiny)
library(ggplot2)
#---------

shinyServer(function(input, output) {
    
    output$qqplot <- renderPlot({
        if (input$pdf1=="norm1" & input$pdf2=="norm2"){
            n1 <- input$norm1.n
            mean1 <- input$norm1.mean
            sd1 <- input$norm1.sd
            x <- rnorm(n1,mean1,sd1)
            n2 <- input$norm2.n
            mean2 <- input$norm2.mean
            sd2 <- input$norm2.sd
            y <- rnorm(n2,mean2,sd2)
            qqplot(x,y)
            abline(-1,1,col=2)
        }
        if (input$pdf1=="norm1" & input$pdf2=="gam2"){
            n1 <- input$norm1.n
            mean1 <- input$norm1.mean
            sd1 <- input$norm1.sd
            x <- rnorm(n1,mean1,sd1)
            n2 <- input$gam2.n
            mean2 <- input$gam2.shp
            sd2 <- input$gam2.rt
            y <- rgamma(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="norm1" & input$pdf2=="stu2"){
            n1 <- input$norm1.n
            mean1 <- input$norm1.mean
            sd1 <- input$norm1.sd
            x <- rnorm(n1,mean1,sd1)
            n2 <- input$stu2.n
            mean2 <- input$stu2.df
            sd2 <- input$stu2.ncp
            y <- rt(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="gam1" & input$pdf2=="norm2"){
            n1 <- input$gam1.n
            mean1 <- input$gam1.shp
            sd1 <- input$gam1.rt
            x <- rgamma(n1,mean1,sd1)
            n2 <- input$norm2.n
            mean2 <- input$norm2.mean
            sd2 <- input$norm2.sd
            y <- rnorm(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="gam1" & input$pdf2=="gam2"){
            n1 <- input$gam1.n
            mean1 <- input$gam1.shp
            sd1 <- input$gam1.rt
            x <- rgamma(n1,mean1,sd1)
            n2 <- input$norm2.n
            mean2 <- input$norm2.shp
            sd2 <- input$norm2.rt
            y <- rgamma(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="gam1" & input$pdf2=="stu2"){
            n1 <- input$gam1.n
            mean1 <- input$gam1.shp
            sd1 <- input$gam1.rt
            x <- rgamma(n1,mean1,sd1)
            n2 <- input$stu2.n
            mean2 <- input$stu2.df
            sd2 <- input$stu2.ncp
            y <- rt(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="stu1" & input$pdf2=="norm2"){
            n1 <- input$stu1.n
            mean1 <- input$stu1.df
            sd1 <- input$stu1.ncp
            x <- rt(n1,mean1,sd1)
            n2 <- input$norm2.n
            mean2 <- input$norm2.mean
            sd2 <- input$norm2.sd
            y <- rnorm(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="stu1" & input$pdf2=="gam2"){
            n1 <- input$stu1.n
            mean1 <- input$stu1.df
            sd1 <- input$stu1.ncp
            x <- rt(n1,mean1,sd1)
            n2 <- input$norm2.n
            mean2 <- input$norm2.shp
            sd2 <- input$norm2.rt
            y <- rgamma(n2,mean2,sd2)
            qqplot(x,y)
            abline(0,1,col=2)
        }
        if (input$pdf1=="stu1" & input$pdf2=="stu2"){
            n1 <- input$stu1.n
            mean1 <- input$stu1.df
            sd1 <- input$stu1.ncp
            x <- rt(n1,mean1,sd1)
            n2 <- input$stu2.n
            mean2 <- input$stu2.df
            sd2 <- input$stu2.ncp
            y <- rt(n2,mean2,sd2)
            qqplot(x,y)
            abline(-1,1,col=2)
        }
    })



})