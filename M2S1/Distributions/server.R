library(shiny)
library(shinythemes)

shinyServer(function(input, output) {
  
  #univariate dists
  output$cdf <- renderPlot({
    if (input$illustration=="gamma"){
      par(mar=c(2.1,2.1,0.1,0.1))
      shp <- input$gamma.shp
      rt <- input$gamma.rt
      x <- seq(0,b*(a+ncp)/(a*max(b-2,1))*4,length.out=500)
        plot(x,pbeta(x,a,b,ncp),type="l",xlab="",ylab="")
    }

    if (input$illustration=="tdistr"){
      par(mar=c(2.1,2.1,0.1,0.1))
      df <- input$tdistr.df
      ncp <- input$tdistr.ncp
      x <- seq(-3,3,by=0.02)
      if (input$tdistr.plottype=="pdf"){
        plot(x,dt(x,df=df,ncp=ncp),type="l",ylim=c(0,dnorm(0)),xlab="",ylab="")
        lines(x,dnorm(x,mean=ncp),col="blue")
        legend("topright",expression(t[n](delta),N(delta,1)),title="pdf of",col=c("black","blue"),lty=1)
      }
      if (input$tdistr.plottype=="cdf"){
        plot(x,pt(x,df=df,ncp=ncp),type="l",ylim=c(0,1),xlab="",ylab="")
        lines(x,pnorm(x,mean=ncp),col="blue")
        legend("topleft",expression(t[n](delta),N(delta,1)),title="cdf of",col=c("black","blue"),lty=1)
      }
      
    }
    if (input$illustration=="chisq"){
      par(mar=c(2.1,2.1,0.1,0.1))
      df <- input$chisq.df
      ncp <- input$chisq.ncp
      x <- seq(0,(df+ncp)*1.5,length.out=500)
      if (input$chisqplottype=="pdf")
        plot(x,dchisq(x,df=df,ncp=ncp),type="l",xlab="",ylab="")
      if (input$chisqplottype=="cdf")
        plot(x,pchisq(x,df=df,ncp=ncp),type="l",xlab="",ylab="")
    }
    if (input$illustration=="b"){
      par(mar=c(2.1,2.1,0.1,0.1))
      a <- input$b.a
      b <- input$b.b
      ncp <- input$b.ncp
      x <- seq(0,b*(a+ncp)/(a*max(b-2,1))*4,length.out=500)
      if (input$bplottype=="pdf")
        plot(x,dbeta(x,a,b,ncp),type="l",xlab="",ylab="")
      if (input$bplottype=="cdf")
        plot(x,pbeta(x,a,b,ncp),type="l",xlab="",ylab="")
    }
  })
  output$mvcdf <- renderPlot({
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
    if (input$illustration1=="dir"){
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
})