library(shiny)

shinyServer(function(input, output) {
  
  output$cdf <- renderPlot({
    if (input$illustration=="mvnorm"){
      library(mvtnorm)
      rho <- input$mvnorm.rho
      sd1 <- input$mvnorm.sd1
      sd2 <- input$mvnorm.sd2
      x <- y <- seq(-3,3,by=0.01);
      Sigma <- diag(c(sd1,sd2))%*% matrix(c(1,rho,rho,1),nrow=2)%*% diag(c(sd1,sd2))
      par(mar=c(2.1,2.1,0.1,0.1))
      contour(x,y,outer(x,y,FUN=function(x,y) dmvnorm(cbind(x,y),sigma=Sigma)))
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
    if (input$illustration=="F"){
      par(mar=c(2.1,2.1,0.1,0.1))
      df1 <- input$F.df1
      df2 <- input$F.df2
      ncp <- input$F.ncp
      x <- seq(0,df2*(df1+ncp)/(df1*max(df2-2,1))*4,length.out=500)
      if (input$Fplottype=="pdf")
        plot(x,df(x,df1=df1,df2=df2,ncp=ncp),type="l",xlab="",ylab="")
      if (input$Fplottype=="cdf")
        plot(x,pf(x,df1=df1,df2=df2,ncp=ncp),type="l",xlab="",ylab="")
    }
  })
})