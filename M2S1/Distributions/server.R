#-------------
library(shiny)
library(shinythemes)
library(mvtnorm)
library(EnvStats)
#-------------

shinyServer(function(input, output) {
  
  #univariate dists
  #gamma
  output$cdf <- renderPlot({
    if (input$illustration=="gamma"){
      par(mar=c(2.1,2.1,0.1,0.1))
      shp <- input$gamma.shp
      rt <- input$gamma.rt
      x <- seq(0,10,length.out=500)
      plot(x,dgamma(x,shp,rt),type="l",xlab="",ylab="")
    }
  #t-distribution
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
    #chi sq dist
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
    #beta dist
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
    #uniform dist
    if (input$illustration=="unif"){
      par(mar=c(2.1,2.1,0.1,0.1))
      n <- input$unif.n
      minmax <- input$unif.minmax
      x <- seq(-20,20,length.out=500)
      plot(x,dunif(x,minmax[1],minmax[2]),type="l",xlab="",ylab="")
    }
    #F dist
    if (input$illustration=="f"){
      par(mar=c(2.1,2.1,0.1,0.1))
      df1 <- input$f.df1
      df2 <- input$f.df2
      ncp <- input$f.ncp
      x <- seq(0,10,length.out=500)
      plot(x,df(x,df1,df2,ncp),type="l",xlab="",ylab="")
    }
    #weibull dist
    if (input$illustration=="wei"){
      par(mar=c(2.1,2.1,0.1,0.1))
      shp <- input$wei.shp
      scl <- input$wei.scl
      x <- seq(0,10,length.out=500)
      plot(x,dweibull(x,shp,scl),type="l",xlab="",ylab="")
    }
    #cauchy dist
    if (input$illustration=="cau"){
      par(mar=c(2.1,2.1,0.1,0.1))
      loc <- input$cau.loc
      scl <- input$cau.scl
      x <- seq(0,10,length.out=500)
      plot(x,dcauchy(x,loc,scl),type="l",xlab="",ylab="")
    }
    #exponential dist
    if (input$illustration=="exp"){
      par(mar=c(2.1,2.1,0.1,0.1))
      rate <- input$exp.rate
      x <- seq(0,10,length.out=500)
      plot(x,dexp(x,rate,),type="l",xlab="",ylab="")
    }
    #normal dist
    if (input$illustration=="norm"){
      par(mar=c(2.1,2.1,0.1,0.1))
      mean <- input$norm.mean
      sd <- input$norm.sd
      x <- seq(0,10,length.out=500)
      plot(x,dnorm(x,mean,sd),type="l",xlab="",ylab="")
    }
    #pareto dist
    if (input$illustration=="par"){
      par(mar=c(2.1,2.1,0.1,0.1))
      loc <- input$par.loc
      shp <- input$par.shp
      x <- seq(0,10,length.out=500)
      plot(x,dpareto(x,loc,shp),type="l",xlab="",ylab="")
    }
  })
  #multivariable normal
  output$mvcdf <- renderPlot({
    if (input$illustration1=="mvnorm"){
      rho <- input$mvnorm.rho
      sd1 <- input$mvnorm.sd1
      sd2 <- input$mvnorm.sd2
      x <- y <- seq(-3,3,by=0.01);
      Sigma <- diag(c(sd1,sd2))%*% matrix(c(1,rho,rho,1),nrow=2)%*% diag(c(sd1,sd2))
      par(mar=c(2.1,2.1,0.1,0.1))
      contour(x,y,outer(x,y,FUN=function(x,y) dmvnorm(cbind(x,y),sigma=Sigma)))
    }
    #dirichlet distribution
    if (input$illustration1=="dir"){
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