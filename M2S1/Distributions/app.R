#-----------------------------------------------

library(shiny)
library(ggplot2)
library(shinythemes)
library(EnvStats)
#-----------------------------------------------

ui <- navbarPage("Distributions in M2S1",
                   #don't quite understand what the point of this is
                   #includeHTML("navbar.html"),
                   
                   #changes the theme of the webpage
                   theme = shinythemes::shinytheme("cerulean"),
                   
                   #title
                   #headerPanel("Illustrations in M2S1"),
                   
                   tabPanel("Univariate Families",
                            sidebarPanel(
                                selectInput("illustration", "Choose Distribution:",
                                            #the options for the distributions to choose between
                                            choices = c("Uniform" = "unif", "Exponential" = "exp", "Gamma" = "gamma", "Weibull" = "wei" , "Normal" = "norm" , "student-t"="tdistr", "Pareto" = "par" ,"\\(\\beta\\)"="b"  , "\\(\\chi^2\\)"="chisq", "F" = "f","Cauchy" = "cau")),
                                #gamma
                                conditionalPanel(
                                    condition="input.illustration==\"gamma\"",
                                    sliderInput("gamma.shp","Shape \\(n\\)",min=1,max=100,value=10,step=1),
                                    sliderInput("gamma.rt","Rate \\(\\delta^2\\)",min=0,max=50,value=5,step=0.1),
                                    radioButtons("gamma.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #multivariable normal
                                conditionalPanel(
                                    condition="input.illustration==\"mvnorm\"",
                                    sliderInput("mvnorm.sd1","\\(\\sigma_1\\)",min=0.1,max=10,value=1,step=0.1),
                                    sliderInput("mvnorm.sd2","\\(\\sigma_2\\)",min=0.1,max=10,value=1,step=0.1),
                                    sliderInput("mvnorm.rho","\\(\\rho\\)",min=-0.99,max=0.99,value=0,step=0.01)
                                ),
                                #t distribution
                                conditionalPanel(
                                    condition="input.illustration==\"tdistr\"",
                                    sliderInput("tdistr.df","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
                                    sliderInput("tdistr.ncp","non-centrality parameter \\(\\delta\\)",min=-3,max=3,value=0,step=0.1),
                                    radioButtons("tdistr.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #chi-square dist
                                conditionalPanel(
                                    condition="input.illustration==\"chisq\"",
                                    sliderInput("chisq.df","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
                                    sliderInput("chisq.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1),
                                    radioButtons("chisqplottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #dirichlet distribution
                                conditionalPanel(
                                    condition="input.illustration==\"dir\"",
                                    sliderInput("dir.df","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
                                    sliderInput("dir.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1),
                                    radioButtons("dirplottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #beta distribution
                                conditionalPanel(
                                    condition="input.illustration==\"b\"",
                                    sliderInput("b.a",withMathJax(helpText("Shape parameter : \\(\\alpha\\)")),min=0,max=50,value=1,step=1),
                                    sliderInput("b.b",withMathJax(helpText("Shape parameter : \\(\\beta\\)")),min=0,max=50,value=1,step=1),
                                    sliderInput("b.ncp","Non-centrality parameter : \\(\\delta^2\\)",min=0,max=0.99,value=0,step=0.01),
                                    radioButtons("bplottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #uniform distribution
                                conditionalPanel(
                                    condition="input.illustration==\"unif\"",
                                    sliderInput("unif.n",withMathJax(helpText("Number of observations : \\(n\\)")),min=1,max=100,value=5,step=1),
                                    sliderInput("unif.minmax",withMathJax(helpText("Lower and upper bounds : \\(minmax\\)")),min = -20, max = 20,value = c(-15,15)),
                                    radioButtons("unif.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #F distribution
                                conditionalPanel(
                                    condition="input.illustration==\"f\"",
                                    sliderInput("f.df1",withMathJax(helpText("Degrees of Freedom : \\(d_1\\)")),min=0,max=10,value=1,step=1),
                                    sliderInput("f.df2",withMathJax(helpText("Degrees of Freedom : \\(d_2\\)")),min=0,max=10,value=1,step=1),
                                    sliderInput("f.ncp","Non-centrality parameter : \\(\\delta^2\\)",min=0,max=0.99,value=0,step=0.01),
                                    radioButtons("f.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #Weibull distribution
                                conditionalPanel(
                                    condition="input.illustration==\"wei\"",
                                    sliderInput("wei.shp",withMathJax(helpText("Shape : \\(\\lambda\\)")),min=0,max=10,value=1,step=1),
                                    sliderInput("wei.scl",withMathJax(helpText("Scale : \\(k\\)")),min=0,max=10,value=1,step=1),
                                    radioButtons("wei.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #Cauchy distribution
                                conditionalPanel(
                                    condition="input.illustration==\"cau\"",
                                    sliderInput("cau.loc",withMathJax(helpText("Location : \\(x_0\\)")),min=-10,max=10,value=1,step=1),
                                    sliderInput("cau.scl",withMathJax(helpText("Scale : \\(\\gamma\\)")),min=0,max=10,value=1,step=1),
                                    radioButtons("cau.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #Exponential distribution
                                conditionalPanel(
                                    condition="input.illustration==\"exp\"",
                                    sliderInput("exp.rate",withMathJax(helpText("Rate : \\(\\lambda\\)")),min=0,max=10,value=1,step=1),
                                    radioButtons("log","Log:",c("True" = "t","False"="f")),
                                    radioButtons("exp.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #pareto distribution
                                conditionalPanel(
                                    condition="input.illustration==\"par\"",
                                    sliderInput("par.loc",withMathJax(helpText("Location : \\(\\mu\\)")),min=0,max=50,value=1,step=1),
                                    sliderInput("par.shp",withMathJax(helpText("Scale : \\(\\sigma\\)")),min=0,max=50,value=1,step=1),
                                    radioButtons("par.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #normal distribution
                                conditionalPanel(
                                    condition="input.illustration==\"norm\"",
                                    sliderInput("norm.mean",withMathJax(helpText("Mean : \\(\\mu\\)")),min=-100,max=100,value=1,step=1),
                                    sliderInput("norm.sd",withMathJax(helpText("Standard Deviation : \\(\\sigma\\)")),min=0,max=100,value=1,step=1),
                                    radioButtons("norm.plottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                )
                                
                                
                            ),
                            
                            
                            #text outputs to accompany the plots 
                            mainPanel(
                                plotOutput("cdf"),
                                #multivar-norm
                                conditionalPanel(
                                    condition="input.illustration==\"mvnorm\"",
                                    helpText("multivariable"),
                                    helpText("Contour plot of the pdf of N(0,\\(\\Sigma\\))"),
                                    helpText("where \\(\\Sigma=\\begin{pmatrix}\\sigma_1^2&\\rho\\sigma_1\\sigma_2\\\\\\rho\\sigma_1\\sigma_2&\\sigma_2^2\\end{pmatrix}\\)")
                                ),
                                #t distribution
                                conditionalPanel(
                                    condition="input.illustration==\"tdistr\"",
                                    h3(strong("Mean:"))
                                ),
                                #cauchy distribution
                                conditionalPanel(
                                    condition="input.illustration==\"cau\"",
                                    p("The Cauchy distribution has the interesting property that all of the higher moments are undefined."),
                                    h3(strong("Mean:"))
                                ),
                                #norm distribution
                                conditionalPanel(
                                    condition="input.illustration==\"norm\"",
                                    p("The normal distributions"),
                                    h3(strong("Mean:"))
                                ),
                                #gamma distribution
                                conditionalPanel(
                                    condition="input.illustration==\"gamma\"",
                                    p("The Gamma distribution is"),
                                    hr(),
                                    p("Here's some other paragraph to see what the horizontal lines do")
                                ),
                                #exp distribution
                                conditionalPanel(
                                    condition="input.illustration==\"exp\"",
                                    p("The exponential distribution only has one parameter")
                                ),
                                #weibull distribution
                                conditionalPanel(
                                    condition="input.illustration==\"wei\"",
                                    p("The Weibull distribution is")
                                ),
                                #chi-square dist
                                conditionalPanel(
                                    condition="input.illustration==\"chisq\"",
                                    p("The chi-square distribution is related to other distributions in the following way")
                                ),
                                #dirichlet distribution
                                conditionalPanel(
                                    condition="input.illustration==\"dir\"",
                                    p("The Dirichlet distribution is an example of a multivariate distribution")
                                ),
                                #uniform distribution
                                conditionalPanel(
                                    condition="input.illustration==\"unif\"",
                                    p("The uniform distribution is a very simple distribution."),
                                    strong("Mean: "),
                                    strong("Variance:")
                                ),
                                #pareto distribution
                                conditionalPanel(
                                    condition="input.illustration==\"par\"",
                                    p("The Pareto distribution, coined by Italian economist Vilfredo Pareto")
                                ),
                                #F distribution
                                conditionalPanel(
                                    condition="input.illustration==\"f\"",
                                    p("The F distribution is a distribution less often discussed in M2S1")
                                ),
                                #beta distribution
                                conditionalPanel(
                                    condition="input.illustration==\"b\"",
                                    p("The beta distribution is part of a family")
                                )
                            )
                            
                   ),
                   tabPanel("Multivariate Families", 
                            
                            sidebarPanel(
                                selectInput("illustration1", "Choose Distribution:",
                                            #
                                            choices = c("Dirichlet" = "dir", "Bivariate Normal"="mvnorm")),
                                #dirichlet distribution
                                conditionalPanel(
                                    condition="input.illustration1==\"dir\"",
                                    sliderInput("dir.df","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
                                    sliderInput("dir.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1),
                                    radioButtons("dirplottype", "Choose the plot",
                                                 list("cdf" = "cdf","pdf"="pdf"))
                                ),
                                #multivariable normal
                                conditionalPanel(
                                    condition="input.illustration1==\"mvnorm\"",
                                    sliderInput("mvnorm.sd1","\\(\\sigma_1\\)",min=0.1,max=10,value=1,step=0.1),
                                    sliderInput("mvnorm.sd2","\\(\\sigma_2\\)",min=0.1,max=10,value=1,step=0.1),
                                    sliderInput("mvnorm.rho","\\(\\rho\\)",min=-0.99,max=0.99,value=0,step=0.01)
                                )
                                
                            ),
                            
                            mainPanel(
                                plotOutput("mvcdf"),
                                #multivar-norm
                                conditionalPanel(
                                    condition="input.illustration1==\"mvnorm\"",
                                    helpText("multivariable"),
                                    helpText("Contour plot of the pdf of N(0,\\(\\Sigma\\))"),
                                    helpText("where \\(\\Sigma=\\begin{pmatrix}\\sigma_1^2&\\rho\\sigma_1\\sigma_2\\\\\\rho\\sigma_1\\sigma_2&\\sigma_2^2\\end{pmatrix}\\)")
                                ),
                                #dirichlet distribution
                                conditionalPanel(
                                    condition="input.illustration1==\"dir\"",
                                    helpText("\\(\\chi^2_n(\\delta)\\) distribution"),
                                    helpText("f")
                                )
                            )
                   ),
                   tabPanel("Location and Scale Families", 
                            sidebarPanel(
                                selectInput("lsfam","Choose distribution:", choices = c("binomial" = "bin","poisson"="poi"))
                            ),
                            conditionalPanel(
                                condition="input.lsfam==\"bin\"",
                                helpText("blah")
                            )
                            
                   )
                   
                   #includeHTML("mycode.html")
)




server <- function(input, output) {
    
    #univariate dists
    #gamma
    output$cdf <- renderPlot({
        if (input$illustration=="gamma"){
            par(mar=c(2.1,2.1,0.1,0.1))
            shp <- input$gamma.shp
            rt <- input$gamma.rt
            x <- seq(0,10,length.out=500)
            if (input$gamma.plottype=="pdf"){
                plot(x,dgamma(x,shp,rt),type="l",ylim=c(0,dnorm(0)),xlab="",ylab="")
                #lines(x,dnorm(x,mean=ncp),col="blue")
                legend("topright",expression(t[n](delta),N(delta,1)),title="pdf of",col=c("black","blue"),lty=1)
            }
            if (input$gamma.plottype=="cdf"){
                plot(x,pgamma(x,shp,rt),type="l",ylim=c(0,1),xlab="",ylab="")
                #lines(x,pnorm(x,mean=ncp),col="blue")
                legend("topleft",expression(t[n](delta),N(delta,1)),title="cdf of",col=c("black","blue"),lty=1)
            }
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
            if (input$unif.plottype=="pdf"){
                plot(x,dunif(x,minmax[1],minmax[2]),type="l",xlab="",ylab="")}
            if (input$unif.plottype=="cdf"){
                plot(x,punif(x,minmax[1],minmax[2]),type="l",xlab="",ylab="")}
        }
        #F dist
        if (input$illustration=="f"){
            par(mar=c(2.1,2.1,0.1,0.1))
            df1 <- input$f.df1
            df2 <- input$f.df2
            ncp <- input$f.ncp
            x <- seq(0,10,length.out=500)
            
            if (input$f.plottype=="pdf"){
                plot(x,df(x,df1,df2,ncp),type="l",xlab="",ylab="")}
            if (input$f.plottype=="cdf"){
                plot(x,pf(x,df1,df2,ncp),type="l",xlab="",ylab="")}
        }
        #weibull dist
        if (input$illustration=="wei"){
            par(mar=c(2.1,2.1,0.1,0.1))
            shp <- input$wei.shp
            scl <- input$wei.scl
            x <- seq(0,10,length.out=500)
            
            if (input$wei.plottype=="pdf"){
                plot(x,dweibull(x,shp,scl),type="l",xlab="",ylab="")}
            if (input$wei.plottype=="cdf"){
                plot(x,pweibull(x,shp,scl),type="l",xlab="",ylab="")}
        }
        #cauchy dist
        if (input$illustration=="cau"){
            par(mar=c(2.1,2.1,0.1,0.1))
            loc <- input$cau.loc
            scl <- input$cau.scl
            x <- seq(0,10,length.out=500)
            
            if (input$cau.plottype=="pdf"){
                plot(x,dcauchy(x,loc,scl),type="l",xlab="",ylab="")}
            if (input$cau.plottype=="cdf"){
                plot(x,pcauchy(x,loc,scl),type="l",xlab="",ylab="")}
        }
        #exponential dist
        if (input$illustration=="exp"){
            par(mar=c(2.1,2.1,0.1,0.1))
            rate <- input$exp.rate
            x <- seq(0,10,length.out=500)
            
            if (input$exp.plottype=="pdf"){
                plot(x,dexp(x,rate),type="l",xlab="",ylab="")}
            if (input$exp.plottype=="cdf"){
                plot(x,pexp(x,rate),type="l",xlab="",ylab="")}
        }
        #normal dist
        if (input$illustration=="norm"){
            par(mar=c(2.1,2.1,0.1,0.1))
            mean <- input$norm.mean
            sd <- input$norm.sd
            x <- seq(0,10,length.out=500)
            
            if (input$norm.plottype=="pdf"){
                plot(x,dnorm(x,mean,sd),type="l",xlab="",ylab="")}
            if (input$norm.plottype=="cdf"){
                plot(x,pnorm(x,mean,sd),type="l",xlab="",ylab="")}
            
        }
        #pareto dist
        if (input$illustration=="par"){
            par(mar=c(2.1,2.1,0.1,0.1))
            loc <- input$par.loc
            shp <- input$par.shp
            x <- seq(0,10,length.out=500)
            
            if (input$par.plottype=="pdf"){
                plot(x,dpareto(x,loc,shp),type="l",xlab="",ylab="")}
            if (input$par.plottype=="cdf"){
                plot(x,ppareto(x,loc,shp),type="l",xlab="",ylab="")}
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
}

shinyApp(ui = ui, server = server)




