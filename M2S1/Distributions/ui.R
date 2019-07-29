#-----------------------------------------------

library(shiny)
library(ggplot2)
library(shinythemes)
	
#-----------------------------------------------

shinyUI(navbarPage("Distributions in M2S1",
  #don't quite understand what the point of this is
  #includeHTML("navbar.html"),
  
  #changes the theme of the webpage
  theme = shinythemes::shinytheme("cerulean"),
  
  #title
  #headerPanel("Illustrations in M2S1"),
  
tabPanel("Univariate Families",
  sidebarPanel(
    selectInput("illustration", "Choose Distribution:",
                #
                choices = c("Gamma" = "gamma","t-distribution"="tdistr","\\(\\chi^2\\)-distribution"="chisq","\\(\\beta\\)-distribution"="b", "Uniform" = "unif", "F" = "F")),
#gamma
    conditionalPanel(
      condition="input.illustration==\"gamma\"",
      sliderInput("gamma.shp","Shape \\(n\\)",min=1,max=100,value=1,step=1),
      sliderInput("gamma.rt","Rate \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1)
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
      sliderInput("b.a",withMathJax(helpText("Shape parameter : \\(\\alpha\\)")),min=-10,max=10,value=1,step=1),
      sliderInput("b.b",withMathJax(helpText("Shape parameter : \\(\\beta\\)")),min=-10,max=10,value=1,step=1),
      sliderInput("b.ncp","Non-centrality parameter : \\(\\delta^2\\)",min=-0.99,max=0.99,value=0,step=0.01),
      radioButtons("bplottype", "Choose the plot",
                   list("cdf" = "cdf","pdf"="pdf"))
    ),
#uniform distribution
conditionalPanel(
  condition="input.illustration==\"unif\"",
  sliderInput("unif.n",withMathJax(helpText("Number of observations : \\(n\\)")),min=-10,max=10,value=1,step=1),
  sliderInput("unif.minmax",withMathJax(helpText("Lower and upper bounds : \\(min\\)")),value = c(-15,15),min=-10,max=10)
  )

),

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
    helpText("f"),
    helpText("\\(t_n(\\delta)\\) distribution")
  ),
  #chi-square dist
  conditionalPanel(
    condition="input.illustration==\"chisq\"",
    helpText("f"),
    helpText("\\(\\chi^2_n(\\delta)\\) distribution")
  ),
  #dirichlet distribution
  conditionalPanel(
    condition="input.illustration==\"dir\"",
    helpText("\\(\\chi^2_n(\\delta)\\) distribution"),
    helpText("f")
  ),
  #uniform distribution
  conditionalPanel(
    condition="input.illustration==\"unif\"",
    helpText("\\(U(a,b)\\) distribution"),
    helpText("f")
  ),
  #beta distribution
  conditionalPanel(
    condition="input.illustration==\"b\"",
    helpText("\\(beta(\\alpha, \\beta)\\) distribution"),
    helpText("\\(\\frac{x^{\\alpha - 1}(1 - x)^{\\beta - 1}}{B(\\alpha,\\beta)}\\)")
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
tabPanel("Exponential Families", 
         "this"
       
),

tabPanel("Location and Scale Families", 
         "this"
         
),

tabPanel("Transformation of Random Variables", 
         "this"
         
)

  #includeHTML("mycode.html")
)
)