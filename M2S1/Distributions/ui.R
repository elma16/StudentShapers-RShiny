#-----------------------------------------------

library(shiny)
library(ggplot2)
library(shinythemes)
library(EnvStats)
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
                #the options for the distributions to choose between
                choices = c("Uniform" = "unif", "Exponential" = "exp", "Gamma" = "gamma", "Weibull" = "wei" , "Normal" = "norm" , "student-t"="tdistr", "Pareto" = "par" ,"\\(\\beta\\)"="b"  , "\\(\\chi^2\\)"="chisq", "F" = "f","Cauchy" = "cau")),
#gamma
    conditionalPanel(
      condition="input.illustration==\"gamma\"",
      sliderInput("gamma.shp","Shape \\(n\\)",min=1,max=100,value=10,step=1),
      sliderInput("gamma.rt","Rate \\(\\delta^2\\)",min=0,max=50,value=5,step=0.1)
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
  sliderInput("unif.n",withMathJax(helpText("Number of observations : \\(n\\)")),min=-10,max=10,value=1,step=1),
  sliderInput("unif.minmax",withMathJax(helpText("Lower and upper bounds : \\(minmax\\)")),min = -20, max = 20,value = c(-15,15))
  ),
#F distribution
conditionalPanel(
  condition="input.illustration==\"f\"",
  sliderInput("f.df1",withMathJax(helpText("Degrees of Freedom : \\(d_1\\)")),min=0,max=10,value=1,step=1),
  sliderInput("f.df2",withMathJax(helpText("Degrees of Freedom : \\(d_2\\)")),min=0,max=10,value=1,step=1),
  sliderInput("f.ncp","Non-centrality parameter : \\(\\delta^2\\)",min=0,max=0.99,value=0,step=0.01)
),
#Weibull distribution
conditionalPanel(
  condition="input.illustration==\"wei\"",
  sliderInput("wei.shp",withMathJax(helpText("Shape : \\(\\lambda\\)")),min=0,max=10,value=1,step=1),
  sliderInput("wei.scl",withMathJax(helpText("Scale : \\(k\\)")),min=0,max=10,value=1,step=1)
),
#Cauchy distribution
conditionalPanel(
  condition="input.illustration==\"cau\"",
  sliderInput("cau.loc",withMathJax(helpText("Location : \\(x_0\\)")),min=-10,max=10,value=1,step=1),
  sliderInput("cau.scl",withMathJax(helpText("Scale : \\(\\gamma\\)")),min=0,max=10,value=1,step=1)
),
#Exponential distribution
conditionalPanel(
  condition="input.illustration==\"exp\"",
  sliderInput("exp.rate",withMathJax(helpText("Rate : \\(\\lambda\\)")),min=0,max=10,value=1,step=1),
  radioButtons("log","Log:",c("True" = "t","False"="f"))
),
#pareto distribution
conditionalPanel(
  condition="input.illustration==\"par\"",
  sliderInput("par.loc",withMathJax(helpText("Location : \\(\\mu\\)")),min=0,max=50,value=1,step=1),
  sliderInput("par.shp",withMathJax(helpText("Scale : \\(\\sigma\\)")),min=0,max=50,value=1,step=1)
),
#normal distribution
conditionalPanel(
  condition="input.illustration==\"norm\"",
  sliderInput("norm.mean",withMathJax(helpText("Mean : \\(\\mu\\)")),min=-100,max=100,value=1,step=1),
  sliderInput("norm.sd",withMathJax(helpText("Standard Deviation : \\(\\sigma\\)")),min=0,max=100,value=1,step=1)
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
    p("The Cauchy distribution has the interesting property that all of the higher moments are undefined."),
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
)