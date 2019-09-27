#----------
#loading the required modules
library(shiny)
library(ggplot2)
library(shinythemes)
library(EnvStats)
library(Compositional)
library(MCMCpack)
library(mvtnorm)
#----------

shinyUI(navbarPage("Distributions in M2S1",
                 #changing the theme of the webpage
                 theme = shinythemes::shinytheme("cerulean"),
                 
                 #title
                 tabPanel("Introduction",
                          #img(src='Imperial-logo.svg', height = "20%", width="20%", align = "right"),
                          h1("This is an interactive form of the formula sheet, applicable for students of M1S and M2S1."),
                          h2("In here are lots of the core facts for each of the distributions, along with the plots of the function"),
                          h2("Feel free to play around with it!")
                 ),
                 tabPanel("Univariate Discrete Families",
                          sidebarPanel(
                            selectInput("illustration3", "Choose Distribution:",
                                        #the options for the distributions to choose between
                                        choices = c("Bernoulli" = "bern", "Binomial" = "bin", "Poisson" = "poi", "Geometric" = "geom" , "Negative Binomial" = "negbin")
                                        ),
                            
                            #bernoulli
                            conditionalPanel(
                              condition="input.illustration3==\"bern\"",
                              sliderInput("bern.p","Probability of success \\(p\\)",min=0,max=1,value=0.5,step=0.01),
                              radioButtons("bern.plottype", "Choose the plot",
                                           list("pmf"="pmf","cdf" = "cdf"))
                            ),
                            #binomial
                            conditionalPanel(
                              condition="input.illustration3==\"bin\"",
                              sliderInput("bin.n","Number of trials \\(n\\)",min=0,max=100,value=10,step=1),
                              sliderInput("bin.p","Probability of success \\(p\\)",min=0,max=1,value=0.5,step=0.01),
                              radioButtons("bin.plottype", "Choose the plot",
                                           list("pmf"="pmf","cdf" = "cdf"))
                            ),
                            #poisson
                            conditionalPanel(
                              condition="input.illustration3==\"poi\"",
                              sliderInput("poi.lam","Rate \\(\\lambda\\)",min=1,max=50,value=10,step=1),
                              radioButtons("poi.plottype", "Choose the plot",
                                           list("pmf"="pmf","cdf" = "cdf"))
                            ),
                            #geometric
                            conditionalPanel(
                              condition="input.illustration3==\"geom\"",
                              sliderInput("geom.p","Probability of success \\(p\\)",min=0,max=1,value=0.5,step=0.01),
                              radioButtons("geom.plottype", "Choose the plot",
                                           list("pmf"="pmf","cdf" = "cdf"))
                            ),
                            #negative binomial
                            conditionalPanel(
                              condition="input.illustration3==\"negbin\"",
                              sliderInput("negbin.n","Number of trials \\(n\\)",min=1,max=50,value=10,step=1),
                              sliderInput("negbin.p","Probability of success \\(p\\)",min=0,max=1,value=0.5,step=0.01),
                              radioButtons("negbin.plottype", "Choose the plot",
                                           list("pdf"="pdf","cdf" = "cdf"))
                            ),
                            
                            #links to all the code
                            helpText(a(href = "https://github.com/elma16/StudentShapers-RShiny/tree/master/M2S1/Distributions", target = "_blank", "View code")),
                            helpText(a(href = "https://github.com/elma16/StudentShapers-RShiny", target = "_blank", "Check out other apps"))
                            
                            
                          ),
                          
                          mainPanel(
                            img(src='Imperial-logo.svg', height = "20%", width="20%", align = "right"),
                            plotOutput("disccdf")
                          )

                          ),
                 tabPanel("Univariate Continuous Families",
                          sidebarPanel(
                              selectInput("illustration", "Choose Distribution:",
                                          #the options for the distributions to choose between
                                          choices = c("Uniform" = "unif", "Exponential" = "exp", "Gamma" = "gamma", "Weibull" = "wei" , "Normal" = "norm" , "Student-t"="tdistr", "Pareto" = "par" ,"\\(\\beta\\)"="b"  , "\\(\\chi^2\\)"="chisq", "F" = "f","Cauchy" = "cau")),
                              #gamma
                              conditionalPanel(
                                  condition="input.illustration==\"gamma\"",
                                  sliderInput("gamma.shp","Shape \\(n\\)",min=1,max=50,value=10,step=1),
                                  sliderInput("gamma.rt","Rate \\(\\delta^2\\)",min=0,max=50,value=25,step=0.1),
                                  radioButtons("gamma.plottype", "Choose the plot",
                                               list("pdf"="pdf","cdf" = "cdf"))
                              ),
                              #multivariable normal
                              conditionalPanel(
                                  condition="input.illustration==\"mvnorm\"",
                                  sliderInput("mvnorm.sd1","\\(\\sigma_1\\)",min=0.1,max=50,value=21,step=0.1),
                                  sliderInput("mvnorm.sd2","\\(\\sigma_2\\)",min=0.1,max=50,value=34,step=0.1),
                                  sliderInput("mvnorm.rho","\\(\\rho\\)",min=-0.99,max=0.99,value=0,step=0.01)
                              ),
                              #t distribution
                              conditionalPanel(
                                  condition="input.illustration==\"tdistr\"",
                                  sliderInput("tdistr.df","degrees of freedom \\(n\\)",min=1,max=50,value=31,step=1),
                                  sliderInput("tdistr.ncp","non-centrality parameter \\(\\delta\\)",min=-3,max=3,value=0,step=0.1),
                                  radioButtons("tdistr.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #chi-square dist
                              conditionalPanel(
                                  condition="input.illustration==\"chisq\"",
                                  sliderInput("chisq.df","degrees of freedom \\(n\\)",min=1,max=50,value=17,step=1),
                                  sliderInput("chisq.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1),
                                  radioButtons("chisqplottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #dirichlet distribution
                              conditionalPanel(
                                  condition="input.illustration==\"dir\"",
                                  sliderInput("dir.df","degrees of freedom \\(n\\)",min=1,max=50,value=1,step=1),
                                  sliderInput("dir.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1),
                                  radioButtons("dirplottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #beta distribution
                              conditionalPanel(
                                  condition="input.illustration==\"b\"",
                                  sliderInput("b.a",withMathJax(helpText("Shape parameter : \\(\\alpha\\)")),min=0.1,max=50,value=13.5,step=0.1),
                                  sliderInput("b.b",withMathJax(helpText("Shape parameter : \\(\\beta\\)")),min=0.1,max=50,value=21,step=0.1),
                                  sliderInput("b.ncp","Non-centrality parameter : \\(\\delta^2\\)",min=0,max=0.99,value=0,step=0.01),
                                  radioButtons("bplottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #uniform distribution
                              conditionalPanel(
                                  condition="input.illustration==\"unif\"",
                                  sliderInput("unif.minmax",withMathJax(helpText("Lower and upper bounds :")),min = -20, max = 20,value = c(-15,15)),
                                  radioButtons("unif.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #F distribution
                              conditionalPanel(
                                  condition="input.illustration==\"f\"",
                                  sliderInput("f.df1",withMathJax(helpText("Degrees of Freedom : \\(d_1\\)")),min=0,max=50,value=30,step=1),
                                  sliderInput("f.df2",withMathJax(helpText("Degrees of Freedom : \\(d_2\\)")),min=0,max=50,value=24,step=1),
                                  sliderInput("f.ncp","Non-centrality parameter : \\(\\delta^2\\)",min=0,max=0.99,value=0,step=0.01),
                                  radioButtons("f.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #Weibull distribution
                              conditionalPanel(
                                  condition="input.illustration==\"wei\"",
                                  sliderInput("wei.shp",withMathJax(helpText("Shape : \\(\\lambda\\)")),min=0.1,max=50,value=16,step=0.1),
                                  sliderInput("wei.scl",withMathJax(helpText("Scale : \\(k\\)")),min=0.1,max=50,value=19,step=0.1),
                                  radioButtons("wei.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #Cauchy distribution
                              conditionalPanel(
                                  condition="input.illustration==\"cau\"",
                                  sliderInput("cau.loc",withMathJax(helpText("Location : \\(x_0\\)")),min=-10,max=50,value=17,step=1),
                                  sliderInput("cau.scl",withMathJax(helpText("Scale : \\(\\gamma\\)")),min=0,max=50,value=13,step=1),
                                  radioButtons("cau.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #Exponential distribution
                              conditionalPanel(
                                  condition="input.illustration==\"exp\"",
                                  sliderInput("exp.rate",withMathJax(helpText("Rate : \\(\\lambda\\)")),min=0,max=50,value=30,step=0.1),
                                  radioButtons("exp.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #pareto distribution
                              conditionalPanel(
                                  condition="input.illustration==\"par\"",
                                  sliderInput("par.loc",withMathJax(helpText("Location : \\(\\mu\\)")),min=0,max=50,value=32,step=1),
                                  sliderInput("par.shp",withMathJax(helpText("Scale : \\(\\sigma\\)")),min=0,max=50,value=61,step=1),
                                  radioButtons("par.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #normal distribution
                              conditionalPanel(
                                  condition="input.illustration==\"norm\"",
                                  sliderInput("norm.mean",withMathJax(helpText("Mean : \\(\\mu\\)")),min=-5,max=5,value=0,step=0.1),
                                  sliderInput("norm.sd",withMathJax(helpText("Standard Deviation : \\(\\sigma\\)")),min=0,max=10,value=1,step=0.1),
                                  radioButtons("norm.plottype", "Choose the plot",
                                               list("pdf" = "pdf","cdf" = "cdf"))
                              ),
                              #links to all the code
                              helpText(a(href = "https://github.com/elma16/StudentShapers-RShiny/tree/master/M2S1/Distributions", target = "_blank", "View code")),
                              helpText(a(href = "https://github.com/elma16/StudentShapers-RShiny", target = "_blank", "Check out other apps"))
                              
                              
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
                                  h4(strong("pdf:")),
                                  h4(strong("cdf:")),
                                  h4(strong("E(x):")),
                                  h4(strong("Var(x):")),
                                  h4(strong("mgf:"))
                                
                
                              ),
                              #cauchy distribution
                              conditionalPanel(
                                  condition="input.illustration==\"cau\"",
                                  p("The Cauchy distribution has the interesting property that all of the higher moments are undefined."),
                                  h4(strong("pdf:")),
                                  h4(strong("cdf:")),
                                  h4(strong("E(x):")),
                                  h4(strong("Var(x):")),
                                  h4(strong("mgf:"))
                              ),
                              #norm distribution
                              conditionalPanel(
                                  condition="input.illustration==\"norm\"",
                                  p("The normal distributions"),
                                  h4(strong("pdf: \\(\\frac{1}{\\beta - \\alpha}\\)")),
                                  h4(strong("cdf:")),
                                  h4(strong("E(x):")),
                                  h4(strong("Var(x):")),
                                  h4(strong("mgf:"))
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
                                  h4(strong("Params: \\(\\lambda \\in \\mathbb{R}\\)")),
                                  h4(strong("PDF: \\(\\frac{1}{\\beta - \\alpha}\\)")),
                                  h4(strong("CDF: \\(\\frac{x - \\alpha}{\\beta - \\alpha}\\) ")),
                                  h4(strong("E(x): \\(\\frac{(\\alpha + \\beta)^2}{2}\\)")),
                                  h4(strong("Var(x): \\(\\frac{(\\beta - \\alpha)^2}{12}\\)")),
                                  h4(strong("MGF: \\(\\frac{e^{\\beta t} - e^{\\alpha t}}{t(\\beta - \\alpha)}\\)"))
                              ),
                              #weibull distribution
                              conditionalPanel(
                                  condition="input.illustration==\"wei\"",
                                  h4(strong("Params: \\(\\alpha < \\beta \\in \\mathbb{R}\\)")),
                                  h4(strong("PDF: \\(\\frac{1}{\\beta - \\alpha}\\)")),
                                  h4(strong("CDF: \\(\\frac{x - \\alpha}{\\beta - \\alpha}\\) ")),
                                  h4(strong("E(x): \\(\\frac{(\\alpha + \\beta)^2}{2}\\)")),
                                  h4(strong("Var(x): \\(\\frac{(\\beta - \\alpha)^2}{12}\\)")),
                                  h4(strong("MGF: \\(\\frac{e^{\\beta t} - e^{\\alpha t}}{t(\\beta - \\alpha)}\\)"))
                              ),
                              #chi-square dist
                              conditionalPanel(
                                  condition="input.illustration==\"chisq\"",
                                  p("Facts:
                                    
                                    a) If Z is a Norm(0,1) random variable, then Z^2 ~ chi^2_1
                                    b) If X_1 , ... , X_n are independent and X_i ~ chi_p^2 then X_1 + X_2 + ... + X_n ~ chi^2_{p_1 + ... + p_n}")
                              ),
                              
                              #dirichlet distribution
                              conditionalPanel(
                                  condition="input.illustration==\"dir\"",
                                  h4(strong("Params: \\(\\alpha < \\beta \\in \\mathbb{R}\\)")),
                                  h4(strong("PDF: \\(\\frac{1}{\\beta - \\alpha}\\)")),
                                  h4(strong("CDF: \\(\\frac{x - \\alpha}{\\beta - \\alpha}\\) ")),
                                  h4(strong("E(x): \\(\\frac{(\\alpha + \\beta)^2}{2}\\)")),
                                  h4(strong("Var(x): \\(\\frac{(\\beta - \\alpha)^2}{12}\\)")),
                                  h4(strong("MGF: \\(\\frac{e^{\\beta t} - e^{\\alpha t}}{t(\\beta - \\alpha)}\\)"))
                              ),
                              #uniform distribution
                              conditionalPanel(
                                  condition="input.illustration==\"unif\"",
                                  h4(strong("Params: \\(\\alpha < \\beta \\in \\mathbb{R}\\)")),
                                  h4(strong("PDF: \\(\\frac{1}{\\beta - \\alpha}\\)")),
                                  h4(strong("CDF: \\(\\frac{x - \\alpha}{\\beta - \\alpha}\\) ")),
                                  h4(strong("E(x): \\(\\frac{(\\alpha + \\beta)^2}{2}\\)")),
                                  h4(strong("Var(x): \\(\\frac{(\\beta - \\alpha)^2}{12}\\)")),
                                  h4(strong("MGF: \\(\\frac{e^{\\beta t} - e^{\\alpha t}}{t(\\beta - \\alpha)}\\)"))
                              ),
                              #pareto distribution
                              conditionalPanel(
                                  condition="input.illustration==\"par\"",
                                  p("The Pareto distribution, coined by Italian economist Vilfredo Pareto"),
                                  h4(strong("pdf:")),
                                  h4(strong("cdf:")),
                                  h4(strong("E(x):")),
                                  h4(strong("Var(x):")),
                                  h4(strong("mgf:"))
                              ),
                              #F distribution
                              conditionalPanel(
                                  condition="input.illustration==\"f\"",
                                  p("The F distribution is a distribution less often discussed in M2S1"),
                                  h4(strong("pdf:")),
                                  h4(strong("cdf:")),
                                  h4(strong("E(x):")),
                                  h4(strong("Var(x):")),
                                  h4(strong("mgf:"))
                              ),
                              #beta distribution
                              conditionalPanel(
                                  condition="input.illustration==\"b\"",
                                  p("The beta distribution is part of a family"),
                                  h4(strong("pdf:")),
                                  h4(strong("cdf:")),
                                  h4(strong("E(x):")),
                                  h4(strong("Var(x):")),
                                  h4(strong("mgf:"))
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
                                  sliderInput("dir.n","Number of random vectors: \\(n\\)",min=1,max=2200,value=200,step=1),
                                  sliderInput("dir.a1","Vector parameter \\(\\alpha_1\\)",min=0,max=5,value=0.1,step=0.1),
                                  sliderInput("dir.a2","Vector param \\(\\alpha_2\\)",min=0,max=5,value=0.1,step=0.1),
                                  sliderInput("dir.a3","Vector param \\(\\alpha_3\\)",min=0,max=5,value=0.1,step=0.1)
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
                 )
))

