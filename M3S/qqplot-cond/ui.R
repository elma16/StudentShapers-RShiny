library(shiny)

#what does bootstrap mean?
shinyUI(bootstrapPage(
  #not sure what this means
  #includeHTML("navbar.html"),
  headerPanel("QQ plot"),
  
  sidebarPanel(
    selectInput("illustration", "Choose Distribution:",
                choices = c("Normal"="mvnorm","t-distribution"="tdistr","\\(\\chi^2\\)-distribution"="chisq","F-distribution"="F")),
    conditionalPanel(
      condition="input.illustration==\"mvnorm\"",
      withMathJax(helpText("Contour plot of the pdf of N(0,\\(\\Sigma\\))")),
      withMathJax(helpText("where \\(\\Sigma=\\begin{pmatrix}\\sigma_1^2&\\rho\\sigma_1\\sigma_2\\\\\\rho\\sigma_1\\sigma_2&\\sigma_2^2\\end{pmatrix}\\)")),
      sliderInput("mvnorm.sd1",withMathJax("n_1"),min=0.1,max=10,value=1,step=0.1),
      sliderInput("mvnorm.sd2","\\(\\mu_1\\)",min=0.1,max=10,value=1,step=0.1),
      sliderInput("mvnorm.rho","\\(\\sigma_1\\)",min=-0.99,max=0.99,value=0,step=0.01)
      
    ),
    conditionalPanel(
      condition="input.illustration==\"tdistr\"",
      helpText("\\(t_n(\\delta)\\) distribution"),
      sliderInput("tdistr.df","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
      sliderInput("tdistr.ncp","non-centrality parameter \\(\\delta\\)",min=-3,max=3,value=0,step=0.1),
      radioButtons("tdistr.plottype", "Choose the plot",
                   list("cdf" = "cdf","pdf"="pdf"))
    ),
    conditionalPanel(
      condition="input.illustration==\"chisq\"",
      helpText("\\(\\chi^2_n(\\delta)\\) distribution"),
      sliderInput("chisq.df","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
      sliderInput("chisq.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1),
      radioButtons("chisqplottype", "Choose the plot",
                   list("cdf" = "cdf","pdf"="pdf"))
    ),
    conditionalPanel(
      condition="input.illustration==\"F\"",
      helpText("\\(F_{n_1,n_2}(\\delta)\\) distribution"),
      sliderInput("F.df1","degrees of freedom \\(n_1\\)",min=1,max=100,value=1,step=1),
      sliderInput("F.df2","degrees of freedom \\(n_2\\)",min=1,max=100,value=1,step=1),
      sliderInput("F.ncp","non-centrality parameter \\(\\delta^2\\)",min=0,max=100,value=0,step=0.5),
      radioButtons("Fplottype", "Choose the plot",
                   list("cdf" = "cdf","pdf"="pdf"))
      
    )
  ),
  mainPanel(plotOutput("cdf"))
 # includeHTML("mycode.html")
)
)
