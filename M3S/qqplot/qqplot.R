library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    title = "QQ plots",
    
    # Show a plot of the generated distribution
    plotOutput("qqplot"),
    
    hr(),
    
    # Sidebar with a slider input for the mean, sd, skew and kurtosis 
    fluidRow(
        column(3,
               h4("First variable"),
               selectInput("illustration1", "Choose Distribution:",
                           choices = c("Normal"="mvnorm","t-distribution"="tdistr","\\(\\chi^2\\)-distribution"="chisq","F-distribution"="F")),
               
               conditionalPanel(
                 condition="input.illustration1==\"mvnorm\"",
                 withMathJax(helpText("Contour plot of the pdf of N(0,\\(\\Sigma\\))")),
                 withMathJax(helpText("where \\(\\Sigma=\\begin{pmatrix}\\sigma_1^2&\\rho\\sigma_1\\sigma_2\\\\\\rho\\sigma_1\\sigma_2&\\sigma_2^2\\end{pmatrix}\\)")),
                 sliderInput("mvnorm.n1",withMathJax(helpText("n_1")),min=0.1,max=1000,value=100,step=1),
                 sliderInput("mvnorm.mu1","\\(\\mu_1\\)",min=0.1,max=10,value=1,step=0.1),
                 sliderInput("mvnorm.sd1","\\(\\sigma_1\\)",min=-0.99,max=0.99,value=0,step=0.01)
               )
               # conditionalPanel(
               #   condition="input.illustration1==\"tdistr\"",
               #   helpText("\\(t_n(\\delta)\\) distribution"),
               #   sliderInput("tdistr.df1","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("tdistr.ncp1","non-centrality parameter \\(\\delta\\)",min=-3,max=3,value=0,step=0.1)
               # ),
               # conditionalPanel(
               #   condition="input.illustration1==\"chisq\"",
               #   helpText("\\(\\chi^2_n(\\delta)\\) distribution"),
               #   sliderInput("chisq.df1","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("chisq.ncp1","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1)
               # ),
               # conditionalPanel(
               #   condition="input.illustration1==\"F\"",
               #   helpText("\\(F_{n_1,n_2}(\\delta)\\) distribution"),
               #   sliderInput("F.df11","degrees of freedom \\(n_1\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("F.df21","degrees of freedom \\(n_2\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("F.ncp1","non-centrality parameter \\(\\delta^2\\)",min=0,max=100,value=0,step=0.5)
               # )
        ),
        
        column(3, offset = 1,
               h4("Second variable"),
               selectInput("illustration2", "Choose Distribution:",
                           choices = c("Normal"="mvnorm","t-distribution"="tdistr","\\(\\chi^2\\)-distribution"="chisq","F-distribution"="F")),
     
              conditionalPanel(
                 condition="input.illustration2==\"mvnorm\"",
                 withMathJax(helpText("Contour plot of the pdf of N(0,\\(\\Sigma\\))")),
                 withMathJax(helpText("where \\(\\Sigma=\\begin{pmatrix}\\sigma_1^2&\\rho\\sigma_1\\sigma_2\\\\\\rho\\sigma_1\\sigma_2&\\sigma_2^2\\end{pmatrix}\\)")),
                 sliderInput("mvnorm.n2",withMathJax("n_1"),min=0.1,max=1000,value=10,step=1),
                 sliderInput("mvnorm.mu2","\\(\\mu_1\\)",min=0.1,max=10,value=1,step=0.1),
                 sliderInput("mvnorm.sd2","\\(\\sigma_1\\)",min=-0.99,max=0.99,value=0,step=0.01)
               )
               # conditionalPanel(
               #   condition="input.illustration2==\"tdistr\"",
               #   helpText("\\(t_n(\\delta)\\) distribution"),
               #   sliderInput("tdistr.df2","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("tdistr.ncp2","non-centrality parameter \\(\\delta\\)",min=-3,max=3,value=0,step=0.1)
               # ),
               # conditionalPanel(
               #   condition="input.illustration2==\"chisq\"",
               #   helpText("\\(\\chi^2_n(\\delta)\\) distribution"),
               #   sliderInput("chisq.df2","degrees of freedom \\(n\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("chisq.ncp2","non-centrality parameter \\(\\delta^2\\)",min=0,max=5,value=0,step=0.1)
               # ),
               # conditionalPanel(
               #   condition="input.illustration2==\"F\"",
               #   helpText("\\(F_{n_1,n_2}(\\delta)\\) distribution"),
               #   sliderInput("F.df12","degrees of freedom \\(n_1\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("F.df22","degrees of freedom \\(n_2\\)",min=1,max=100,value=1,step=1),
               #   sliderInput("F.ncp2","non-centrality parameter \\(\\delta^2\\)",min=0,max=100,value=0,step=0.5)
               # )
        ),
      
        column(3, offset = 1,
               h4("General Information")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
#want to have some refresh thing
    output$qqplot <- renderPlot({
      if (input$illustration1=="mvnorm" & input$illustration2 == "mvnorm"){
        
        n1 <- input$mvnorm.n1
        mu1 <- input$mvnorm.mu1
        sd1 <- input$mvnorm.sd1
        n2 <- input$mvnorm.n2
        mu2 <- input$mvnorm.mu2
        sd2 <- input$mvnorm.sd2
        
        x <- rnorm(n1, mu1, sd1)
        y <- rnorm(n2, mu2, sd2)
        # draw the qqplot of the two normals
        qqplot(x,y)
        abline(0,1,col=2)
      }
    })
  }
  
# Run the application 
shinyApp(ui = ui, server = server)