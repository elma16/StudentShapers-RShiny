library(shiny)


#get a random sample
#create order statistics

#---------------------- 
ui <- bootstrapPage(
   #includeHTML("navbar.html"),
   headerPanel("Order Statistics"),
   sidebarPanel(
      selectInput("illustration", "Choose Distribution:",choices = c("Binomial" = "binom", "Poisson" = "poi","Normal"="norm")),
      numericInput("rstat", "rth order statistics:", 1, min = 1, max = 100),
      conditionalPanel(
         condition="input.illustration==\"binom\"",
         sliderInput("binom.n","Number of attempts",min=1,max=100,value=10),
         sliderInput("binom.p","Success probability p",min=0,max=1,value=0.5),
         sliderInput("binom.size","Size",min = 1,max = 10,value=1),
         sliderInput("binom.seed","Seed for Random Numbers",min=1,max=5000,value=1231,step=1)
      ),
      
      conditionalPanel(
         condition="input.illustration==\"poi\"",
         sliderInput("norm.mu1",withMathJax("\\(\\mu_1\\)"),min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.sd1","\\(\\sigma_1\\)",min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.mu2","\\(\\mu_2\\)",min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.sd2","\\(\\sigma_2\\)",min=0.1,max=10,value=1,step=0.1)
      ),
      
      conditionalPanel(
         condition="input.illustration==\"norm\"",
         sliderInput("norm.mu1",withMathJax("\\(\\mu_1\\)"),min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.sd1","\\(\\sigma_1\\)",min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.mu2","\\(\\mu_2\\)",min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.sd2","\\(\\sigma_2\\)",min=0.1,max=10,value=1,step=0.1)
      ),
      
      withMathJax(helpText("F_Y(y) = sum_{j = r}^n"))
      #helpText("MLE:  \\(\\hat{p}=\\frac{1}{n}\\sum_{i=1}^n X_i\\).")
   ),
   mainPanel(verbatimTextOutput("observations"),plotOutput("likelihood"))
   #includeHTML("mycode.html")
)


#------------------------------------
server <- function(input, output) {
   #observations for the order
   output$observations <- renderText({
      n <- input$binom.n
      p <- input$binom.p
      size <- input$binom.size
      set.seed(input$binom.seed)
      X <- rbinom(n,size,p)
      Xsorted <- sort(X,decreasing = FALSE)
      paste("Observations:",
            paste(X,collapse=", "),
            #paste("\\hat{p}=",sum(X)/input$n,sep=""),
            "Ordered Observations:",paste(Xsorted, collapse =", "),
            sep="\n")
   })
   output$likelihood <-   renderPlot({
      set.seed(input$seed)
      X <- rbinom(n,size=1,p)
      #Y <- sum(choose(n,j),X^j(1-X)^j)
      p <- seq(0,1,by=0.005)
      plot()
      plot(p,p^sum(X)*(1-p)^(input$n-sum(X)),xlab="p",ylab="L(p)",main="Likelihood function",type="l")
      lines(rep(sum(X)/input$n,2),c(0,1000),col="red")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

