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
         sliderInput("poi.n","Number of attempts",min=1,max=100,value=10),
         sliderInput("poi.lam",withMathJax("\\(\\lambda\\)"),min=0.1,max=10,value=1,step=0.1)
      ),
      
      conditionalPanel(
         condition="input.illustration==\"norm\"",
         sliderInput("norm.n",withMathJax("\\(n\\)"),min=1,max=100,value=1,step=1),
         sliderInput("norm.mean","\\(\\mu\\)",min=0.1,max=10,value=1,step=0.1),
         sliderInput("norm.sd","\\(\\sigma\\)",min=0.1,max=10,value=1,step=0.1)
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
      
      if(input$illustration=="binom"){
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
      }
      
      if(input$illustration=="poi"){
      n <- input$poi.n
      lam <- input$poi.lam
      Y <- rpois(n,lam)
      Ysorted <- sort(Y,decreasing = FALSE)
      paste("Observations:",
            paste(Y,collapse=", "),
            #paste("\\hat{p}=",sum(X)/input$n,sep=""),
            "Ordered Observations:",paste(Ysorted, collapse =", "),
            sep="\n")
      }
      
      if(input$illustration=="norm"){
      n <- input$norm.n
      mean <- input$norm.mean
      sd <- input$norm.sd
      Z <- rnorm(m,mean,sd)
      Zsorted <- sort(Z,decreasing = FALSE)
      paste("Observations:",
            paste(Z,collapse=", "),
            #paste("\\hat{p}=",sum(X)/input$n,sep=""),
            "Ordered Observations:",paste(Zsorted, collapse =", "),
            sep="\n")
      }
   })
   #output$likelihood <-   renderPlot({
   #  
   #    if(input$illustration=="binom"){
   #    set.seed(input$seed)
   #    X <- rbinom(n,size=1,p)
   #    #Y <- sum(choose(n,j),X^j(1-X)^j)
   #    p <- seq(0,1,by=0.005)
   #    plot()
   #    plot(p,p^sum(X)*(1-p)^(input$n-sum(X)),xlab="p",ylab="L(p)",main="Likelihood function",type="l")
   #    lines(rep(sum(X)/input$n,2),c(0,1000),col="red")
   #    }
   #    
   #    if(input$illustration=="poi"){
   #       
   #    }
   #    
   #    if(input$illustration=="norm"){
   #       
   #    }
   #    
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

