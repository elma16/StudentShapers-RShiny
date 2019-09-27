#------------
library(shiny)
library(ggplot2)

#------------

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Brief overview of QQ plots"),
    
    # Show a plot of the generated distribution
    plotOutput("qqplot"),
    
    hr(),
    
    # Sidebar with a slider input for the mean, sd, skew and kurtosis (just a test to see if vs works)
    fluidRow(
        column(3,
               h4("First variable"),
               selectInput("pdf1", "Choose the pdf:",
                           choices = c("Normal" = "norm1", "Gamma" = "gam1", "Student-t" = "stu1")),
               #normal
               conditionalPanel(
                   condition="input.pdf1==\"norm1\"",
                   sliderInput("norm1.n", "Number of trials:", min = 10, max = 1000, value = 500),
                   sliderInput("norm1.mean","\\(\\delta^2\\):", min = -5, max = 5,value = 0, step = 0.1),
                   sliderInput("norm1.sd", "Standard deviation:",min = 0, max = 5,value = 1, step = 0.1)
               ),
               #gamma
               conditionalPanel(
                   condition="input.pdf1==\"gam1\"",
                   sliderInput("gam1.n", "Number of trials:", min = 10, max = 1000, value = 500),
                   sliderInput("gam1.shp","Shape:", min = -5, max = 5,value = 0, step = 0.1),
                   sliderInput("gam1.rt", "Rate:",min = 0, max = 5,value = 1, step = 0.1)
               ),
               #student-t
               conditionalPanel(
                   condition="input.pdf1==\"stu1\"",
                   sliderInput("stu1.n", "Number of trials:", min = 10, max = 1000, value = 500),
                   sliderInput("stu1.df","\\(\\delta^2\\):", min = -5, max = 5,value = 0, step = 0.1),
                   sliderInput("stu1.ncp", "Standard deviation:",min = 0, max = 5,value = 1, step = 0.1)
               )
   ),
        column(3, offset = 1,
               h4("Second variable"),
               selectInput("pdf2", "Choose the pdf:",
                           choices = c("Normal" = "norm2", "Gamma" = "gam2", "Student-t" = "stu2")),
               #normal
               conditionalPanel(
                   condition="input.pdf2==\"norm2\"",
                   sliderInput("norm2.n", "Number of trials:", min = 10, max = 1000, value = 500),
                   sliderInput("norm2.mean","\\(\\delta^2\\):", min = -5, max = 5,value = 0, step = 0.1),
                   sliderInput("norm2.sd", "Standard deviation:",min = 0, max = 5,value = 1, step = 0.1)
               ),
               #gamma
               conditionalPanel(
                   condition="input.pdf2==\"gam2\"",
                   sliderInput("gam2.n", "Number of trials:", min = 10, max = 1000, value = 500),
                   sliderInput("gam2.shp","Shape:", min = -5, max = 5,value = 0, step = 0.1),
                   sliderInput("gam2.rt", "Rate:",min = 0, max = 5,value = 1, step = 0.1)
               ),
               #student-t
               conditionalPanel(
                   condition="input.pdf2==\"stu2\"",
                   sliderInput("stu2.n", "Number of trials:", min = 10, max = 1000, value = 500),
                   sliderInput("stu2.df","\\(\\delta^2\\):", min = -5, max = 5,value = 0, step = 0.1),
                   sliderInput("stu2.ncp", "Standard deviation:",min = 0, max = 5,value = 1, step = 0.1)
               )
        ),
        column(3, offset = 1,
               h4("General Information"),
               conditionalPanel(
                   condition = "input.pdf1==\"norm1\"",
               p("QQ plots compare the distributions of two variables. Most frequently, we want to compare the distribution of some data (or of some residuals) to a reference distribution, such as the standard normal. We plot the quantiles of one distribution against the quantiles of the other. 
                 This means we plot the median of x against the median of y (50% quantiles), lower quartile of $x$ against lower quartile of $y$ (25% quantiles); the same for the upper quartiles and indeed for all quantiles.We can either plot some observed data against the exact quantiles of a known distribution, or plot two sets of observed data against each other. 
                 Let's compare a random sample from the standard normal to the known distribution.")
        ),
                # mean and sd
                conditionalPanel(
                    condition = "input.pdf1==\"norm1\"" ,
                    p("Changes in the mean and standard deviation affect the QQ plot in an easily identifiable way. You should be able to work out how, by thinking about the properties of expectation. Let's look at what happens when we plot a variable with a higher mean, but otherwise identical distribution")
                ),
                conditionalPanel(
                    condition = "input.pdf1==\"gam1\"",
                    p("What about if we look at a skewed distribution? We use the `scale` command to make `y` have zero mean and standard deviation 1 (since we know how these affect the QQ-plot).")
                ),
                conditionalPanel(
                    condition = "input.n <= 100",
                    p("Large samples have easily interpretable patterns. To understand what is reasonable for a small sample, look at many small samples of how well genuinely normally distributed samples match the theoretical distribution. Note that a couple of times in 20 we see quite marked departures from the line.")
                ),
                conditionalPanel(
                   condition="input.pdf2==\"stu2\"",
                   p("Note how, particularly in the centre of the distribution, the theoretical and sample quantiles match perfectly.
                     To compare two samples of observed data, we proceed as follows")
               )
        )
    )
))

