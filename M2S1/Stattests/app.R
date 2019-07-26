

library(shiny)

#-----
ui <- navbarPage("Statistical Tests",
                   #don't quite understand what the point of this is
                   #includeHTML("navbar.html"),
                   
                   #changes the theme of the webpage
                   theme = shinythemes::shinytheme("cerulean"),
                   
                   #title
                   #headerPanel("Illustrations in M2S1"),
                   
                   tabPanel("Correlational",
                            
                            
                            
                            #ff 
                            sidebarPanel(
                                
                                # Input: Select a file ----
                                fileInput("file1", "Choose CSV File",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                #Alternative input: Choose a samp
                                selectInput("dataset", "Choose a dataset:",
                                            choices = c("rock", "pressure", "cars")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                
                                # Input: Select quotes ----
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = '"'),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head")
                                
                            ),
                            
                            mainPanel(
                                
                                # Output: Tabset w/ plot, summary, and table ----
                                tabsetPanel(type = "tabs",
                                            tabPanel("Pearson Correlation", plotOutput("pearsonplot"),p("Tests for the strength of the association between two continuous variables")),
                                            tabPanel("Spearman Correlation", plotOutput("spearmanplot"),p("Tests for the strength of the association between two ordinal variables (does not rely on the assumption of normal distributed data)")),
                                            tabPanel("Chi-Square", plotOutput("chisqplot"),p("The chi-square test is a practical test for the strength of the association between two categorical variables."))
                                )
                            )
                            
                            
                            
                    
                            
                   ),
                   tabPanel("Comparison of Means", 
                            
                            #ff 
                            sidebarPanel(
                                
                                # Input: Select a file ----
                                fileInput("file1", "Choose CSV File",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                
                                # Input: Select quotes ----
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = '"'),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head")
                                
                            ),
                            
                            mainPanel(
                                
                                # Output: Tabset w/ plot, summary, and table ----
                                tabsetPanel(type = "tabs",
                                            tabPanel("Paired T-Test", plotOutput("plot")),
                                            tabPanel("Independent T-Test", verbatimTextOutput("summary")),
                                            tabPanel("ANOVA", tableOutput("table"))
                                )
                            )
                            
                   ),
                   tabPanel("Regression", 
                            
                            #ff 
                            sidebarPanel(
                                
                                # Input: Select a file ----
                                fileInput("file1", "Choose CSV File",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                
                                # Input: Select quotes ----
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = '"'),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head")
                                
                            ),
                            
                            mainPanel(
                                
                                # Output: Tabset w/ plot, summary, and table ----
                                tabsetPanel(type = "tabs",
                                            tabPanel("Simple Regression", plotOutput("plot")),
                                            tabPanel("Multiple Regression", verbatimTextOutput("summary"))
                                )
                            )
                            
                   ),
                   
                   tabPanel("Non-Parametric",
                            
                           #ff 
                            sidebarPanel(
                                
                                # Input: Select a file ----
                                fileInput("file1", "Choose CSV File",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                
                                # Input: Select quotes ----
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = '"'),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head")
                                
                            ),
                            
                            mainPanel(
                                
                                # Output: Tabset w/ plot, summary, and table ----
                                tabsetPanel(type = "tabs",
                                            tabPanel("Wilcoxon Rank-Sum Test", plotOutput("plot")),
                                            tabPanel("Wilcoxon Sign-Rank Test", verbatimTextOutput("summary")),
                                            tabPanel("Sign Test", tableOutput("table"))
                                )
                            )
                            
                   )
                   
                   #includeHTML("mycode.html")
)

#----


server <- function(input, output) {
    
    # Return the requested dataset ----
    # Note that we use eventReactive() here, which depends on
    # input$update (the action button), so that the output is only
    # updated when the user clicks the button
    datasetInput <- eventReactive(input$update, {
        switch(input$dataset,
               "rock" = rock,
               "pressure" = pressure,
               "cars" = cars)
    }, ignoreNULL = FALSE)
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    # The use of isolate() is necessary because we don't want the table
    # to update whenever input$obs changes (only when the user clicks
    # the action button)
    output$view <- renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })
    
    #pearson 
    output$pearsonplot <- renderPlot({
        if (input$illustration=="gamma"){
            par(mar=c(2.1,2.1,0.1,0.1))
            shp <- input$gamma.shp
            rt <- input$gamma.rt
            x <- seq(0,b*(a+ncp)/(a*max(b-2,1))*4,length.out=500)
            plot(x,pbeta(x,a,b,ncp),type="l",xlab="",ylab="")
        }
    })
    
    #spearman 
    output$spearmanplot <- renderPlot({
        if (input$illustration1=="mvnorm"){
            library(mvtnorm)
            rho <- input$mvnorm.rho
            sd1 <- input$mvnorm.sd1
            sd2 <- input$mvnorm.sd2
            x <- y <- seq(-3,3,by=0.01);
            Sigma <- diag(c(sd1,sd2))%*% matrix(c(1,rho,rho,1),nrow=2)%*% diag(c(sd1,sd2))
            par(mar=c(2.1,2.1,0.1,0.1))
            contour(x,y,outer(x,y,FUN=function(x,y) dmvnorm(cbind(x,y),sigma=Sigma)))
        }
    })
    
    #chisqplot
    
    output$chisqplot <- renderPlot({
        
        
    })
}
#-----
shinyApp(ui = ui, server = server)







            



   
    



