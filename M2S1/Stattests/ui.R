
library(shiny)

shinyUI(navbarPage("Statistical Tests",
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
))

#----