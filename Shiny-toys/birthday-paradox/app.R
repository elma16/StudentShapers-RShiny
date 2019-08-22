library(shiny)

#-----
ui <- navbarPage("The Birthday Paradox",
                 
                 #changes the theme of the webpage
                 theme = shinythemes::shinytheme("united"),
                 
                 #title
                 #headerPanel("Illustrations in M2S1"),
                 
                 h1("The Birthday Paradox"),
                 mainPanel(
                     textOutput("ques"),
                     textOutput("answer")
                     ),
                 fluidRow(
                     column(10,
                            # Slider input 
                            sliderInput("prob","Probability",min=0,max=1,value=0.5,step=0.01),
                            sliderInput("cls","Classes",min=1,max=365,value=365,step=1),
                            sliderInput("coi","Coincident",min=2,max=10,value=1,step=1)
                 )

                              
                              # Output: Tabset w/ plot, summary, and table ----

                          
                          
                 
                 #includeHTML("mycode.html")
                 )
)

#----


server <- function(input, output) {
    
    #output the question of the birthday paradox
    output$ques <- renderText(
        {
            prob <- input$prob
            cls <- input$cls
            coi <- input$coi
        paste("What is the minimum number of people needed for a probability of at least", prob , "that" , cls , "or more of them have the same one out of" , coi , "categories are needed to have it?")
        }
    )
    #output the answer to the birthday paradox
    output$answer <- renderText({
        prob <- input$prob
        cls <- input$cls
        coi <- input$coi
        birthday <- qbirthday(prob,cls,coi)
        paste("The answer is",birthday)
        
    })
    

}
#-----
shinyApp(ui = ui, server = server)
