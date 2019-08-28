#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
    
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
    
    
})
