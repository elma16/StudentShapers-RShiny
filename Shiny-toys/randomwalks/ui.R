#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(navbarPage("Random Walks",
                 #don't quite understand what the point of this is
                 #includeHTML("navbar.html"),
                 
                 #changes the theme of the webpage
                 theme = shinythemes::shinytheme("cerulean"),
                 
                 #title
                 #headerPanel("Illustrations in M2S1"),
                 
                 fluidRow(
                     column(3,
                            # Slider input 
                            selectInput("illustration", "Choose Walk:",
                                        #the options for the distributions to choose between
                                        choices = c("1d" = "1d", "2d" = "2d","angle" = "angle")),
                            #1d
                            conditionalPanel(
                                condition="input.illustration==\"1d\"",
                                sliderInput("one.step", "Number of steps:", min = 10, max = 500, value = 100),
                                sliderInput("one.jump", "Size of jump",min = 1, max = 10, value = 1)
                            ),
                            #lattice
                            conditionalPanel(
                                condition="input.illustration==\"2d\"",
                                sliderInput("two.N",withMathJax(helpText("Steps : \\(N\\)")),min=10,max=1000000,value=10000,step=1)
                            ),
                            #angled
                            conditionalPanel(
                                condition="input.illustration==\"angle\"",
                                sliderInput("angle.step", "Number of steps:", min = 10, max = 500, value = 100),
                                sliderInput("angle.jump", "Size of jump",min = 1, max = 10, value = 1)
                            ),
                            #links to all the code
                            helpText(a(href = "https://github.com/elma16/StudentShapers-RShiny/blob/master/M2S1/Distributions/app.R", target = "_blank", "View code")),
                            helpText(a(href = "https://github.com/elma16/StudentShapers-RShiny", target = "_blank", "Check out other apps")),
                            helpText(a(href = "https://openintro.org", target = "_blank", "Learn more for free!"))
                            
                     ),
                     
                     column(3, offset=1,
                            
                            # Maths equations
                            withMathJax(),
                            uiOutput('tdisteq')
                            
                     )
                     
                 ),
                 
                 
                 
                 #text outputs to accompany the plots 
                 mainPanel(
                     plotOutput("cdf")
                 )
                 
                 
                 
                 #includeHTML("mycode.html")
))

