
library(shiny)
library(ggplot2)

shinyUI(fluidPage(
    fluidRow(
        column(width = 6,
               plotOutput("plot1", height = 350,
                          click = "plot1_click",
                          brush = brushOpts(
                              id = "plot1_brush"
                          )
               ),
               actionButton("exclude_toggle", "Toggle points"),
               actionButton("exclude_reset", "Reset")
        )
    )
))
