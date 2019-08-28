#this is based on the box-muller algorithm from page 249 of casella and berger's statistical inference (second edition)


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Box-Muller Algorithm"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Range:",
                        min = 0,
                        max = 1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
