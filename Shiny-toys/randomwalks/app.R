#this is the webapp version of a script i wrote for constructing random walks

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Random Walks"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("illustration","Choose walk:",
                        choices = c("1D" = "1d", "2D" = "2d","Angled" = "angle"))
            ) ,           
        # conditionalPanel(
        #     condition="input.illustration==\"1d\"",
        #     sliderInput("one.step", "Number of steps:", min = 10, max = 500, value = 100),
        #     sliderInput("one.jump", "Size of jump",min = 1, max = 10, value = 1)
        #     ),
        # conditionalPanel(
        #     condition="input.illustration==\"2d\"",
        #     sliderInput("lattice.step", "Number of steps:", min = 10, max = 500, value = 100),
        #     sliderInput("lattice.jump", "Size of jump",min = 1, max = 10, value = 1)
        # ),
        # conditionalPanel(
        #     condition="input.illustration==\"angle\"",
        #     sliderInput("angle.step", "Number of steps:", min = 10, max = 500, value = 100),
        #     sliderInput("angle.jump", "Size of jump",min = 1, max = 10, value = 1)
        # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
            # generate bins based on input$bins from ui.R
            step <- input$one.step
            jump <- input$one.jump
            x <- 0:step
            # compute the y steps
            dy <- jump * sample(c(-1, 1), size=step, 
                                replace=TRUE)
            plot(dy)
            # compute the cumulative y position
            y <- c(0, cumsum(dy))
            # plot the walk
            plot(x, y, type="s")
            # plot a line at y=0
            abline(h=0, lty=2)
            # plot the start and end points
            points(step, y[1], pch=16, cex=1.5) 
            points(step, y[step+1], pch=16, cex=1.5)
            # join with a line the start and end points
            lines(c(step, step), c(0, y[step+1])) # compute the distance travelled
            r <- y[step+1] - y[1]
            # print on the plot
            label <- paste("distance=", signif(r, 3)) 
            text(max(x), max(y), labels=label, pos=2)
        
        # if (input$illustration=="2d"){
        #     step <- input$lattice.step
        #     jump <- input$lattice.jump
        #     RW2D<-function(N)
        #     {
        #         i<-0
        #         xdir<-0
        #         ydir<-0
        #         xpos<-vector()
        #         xpos[1]<-xdir
        #         ypos<-vector()
        #         ypos[1]<-ydir
        #         for (i in 1:N-1)
        #         {
        #             r<-runif(1)
        #             if(r<=0.25) {xdir<-xdir+1}
        #             if(r>0.25 && r<=0.5) {xdir<-xdir-1}
        #             if(r>0.5 && r<=0.75) {ydir<-ydir +1}
        #             if(r>0.75) {ydir<-ydir-1}
        #             xpos[i+1]<-xdir
        #             ypos[i+1]<-ydir
        #         }
        #         return(cbind(xpos,ypos))
        #     }
        #     rw<-RW2D(10000)
        #     
        #     xmin<-min(rw[,1])
        #     xmax<-max(rw[,1])
        #     ymin<-min(rw[,2])
        #     ymax<-max(rw[,2])
        #     
        #     plot(rw[,1],rw[,2],type="l",xlab="x",ylab="y",main="Random Walk Simulation In Two Dimensions",col="green4",xlim=range(xmin:xmax),ylim=range(ymin:ymax))
        #     
        #     end<-cbind(rw[10000,1],rw[10000,2])
        #     start<-cbind(0,0)
        #     
        #     points(start,pch=4,col="red")
        #     points(end,pch=4,col="red")
        #     
        # }
        # 
        # if (input$illustration=="angle"){
        #     step <- input$angle.step
        #     jump <- input$angle.jump
        #     # generate n.step random directions (angles)
        #     theta <- runif(n.step, 0, 2*pi)
        #     # compute the x and y step sizes
        #     dx <- n.jump * cos(theta) 
        #     dy <- n.jump * sin(theta)
        #     # compute the cumulative x and y positions
        #     x <- c(0, cumsum(dx)) 
        #     y <- c(0, cumsum(dy))
        #     # plot the walk
        #     plot(x, y, type="l", bty="n", col="red")
        #     # plot the start and end points
        #     points(x[1], y[1], pch=16, cex=1.5) 
        #     points(x[n.step+1], y[n.step+1], pch=16,cex=1.5)
        #     # join the start and end points
        #     lines(x[c(1, n.step+1)], y[c(1, n.step+1)], lwd=3)
        #     # compute the distance from start to end
        #     r <- sqrt(x[n.step+1]^2 + y[n.step+1]^2)
        #     # print on the plot
        #     label <- paste("distance=", signif(r, 3)) 
        #     text(max(x), max(y), labels=label, pos=2)
        #     
        # }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
