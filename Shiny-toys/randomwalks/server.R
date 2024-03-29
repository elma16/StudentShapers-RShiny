
library(shiny)
library(shinythemes)

shinyServer(function(input, output) {
    
    #plotting the random walks
    #one dimensional
    output$cdf <- renderPlot({
        if (input$illustration=="1d"){
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
        }
        #lattice path
        if (input$illustration=="2d"){
            m <- input$two.N
            RW2D<-function(N)
            {
                i<-0
                xdir<-0
                ydir<-0
                xpos<-vector()
                xpos[1]<-xdir
                ypos<-vector()
                ypos[1]<-ydir
                for (i in 1:N-1)
                {
                    r<-runif(1)
                    if(r<=0.25) {xdir<-xdir+1}
                    if(r>0.25 && r<=0.5) {xdir<-xdir-1}
                    if(r>0.5 && r<=0.75) {ydir<-ydir +1}
                    if(r>0.75) {ydir<-ydir-1}
                    xpos[i+1]<-xdir
                    ypos[i+1]<-ydir
                }
                return(cbind(xpos,ypos))
            }
            rw<-RW2D(m)
            
            xmin<-min(rw[,1])
            xmax<-max(rw[,1])
            ymin<-min(rw[,2])
            ymax<-max(rw[,2])
            
            plot(rw[,1],rw[,2],type="l",xlab="x",ylab="y",main="Random Walk Simulation In Two Dimensions",col="green4",xlim=range(xmin:xmax),ylim=range(ymin:ymax))
            
            end<-cbind(rw[m,1],rw[m,2])
            start<-cbind(0,0)
            
            points(start,pch=4,col="red")
            points(end,pch=4,col="black")
            
        }
        
        if (input$illustration=="angle"){
            step <- input$angle.step
            jump <- input$angle.jump
            # generate n.step random directions (angles)
            theta <- runif(step, 0, 2*pi) 
            # compute the x and y step sizes
            dx <- jump * cos(theta) 
            dy <- jump * sin(theta)
            # compute the cumulative x and y positions
            x <- c(0, cumsum(dx)) 
            y <- c(0, cumsum(dy))
            # plot the walk
            plot(x, y, type="l", bty="n", col="red")
            
            # plot the start and end points
            points(x[1], y[1], pch=16, cex=1.5) 
            points(x[step+1], y[step+1], pch=16,cex=1.5)
            # join the start and end points
            lines(x[c(1, step+1)], y[c(1, step+1)], lwd=3)
            # compute the distance from start to end
            r <- sqrt(x[step+1]^2 + y[step+1]^2)
            # print on the plot
            label <- paste("distance=", signif(r, 3)) 
            text(max(x), max(y), labels=label, pos=2)
        }
    })
    
    
    
    
})
