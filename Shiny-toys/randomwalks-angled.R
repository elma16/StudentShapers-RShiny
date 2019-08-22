# generate n.step random directions (angles)
n.step <- 100
theta <- runif(n.step, 0, 2*pi) jump <- 1
# compute the x and y step sizes
dx <- jump * cos(theta) dy <- jump * sin(theta)
# compute the cumulative x and y positions
x <- c(0, cumsum(dx)) y <- c(0, cumsum(dy))
# plot the walk
plot(x, y, type="l", bty="n", col="red")

# plot the start and end points
points(x[1], y[1], pch=16, cex=1.5) points(x[n.step+1], y[n.step+1], pch=16,
                                           cex=1.5)
# join the start and end points
lines(x[c(1, n.step+1)], y[c(1, n.step+1)], lwd=3)
# compute the distance from start to end
r <- sqrt(x[n.step+1]^2 + y[n.step+1]^2)
# print on the plot
label <- paste("distance=", signif(r, 3)) text(max(x), max(y), labels=label, pos=2)