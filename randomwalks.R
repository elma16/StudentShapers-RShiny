# generate n.step steps
n.step <- 100
jump <- 1
x <- 0:n.step

# compute the y steps
dy <- jump * sample(c(-1, 1), size=n.step, 
                    replace=TRUE)
plot(dy)

# compute the cumulative y position
y <- c(0, cumsum(dy))
# plot the walk
plot(x, y, type="s")

# plot a line at y=0
abline(h=0, lty=2)
# plot the start and end points
points(n.step, y[1], pch=16, cex=1.5) points(n.step, y[n.step+1], pch=16, cex=1.5)
# join with a line the start and end points
lines(c(n.step, n.step), c(0, y[n.step+1])) # compute the distance travelled
r <- y[n.step+1] - y[1]
# print on the plot
label <- paste("distance=", signif(r, 3)) text(max(x), max(y), labels=label, pos=2)