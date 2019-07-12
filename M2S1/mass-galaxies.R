library(MASS)
library(mclust, quietly = TRUE)
data("galaxies")
X = galaxies / 1000

fit = Mclust(X, G=4, model="V") 
summary(fit)

