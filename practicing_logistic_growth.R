
library(rootSolve)
library(deSolve)
library(ggplot2)
library(reshape2)


## from a primer of ecology with R

thetalogistic <- function(times, y, parms) {
	n <- y[1]
	with(as.list(parms), {
		dN.dt <- r * n * (1-(alpha*n) ^ theta)
		return(list(c(dN.dt)))
	})
}

r <- 0.75
alpha <- 0.01
theta <- c(0.5, 1, 2)

N <- 0:110

theta.out <- sapply(theta, function(th) {
	1 - (alpha * N) ^ th
})

matplot(N, theta.out, col = 1)
abline(h = 0)

prms <- c(r <- 0.75, alpha <- 0.01, theta = 1)

thetaN <- sapply(theta, function(th) {
	prms["theta"] <- th
	ode(y = 1, t.s, thetalogistic, prms) [,2]
})

matplot(t.s, thetaN, type = "l")

library(deSolve)
clogistic <- function(times, y, parms) {
	n <- y[1]
	r <- parms[1]
	alpha <- parms[2]
	dN.dt <- r * n * (1-alpha *n)
	return(list(c(dN.dt)))
}

prms <- c(r = 1, alpha = 0.01)
init.N <- c(1)
t.s <- seq(0.1, 10, by = 0.1)

out <- ode(y = init.N, times = t.s, clogistic, parms = prms)

plot(out[,1], out[,2], type = "l", xlab = "Time", ylab = "N")

?plot
plot