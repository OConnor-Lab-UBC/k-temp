

#### Trying to fit a Michaelis Mentenn function

library(drc)
library(dplyr)
library(readr)
library(ggplot2)

data <- read_csv("data/ChlamyKtemp_aftersorting.csv")

data.s <- data %>% 
	filter(temperature == 24) %>% 
	filter(replicate == 2) %>% 
	mutate(density = log(density))



model.drm <- drm(density ~ day, data = data.s, fct = MM.2())
mml <- data.frame(day = seq(0, max(data.s$day), length.out = 100))
mml$density <- predict(model.drm, newdata = mml)

ggplot(data.s, aes(x = day, y = density)) +
	theme_bw() +
	xlab("Concentration [mM]") +
	ylab("Speed [dE/min]") +
	ggtitle("Michaelis-Menten kinetics") +
	geom_point(alpha = 0.5) +
	geom_line(data = mml, aes(x = day, y = density), colour = "red")


model.nls <- nls(density ~ Vm * day/(K+day), data = data.s, 
								 start = list(K = max(data.s$day)/2, Vm = max(data.s$density)))

summary(model.nls)
mml <- data.frame(day = seq(0, max(data.s$day), length.out = 50))
mml$density <- predict(model.nls, newdata = mml)

ggplot(data.s, aes(x = day, y = density)) +
	theme_bw() +
	xlab("day") +
	ylab("log cell density") +
	geom_point(alpha = 1) +
	geom_line(data = mml, aes(x = day, y = density), colour = "red")




