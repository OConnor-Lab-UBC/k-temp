

#### Trying to fit a Michaelis Mentenn function

library(drc)
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(plotrix)

data <- read_csv("data/ChlamyKtemp_aftersorting.csv")

data.s <- data %>% 
	filter(day != 14) %>% 
	filter(temperature == 18) %>% 
	filter(replicate == 1)

model.nls <- nls(density ~ Vm * day/(K+day), data = data.s, 
								 start = list(K = max(data.s$day)/2, Vm = max(data.s$density)))

mml <- data.frame(day = seq(0, max(data.s$day), length.out = 50))
mml$density <- predict(model.nls, newdata = mml)

ggplot(data.s, aes(x = day, y = density)) +
	theme_bw() +
	xlab("day") +
	ylab("log cell density") +
	geom_point(alpha = 1) +
	geom_line(data = mml, aes(x = day, y = density), colour = "red")


# now estimate r and K for all the temperatures! --------------------------
nls(density ~ Vm * day/(K+day), data = data.s, 
								 start = list(K = max(data.s$day)/2, Vm = max(data.s$density)))



# plot the fitted K and r estimates ---------------------------------------

data %>% 
	group_by(temperature, replicate) %>%  
	do(tidy(nls(density ~ K * day/(r+day), data = ., 
							start = list(r = max(.$day)/2, K = max(.$density))))) %>% 
	group_by(temperature, term) %>% 
	summarise_each(funs(mean, std.error), estimate) %>% 
ggplot(data = ., aes(x = temperature, y = mean)) + geom_point() + facet_wrap( ~ term, scales = "free") +
	geom_errorbar(aes(ymin = mean - std.error, ymax= mean + std.error))



ggplot(data.s, aes(x = day, y = density)) +
	theme_bw() +
	xlab("day") +
	ylab("log cell density") +
	geom_point(alpha = 1) +
	geom_line(data = mml, aes(x = day, y = density), colour = "red")
