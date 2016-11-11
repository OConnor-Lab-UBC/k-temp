#### PK temp growth rate analysis
#### nov 2 2016
#### Joey Bernhardt

library(tidyverse)
library(plotrix)
library(purrr)
library(broom)
library(gridExtra)

cells <- read_csv("data/ptemp_all_3.csv")

cells_full <- cells %>% 
	filter(P.treatment == "FULL") 

cells_def <- cells %>% 
	filter(P.treatment == "DEF") 

cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8) %>% 
	group_by(temperature, replicate, Unique_ID) %>% 
	ggplot(aes(x = time_since_innoc_days, y = cell_density, group = Unique_ID, color = factor(replicate))) + geom_point() +
	geom_line() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

cells_full %>% 
	 filter(time_since_innoc_hours < 250) %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == "20" | temperature == "24") %>% 
	group_by(temperature, replicate, Unique_ID) %>% 
	do(tidy(nls(cell_density ~ 80000 * (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
	group_by(temperature) %>% 
	summarise(mean_gr = mean(estimate)) %>% View
	ggplot(aes(x = time_since_innoc_hours, y = cell_density, group = Unique_ID, color = factor(temperature))) + geom_point() + geom_line()




cells_full %>% 
	# filter(time_since_innoc_hours < 500) %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	# filter(temperature == "8" | temperature == "12") %>% 
group_by(temperature, replicate) %>%
	do(tidy(nls(cell_density ~ 80000 * (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) %>%
	group_by(temperature) %>% 
	summarise_each(funs(mean, std.error), estimate) %>% 
	ggplot(aes(x = temperature, y = mean)) + geom_point() +
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error)) + 
scale_y_log10()

growth_rate <- cells_full %>% 
	filter(Unique_ID != "1",
				 Unique_ID != "2",
				 Unique_ID != "3",
				 Unique_ID != "4",
				 Unique_ID != "5",
				 Unique_ID != "41",
				 Unique_ID != "42",
				 Unique_ID != "43",
				 Unique_ID != "44",
				 Unique_ID != "45") %>%
	group_by(temperature, replicate) %>%
	do(tidy(nls(cell_density ~ 100000 * (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) 

growth_rate %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	lm(log(estimate) ~ inverse_temp, data = .) %>% 
	summary

full <- growth_rate %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
ggplot(data = ., aes(x = inverse_temp, y = log(estimate))) + geom_point() +
	geom_smooth(method = "lm")
	


# now for the p def cells -------------------------------------------------

cells_def %>% 
	# filter(time_since_innoc_hours < 500) %>% 
	filter(grepl("B", Unique_ID )) %>% View


cells_def <- cells_def %>% 
	filter(Unique_ID != "36",
				 Unique_ID != "37",
				 Unique_ID != "38",
				 Unique_ID != "39",
				 Unique_ID != "30",
				 Unique_ID != "46",
				 Unique_ID != "47",
				 Unique_ID != "48",
				 Unique_ID != "49",
				 Unique_ID != "50")


ggplot(data = cells_def, aes(x = time_since_innoc_days, y = cell_density, group = replicate, color = temperature)) +
	geom_point() +
	geom_line(group = replicate)

growth_rate_def <- cells_def %>% 
	group_by(temperature, replicate) %>% 
	do(tidy(nls(cell_density ~ 40000 * (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) 

growth_rate_def %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	lm(log(estimate) ~ inverse_temp, data = .) %>% 
	summary

def <- growth_rate_def %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(estimate))) + geom_point() +
	geom_smooth(method = "lm")

growth_rate$nutrient_level <- "replete"
growth_rate_def$nutrient_level <- "deficient"
all_rates <- bind_rows(growth_rate, growth_rate_def)

all_rates %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(estimate), group = nutrient_level, color = nutrient_level)) + geom_point() +
	geom_smooth(method = "lm") +
	scale_x_reverse() +
	xlab("inverse temperature (1/kT)") + 
	ylab("log growth rate")
	

all_rates %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	group_by(nutrient_level) %>% 
	do(tidy(lm(log(estimate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View
	filter(term == "inverse_temp") %>%
	ggplot(data = ., aes(x = nutrient_level, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

