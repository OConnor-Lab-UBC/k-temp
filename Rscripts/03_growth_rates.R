#### PK temp growth rate analysis
#### nov 2 2016
#### Joey Bernhardt
#### Nov 11 2016: next steps, get the growth rates for the exponential for each temperature separately depending on the number of days to include before the peak

library(tidyverse)
library(plotrix)
library(purrr)
library(broom)
library(gridExtra)

cells <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data/ptemp_all_3.csv")

cells_full <- cells %>% 
	filter(P.treatment == "FULL") 

cells_def <- cells %>% 
	filter(P.treatment == "DEF") 

#### where do the replicates reach their first peak?
#### 8C: 1: 16.941, 2: 16.942 3., 16.94326 4., 16.9440856, 5.16.9449
#### 12C: 1: day 16.95, 2, 16.95, 3, 4, 5, 16.06.  
#### 16C: 1: 11.10. 2. 3. 4:11.10, 13.2666
#### 20C: 1: 9.0273 2: 7.7849305 3., 7.786 4., 7.7903 5: 7.7889
#### 24C: 1: 7.8077, 2: 7.80, 3: 11.163 4., 3.958391 5., 11.165844
cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8) %>% 
	# filter(replicate == 5) %>% 
	filter(time_since_innoc_days < 17) %>% 
	group_by(temperature, replicate, Unique_ID) %>% 
	ggplot(aes(x = time_since_innoc_days, y = cell_density, group = Unique_ID, color = factor(replicate))) + geom_point() +
	geom_line() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

r8 <- cells_full %>% 
	 filter(time_since_innoc_days < 16.95) %>% 
	# filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == "8") %>% 
	group_by(temperature, replicate, Unique_ID) %>% 
	do(tidy(nls(cell_density ~ 20000 * (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
	group_by(temperature) %>% 
	summarise(mean_gr = mean(estimate)) %>% View
	ggplot(aes(x = time_since_innoc_hours, y = cell_density, group = Unique_ID, color = factor(temperature))) + geom_point() + geom_line()

	
	cells_full %>% 
		filter(time_since_innoc_days < 16.95) %>% 
		# filter(!grepl("B", Unique_ID )) %>% 
		filter(temperature == "8") %>% 
		group_by(temperature, replicate, Unique_ID) %>% 
	do(tidy(nls(cell_density ~ 
								SSlogis(time_since_innoc_days, Asym, xmid, scal), 
							data=.))) %>% View
	
	
	r12 <- cells_full %>% 
		filter(time_since_innoc_days < 16.96) %>% 
		# filter(!grepl("B", Unique_ID )) %>% 
		filter(temperature == "12") %>% 
		group_by(temperature, replicate, Unique_ID) %>% 
		do(tidy(nls(cell_density ~ 20000 * (1+a)^(time_since_innoc_days),
								data= .,  start=list(a=0.01),
								control = nls.control(maxiter=100, minFactor=1/204800000))))

	r16 <- cells_full %>% 
		filter(time_since_innoc_days < 13.3) %>% 
		# filter(!grepl("B", Unique_ID )) %>% 
		filter(temperature == "16") %>% 
		group_by(temperature, replicate, Unique_ID) %>% 
		do(tidy(nls(cell_density ~ 20000 * (1+a)^(time_since_innoc_days),
								data= .,  start=list(a=0.01),
								control = nls.control(maxiter=100, minFactor=1/204800000))))
	
	r20 <- cells_full %>% 
		filter(time_since_innoc_days <9.03) %>% 
		filter(!grepl("B", Unique_ID )) %>% 
		filter(temperature == "20") %>% 
		group_by(temperature, replicate, Unique_ID) %>% 
		do(tidy(nls(cell_density ~ 20000 * (1+a)^(time_since_innoc_days),
								data= .,  start=list(a=0.01),
								control = nls.control(maxiter=100, minFactor=1/204800000))))
	
	r24 <- cells_full %>% 
		filter(time_since_innoc_days < 7) %>% 
		filter(!grepl("B", Unique_ID )) %>% 
		filter(temperature == "24") %>% 
		group_by(temperature, replicate, Unique_ID) %>% 
		do(tidy(nls(cell_density ~ 20000 * (1+a)^(time_since_innoc_days),
								data= .,  start=list(a=0.01),
								control = nls.control(maxiter=100, minFactor=1/204800000))))
	
all_r <- bind_rows(r8, r12, r16, r20, r24)

all_r %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point()

all_r %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	lm(log(estimate) ~ inverse_temp, data = .) %>% 
	summary
	
cells_full %>% 
	filter(time_since_innoc_days < 7) %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	# filter(temperature == "8" | temperature == "12") %>% 
group_by(temperature, replicate) %>%
	do(tidy(nls(cell_density ~ 10000 * (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) %>%
	# mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	# lm(log(estimate) ~ inverse_temp, data = .) %>% 
	# summary
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
write_csv(all_rates, "/Users/Joey/Documents/chlamy-ktemp/k-temp/data/chlamy-ktemp-growth-rates.csv")

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
	do(tidy(lm(log(estimate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	filter(term == "inverse_temp") %>%
	ggplot(data = ., aes(x = nutrient_level, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

