#### PK temp growth rate analysis
#### nov 2 2016
#### Joey Bernhardt
#### Nov 11 2016: next steps, get the growth rates for the exponential for each temperature separately depending on the number of days to include before the peak
#### feb 7 2017: maybe can just get the r's here and not K for all. 

library(tidyverse)
library(plotrix)
library(purrr)
library(broom)
library(gridExtra)
library(minpack.lm)
library(growthcurver)
library(stringr)
library(simecol)

cells <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data/ptemp_all_3.csv")

cells_full <- cells %>% 
	filter(P.treatment == "FULL") 

cells_def <- cells %>% 
	filter(P.treatment == "DEF") 


# make some initial plots -------------------------------------------------

cells %>% 
	filter(!grepl("B", Unique_ID )) %>%
	filter(temperature == 8) %>% 
	group_by(Unique_ID) %>% 
	ggplot(aes(time_since_innoc_days, y = total_biovolume, group = Unique_ID, color = P.treatment)) + geom_point() +
	geom_line()
	


#### where do the replicates reach their first peak?
#### 8C: 1: 16.941, 2: 16.942 3., 16.94326 4., 16.9440856, 5.16.9449
#### 12C: 1: day 16.95, 2, 16.95, 3, 4, 5, 16.06.  
#### 16C: 1: 11.10. 2. 3. 4:11.10, 13.2666
#### 20C: 1: 9.0273 2: 7.7849305 3., 7.786 4., 7.7903 5: 7.7889
#### 24C: 1: 7.8077, 2: 7.80, 3: 11.163 4., 3.958391 5., 11.165844
cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>%
	filter(temperature == 8, replicate == 5) %>%
	group_by(replicate) %>%
	do(tidy(nlsLM(cell_density ~ Vm * time_since_innoc_days/(K+time_since_innoc_days), data = .,
							start = list(K = max(.$time_since_innoc_days)/2, Vm = max(.$cell_density))))) %>%
	filter(term == "Vm") %>%
	# filter(replicate == 5) %>% 
	# filter(time_since_innoc_days < 17) %>% 
	# group_by(temperature, replicate, Unique_ID) %>% 
	ggplot(aes(x = time_since_innoc_days, y = cell_density, group = Unique_ID, color = factor(temperature))) + geom_point() +
	geom_line() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


### trying growthcurver

## 20, 841569, 762667, 511465, 755810, 755810, 507991
cells_sub <- cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 24, replicate == 3)
gc_fit <- SummarizeGrowth(cells_sub$time_since_innoc_days, cells_sub$cell_density)

str(gc_fit$vals)
gc_fit$vals$note

cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8) %>% 
	split(.$replicate) %>% 
	map(.x = ., .f = SummarizeGrowth, .$time_since_innoc_days, .$cell_density)

cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8, replicate == 1) %>%
	do(SummarizeGrowth(.$time_since_innoc_days, .$cell_density))
	
	
cells_8 <- cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8) %>%
	select(replicate, time_since_innoc_days, cell_density) %>%
	# mutate(time_since_innoc_days = as.character(time_since_innoc_days)) %>% 
	mutate(days = time_since_innoc_days %/% 1) %>% 
	select(-time_since_innoc_days) %>% 
	distinct(days, replicate, .keep_all = TRUE) %>% 
	spread(replicate, cell_density) %>% 
	rename(time = days)
	

gc_out <- SummarizeGrowthByPlate(cells_8)

cells_24 <- cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 24) %>%
	select(replicate, time_since_innoc_days, cell_density) %>%
	# mutate(time_since_innoc_days = as.character(time_since_innoc_days)) %>% 
	mutate(days = time_since_innoc_days %/% 1) %>% 
	select(-time_since_innoc_days) %>% 
	distinct(days, replicate, .keep_all = TRUE) %>% 
	spread(replicate, cell_density) %>% 
	rename(time = days)


gc_out24 <- SummarizeGrowthByPlate(cells_24)





View(gc_out)

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
	ggplot(aes(x = temperature, y = log(estimate))) + geom_point()

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

all_rates <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data/chlamy-ktemp-growth-rates.csv")

unique(all_rates$nutrient_level)

all_rates %>% 
	mutate(nutrient_level = str_replace(nutrient_level, "deficient", "low phosphorus")) %>% 
	mutate(nutrient_level = str_replace(nutrient_level, "replete", "high phosphorus")) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(estimate), group = nutrient_level, color = nutrient_level)) + geom_point(size = 6, alpha = 0.5) +
	geom_smooth(method = "lm") +
	scale_x_reverse() +
	xlab("inverse temperature (1/kT)") + 
	ylab("log population growth rate (r)") + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1)) +
	theme(legend.title=element_blank())+
	theme(legend.text = element_text(size = 16, face = "bold")) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))

	

all_rates %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	group_by(nutrient_level) %>% 
	do(tidy(lm(log(estimate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	filter(term == "inverse_temp") %>% View
	ggplot(data = ., aes(x = nutrient_level, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

	
	

# trying w/simecol --------------------------------------------------------

full <-	cells_full %>% 
		filter(!grepl("B", Unique_ID )) %>% 
		select(Unique_ID, cell_density, start_time, temperature, time_since_innoc_days, replicate) %>% 
		rename(P = cell_density) %>% 
		# filter(temperature == 12) %>% 
		arrange(Unique_ID) %>% 
		# filter(replicate == 1) %>% 
		rename(days = time_since_innoc_days) %>% 
		rename(ID = Unique_ID) %>% 
		filter(days < 22)
	
	
	controldata <- split(full, f = full$ID)
	
# set up models -----------------------------------------------------------
	
	
	Parameters <- c(r = 0.05, K = 10 ^ 5)
	
	# Declare the parameters to be used as the bounds for the fitting algorithm
	LowerBound <- c(r = 0.015, K = 10 ^ 7)
	UpperBound <- c(r = 2, K = 10 ^ 13) 
	
	# Declare the "step size" for the PORT algorithm. 1 / UpperBound is recommended
	# by the simecol documentation.
	ParamScaling <- 1 / UpperBound
	
	CRmodel <- new("odeModel",
								 main = function (time, init, parms) {
								 	with(as.list(c(init, parms)), {
								 		dp <-  r * P * (1 - (P / K))
								 		list(c(dp))
								 	})
								 },
								 parms = Parameters, # Trying Joey's empirically estimated parameters here.
								 times = c(from = 0, to = 22, by = 0.1), # the time interval over which the model will be simulated.
								 init = c(P = 561731438),
								 solver = "lsoda" #lsoda will be called with tolerances of 1e-9, as seen directly below. Default tolerances are both 1e-6. Lower is more accurate.
	)
	
	fittedparms <- c("r", "K") # for assigning fitted parameter values to fittedCRmodel
	
	controlfit <- function(data){
		
		init(CRmodel) <- c(P = data$P[1]) # Set initial model conditions to the biovolume taken from the first measurement day
		obstime <- data$days # The X values of the observed data points we are fitting our model to
		yobs <- select(data, P) # The Y values of the observed data points we are fitting our model to

		
		fittedCRmodel <- fitOdeModel(CRmodel, whichpar = fittedparms, obstime, yobs,
																 debuglevel = 0, fn = ssqOdeModel,
																 method = "PORT", lower = LowerBound, upper = UpperBound, scale.par = ParamScaling,
																 control = list(trace = T)
		)

		r <- coef(fittedCRmodel)[1]
		K <- coef(fittedCRmodel)[2]
		ID <- data$ID[1]
		output <- data.frame(ID, r, K)
		return(output)
	}
	
	
	# plot function -----------------------------------------------------------
	
	
	
	plotsinglefit <- function(data){
	
			
		init(CRmodel) <- c(P = data$P[1]) # Set initial model conditions to the biovolume taken from the first measurement day
		obstime <- data$days # The X values of the observed data points we are fitting our model to
		yobs <- select(data, P) # The Y values of the observed data points we are fitting our model to
		# parms(CRmodel)[TempName] <- data$temp[1] # Set the temperature parameter in CRmodel to whatever our control replicate used.
		
		# Below we fit a CRmodel to the replicate's data. The optimization criterion used here is the minimization of the sum of
		# squared differences between the experimental data and our modelled data. This
		# is fairly standard, although alternatives do exist.
		
		# The PORT algorithm is employed to perform the model fitting, analogous to O'Connor et al.
		# "lower" is a vector containing the lower bound constraints
		# for the parameter values. This may need tweaking.
		
		fittedCRmodel <- fitOdeModel(CRmodel, whichpar = fittedparms, obstime, yobs,
																 debuglevel = 0, fn = ssqOdeModel,
																 method = "PORT", lower = LowerBound, upper = UpperBound, scale.par = ParamScaling,
																 control = list(trace = T)
		)
		
		# To display the fitted results we need to create a new OdeModel object. Here
		# we duplicate CRmodel and then alter it to use our new fitted parameters.
		plotfittedCRmodel <- CRmodel
		parms(plotfittedCRmodel)[fittedparms] <- coef(fittedCRmodel)
		
		# set model parameters to fitted values and simulate again
		times(plotfittedCRmodel) <- c(from=0, to=21, by=1)
		ysim <- out(sim(plotfittedCRmodel, rtol = 1e-9, atol = 1e-9))
		
		# Form observed data into a dataframe; the simulated data are already in a dataframe
		observeddata <- data.frame(obstime, yobs)
		simulateddata <- ysim
		
		# Plot the results of our model fitting.
		biol_plot <- ggplot() +
			geom_point(data = observeddata, aes(x = obstime, y = yobs, color = "observed")) + # Observed data are points
			geom_line(data = simulateddata, aes(x = time, y = P, color = "simulated")) + # Simulated data are in a continuous line
			labs(x = "Time (days)", y = "Algal Biovolume")
		
		# Output the results as a ggplot2 object
		output <- biol_plot
		return(output)
	}



# fit and plot ------------------------------------------------------------
	Parameters <- c(r = 0.05, K = 10 ^ 5)
	
	# Declare the parameters to be used as the bounds for the fitting algorithm
	LowerBound <- c(r = 0.02, K = 10 ^ 3)
	UpperBound <- c(r = 2, K = 10 ^ 6) 
	
	# Declare the "step size" for the PORT algorithm. 1 / UpperBound is recommended
	# by the simecol documentation.
	ParamScaling <- 1 / UpperBound
	

	all_cells <-	cells %>% 
		filter(!grepl("B", Unique_ID )) %>% 
		select(Unique_ID, cell_density, start_time, temperature, time_since_innoc_days, replicate) %>% 
		rename(P = cell_density) %>% 
		# filter(temperature == 12) %>% 
		arrange(Unique_ID) %>% 
		# filter(replicate == 1) %>% 
		rename(days = time_since_innoc_days) %>% 
		rename(ID = Unique_ID) %>% 
		filter(days < 22)
	
	
	full <-	cells_full %>% 
		filter(!grepl("B", Unique_ID)) %>% 
		select(Unique_ID, cell_density, start_time, temperature, time_since_innoc_days, replicate) %>% 
		rename(P = cell_density) %>% 
		# filter(temperature < 20) %>% 
		arrange(Unique_ID) %>% 
		# filter(replicate == 1) %>% 
		rename(days = time_since_innoc_days) %>% 
		rename(ID = Unique_ID) %>% 
		filter(days < 22)
	
	
	controldata <- split(all_cells, f = all_cells$ID)
		
output <- controldata %>% 
		map_df(controlfit)
	
	full %>% 
		filter(ID == 45) %>% 
		plotsinglefit(.)
	
	## looks like rep 3 is missing time initial data