
# load libraries ----------------------------------------------------------

library(tidyverse)

# read in data ------------------------------------------------------------
CEdat<- read_csv("data/ChlamyKtemp_aftersorting.csv") %>%
	select(-time) %>% 
	rename(time = day, 
				 obs = density) %>% 
	filter(time != 14)


library(ggplot2)
theme_set(theme_bw())

CEdat %>% 
	filter(temperature == "20") %>% 
ggplot(aes(x=time,y=obs), data = .) + geom_point() +
		labs(y="Abundance",x = "days") + facet_wrap( ~ replicate)


CE.mod <- nls(obs ~ 
								SSlogis(time, Asym, xmid, scal), 
							data=subset(CEdat, temperature == 16))
summary(CE.mod)

update(CE.mod,data=CEdat)
getInitial(obs~SSlogis(time,Asym,xmid,scal), data= CEdat)


newstart <- list(Asym = max(CEdat$obs),
								 xmid = mean(CEdat$time),
								 scal=1)
CE.fullmod <- update(CE.mod,data=CEdat,
										 start=newstart)
summary(CE.fullmod)


CEsub <- CEdat %>% 
	filter(temperature == "24", replicate == "1")

CE.mod <- nls(obs ~ 
								SSlogis(time, Asym, xmid, scal), 
							data=CEsub)
	
newstart <- list(Asym = max(CEsub$obs),
								 xmid = mean(CEsub$time),
								 scal=1)
CE.fullmod <- update(CE.mod,data=CEsub,
										 start=newstart)
summary(CE.fullmod)

library(plotrix)
estimates <- CEdat %>% 
	group_by(temperature) %>% 
	do(tidy(nls(obs ~ 
								SSlogis(time, Asym, xmid, scal), 
							data=.))) 


estimates %>% 
	filter(term == "Asym") %>% 
	lm(log(estimate) ~ temperature, data = .) %>% 
	summary
	ggplot(data = ., aes(x = temperature, y = estimate)) + geom_point() + facet_wrap( ~ term, scales = "free") +
	geom_errorbar(aes(ymin = estimate - std.error, ymax= estimate + std.error))
	
