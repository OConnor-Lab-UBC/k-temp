CEdat <- data.frame(time = 1:45, obs = c(0.3061324, 1e-05, 0.2361211, 0.505824, 
																				 2.0685032, 2.1944544, 4.2689494, 4.9508297, 3.133472, 3.6570752, 5.6753381, 
																				 10.9133183, 5.4518257, 20.4166979, 15.9741054, 19.0970426, 13.7559959, 14.1358153, 
																				 15.9986416, 29.6762828, 10.3760667, 8.4284488, 6.1060359, 3.7099982, 3.358406, 
																				 2.5981386, 2.5697082, 2.8091952, 5.5487979, 1.6505442, 2.2696972, 2.1835692, 
																				 3.6747876, 4.8307886, 3.5019731, 2.8397137, 1.8605288, 11.1848738, 2.6268683, 
																				 4.1215127, 2.399621, 2.6569938, 2.1987387, 3.0267252, 2.4420927))


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
	
