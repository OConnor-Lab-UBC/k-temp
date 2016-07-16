###

library(readr)
library(car)
library(ggplot2)
library(dplyr)
library(minpack.lm)

data <- read_csv("data/ChlamyKtemp_aftersorting.csv")

data.s <- data %>% 
	filter(temperature == 24) %>% 
	filter(replicate == 1)

min(data$density)

data %>% 
	filter(temperature == 16) %>% 
	# filter(temperature != 16 | day != 14) %>% 
	ggplot(data = ., aes(x = day, y = density)) + geom_point() + facet_wrap( ~ replicate)


data %>% 
	filter(temperature == 16) %>% 
	ggplot(data = ., aes(x = day, y = density)) + geom_point() + facet_wrap( ~ replicate)

library(broom)
library(plotrix)
data %>% 
	filter(day < 25) %>% 
	filter(temperature != 18 | day != 14) %>%
	group_by(temperature, replicate) %>% 
do(tidy(nls(density~ 80000 * (1+a)^(day), data= .,  start=list(a=0.01), control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
	group_by(temperature) %>%
	summarise(mean.r = mean(estimate),
						std.err = std.error(estimate)) %>%
	ggplot(data = ., aes(x = temperature, y = mean.r)) +
	geom_errorbar(aes(ymin=mean.r-std.err, ymax=mean.r+std.err), width=.2) +
	geom_point(size = 6) + theme_bw() + ylab("intrinsic growth rate, r") + xlab("temperature, C") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=16,face="bold"))


### K
data %>% 
	filter(day > 25) %>% 
	group_by(temperature, replicate) %>%
	summarise(mean.k = mean(log(density)),
						std.err = std.error(log(density))) %>%
	ggplot(data = ., aes(x = temperature, y = mean.k)) +
	geom_errorbar(aes(ymin=mean.k-std.err, ymax=mean.k+std.err), width=.2) +
	geom_point(size = 6) + theme_bw() + ylab("carrying capacity, K, log cell density") + xlab("temperature, C") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=16,face="bold"))


data %>% 
	filter(day > 25) %>% 
	group_by(temperature, replicate) %>% 
	summarise(mean.k = mean(log(density))) %>%
	ggplot(data = ., aes(x = temperature, y = mean.k)) + geom_point() +
	geom_smooth(method = "lm") +
	geom_point(size = 3) + theme_bw() + ylab("carrying capacity, K, log cell density") + xlab("temperature, C") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=16,face="bold"))
						
						
						
						std.err = std.error(mean.k)) %>%
	ggplot(data = ., aes(x = temperature, y = k)) +
	geom_errorbar(aes(ymin=k-std.err, ymax=k+std.err), width=.2) +
	geom_point(size = 6) + theme_bw() + ylab("carrying capacity, K, log cell density") + xlab("temperature, C") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=16,face="bold"))
	
	ggplot(data = ., aes( x = temperature, y = log(density))) + geom_point() +
	geom_smooth(method = "lm")



ggplot(data = data.s, aes(x = day, y = log(density))) + geom_point() + facet_wrap( ~ temperature)
	

max(data.s$density)


#### Following Ben Bolker's code ####


data.s <- data %>% 
	filter(temperature == 24) %>% 
	filter(replicate == 1) %>% 
	filter(day < 25)


CE.mod <- nls(density ~ 
								SSlogis(day, Asym, xmid, scal), 
							data=data.s)

newstart <- list(Asym = max(data.s$density),
								 xmid = mean(data.s$day),
								 scal=1)
full.mod <- update(CE.mod,data=data.s,
										 start=newstart)


##### Other stuff ####







plot(mass~days.since.birth, data=data) #always look at your data first!

coef(lm(logit(mass/100)~days.since.birth,data=data))
coef(lm(logit(density/3038175)~day, data=data.s))

algae<-nls(density~phi1/(1+exp(-(phi2+phi3*day))),
						start=list(phi1=2822203,phi2= -1.00781565,phi3= 0.04091668),data=data.s,trace=TRUE)
#build the model, start is the starting parameters, trace=TRUE will return the iterations

algae(nls(density ~ phi1/(1+exp(-(phi2+phi3*day)))))
log.ss <- nls(density ~ SSlogis(day, phi1, phi2, phi3))

getInitial(density ~ SSlogis(day, Asym, xmid, scal), data = data.s)


curve <- nls(density~ 75 * (1+a)^(day), data= data.s,  start=list(a=0.01), control = nls.control(maxiter=100, minFactor=1/204800000))

summary(curve)


summary(algae)

#set parameters
phi1<- coef(wilson)[1]
phi2<-coef(wilson)[2]
phi3<-coef(wilson)[3]
x<-c(min(data$days.since.birth):max(data$days.since.birth)) #construct a range of x values bounded by the data
y<-phi1/(1+exp(-(phi2+phi3*x))) #predicted mass
predict<-data.frame(x,y) #create the prediction data frame#And add a nice plot (I cheated and added the awesome inset jpg in another program)
ggplot(data=data,aes(x=days.since.birth,y=mass))+
	geom_point(color='blue',size=5)+theme_bw()+
	labs(x='Days Since Birth',y='Mass (lbs)')+
	scale_x_continuous(breaks=c(0,250,500,750, 1000,1250))+
	scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80))+
	theme(axis.text=element_text(size=18),axis.title=element_text(size=24))+
	geom_line(data=predict,aes(x=x,y=y), size=1)

