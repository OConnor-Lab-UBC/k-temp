#Logistic growth fitting using nls and bounds with port

#Required improvements
#SSlogis function with integrated bounds
#Confidence intervals of parameters using confint)


library(broom)

cells <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data/ptemp_all_3.csv")

readings <- cells %>% 
	filter(P.treatment == "FULL") 

cells_def <- cells %>% 
	filter(P.treatment == "DEF") 





logistic.growth.nls<-function(readings, upper=10, printer=T, do.interval=T){
	
	if(printer){print(unique(readings$replicate))}
	
	fitted.readings<-readings
	
	
	#lower.v=c(0,0,0)
	#upper.v=c(upper,Inf, Inf)
	#start.values<-getInitial(ABS~SSlogis(Time,Asym, xmid, scal), data=readings)
	#start.values[start.values<=lower.v]<-10^-5
	#start.values[start.values>=upper.v]<-upper.v[start.values>=upper.v]-10^-5
	#
	#Initially used ABS~SSlogis, but it would often start out of bounds
	
	start.values=c("K"=1, "r"=1, "N0"=0.01)
	
	
	culture.model <- try(nls(formula=ABS ~(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*Time)-1)),
													 data=readings,
													 start=start.values,
													 na.action=na.exclude,
													 algorithm="port",
													 lower=c(0,0,0),
													 upper=c(upper,Inf, Inf)))
	
	if(class(culture.model)=="try-error"){
		
		fitted.readings$logistic.nls.N0<-NA
		
		fitted.readings$logistic.nls.K<-NA
		fitted.readings$logistic.nls.K.lower<-NA
		fitted.readings$logistic.nls.K.upper<-NA
		
		fitted.readings$logistic.nls.r<-NA
		fitted.readings$logistic.nls.r.lower<-NA
		fitted.readings$logistic.nls.r.upper<-NA
		
		fitted.readings$logistic.nls.predicted<-NA
		
		
	}else{
		
		#extract the parameters from the model
		parameters<-coef(culture.model)
		#summary.culture.model<-coef(summary(culture.model))
		#print(summary.culture.model)
		
		
		#This was for use with SSlogis
		#asign the value to each parameter
		#     Asym<-parameters[1]
		#     xmid<-parameters[2]
		#     scal<-parameters[3]
		
		#convert parameters to those used in ecological growth equations
		#     N0<-Asym/(1+exp(xmid/scal))
		#     K<-Asym
		#     r<-  1/scal
		
		
		
		
		fitted.readings$logistic.nls.N0<-parameters["N0"]
		
		fitted.readings$logistic.nls.K<-parameters["K"]
		
		
		fitted.readings$logistic.nls.r<-parameters["r"]
		
		
		fitted.readings$logistic.nls.predicted<-predict(culture.model)
		
		interval<-try(confint(culture.model))   
		if(any(class(interval)=="try-error", !do.interval)){
			fitted.readings$logistic.nls.K.lower<-NA
			fitted.readings$logistic.nls.K.upper<-NA
			fitted.readings$logistic.nls.r.lower<-NA
			fitted.readings$logistic.nls.r.upper<-NA}else{
				fitted.readings$logistic.nls.r.lower<-interval["r", "2.5%"]
				fitted.readings$logistic.nls.r.upper<-interval["r", "97.5%"]
				fitted.readings$logistic.nls.K.lower<-interval["K", "2.5%"]
				fitted.readings$logistic.nls.K.upper<-interval["K", "97.5%"]}
		
	}
	
	return(fitted.readings)
}


cells_sample <- cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8) %>% 
	filter(replicate == 5) %>% 
	filter(time_since_innoc_days < 17) %>% 
	rename(ABS = cell_density, 
				 Time = time_since_innoc_days)


start.values=c("K"=1, "r"=1, "N0"=0.01)
culture.model <- try(nls(formula=ABS ~(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*Time)-1)),
												 data=cells_sample,
												 start=start.values,
												 na.action=na.exclude,
												 algorithm="port",
												 lower=c(0,0,0),
												 upper=c(Inf,Inf, Inf)))



parameters<-coef(culture.model)
parameters

start.values=c("K"=1, "r"=1, "N0"=100)
cells_sub <- cells_full %>% 
	filter(!grepl("B", Unique_ID )) %>% 
	filter(temperature == 8) %>% 
	# filter(replicate == 3) %>% 
	filter(time_since_innoc_days < 17) %>% 
	rename(ABS = cell_density, 
				 Time = time_since_innoc_days) %>% 
	filter(!is.na(ABS))

	mod <- nls(formula=ABS ~(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*Time)-1)),
			 data= cells_sub,
			 start=start.values,
			 na.action=na.exclude,
			 algorithm="port",
			 lower=c(0,0,0),
			 upper=c(Inf,Inf, Inf))


%>% 
	filter(term == "K") %>% View
	ggplot(aes(x = temperature, y = estimate)) + geom_point()

	culture.model <- try(nls(formula=ABS ~(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*time)-1)),
													 data= cells_sub,
													 start=start.values,
													 na.action=na.exclude,
													 algorithm="port",
													 lower=c(0,0,0),
													 upper=c(2*max(ABS, na.rm=T),Inf, Inf)))

	
	
	
	
# try again ---------------------------------------------------------------

	#' logistic_growth_nls
	#'
	#' Least-squares estimates of the parameters of a logistic growth curve
	#'
	#' @param time Numeric vector of time since start of measurement (currently does not use date format)
	#' @param abundance Abundance (eg. Optical density, counts etc).
	#' @param do.interval Logical: should a 95\% confidence interval be produced
	#' @keywords growth
	#' @examples
	#' data(navicula_growth)
	#' fitted.readings <- with(navicula_growth,
	#'                        logistic_growth_nls(time=Time,
	#'                                            abundance=ABS))
	#' head(fitted.readings)
	#' @export
	
	

	logistic_growth_nls<-function(time, abundance, do.interval=F,r=NULL,
																K=NULL,
																N0=NULL){
		
		if(is.null(r)){
			start.values=c("K"=max(abundance, na.rm=T), "r"=1, "N0"=min(abundance, na.rm=T))}else{start.values=c("K"=K,"r"=r,"N0"=N0)}
		
			logistic_growth_nls<-function(time, abundance, do.interval=F,
																		r=NULL,
																		K=NULL,
																		N0=NULL){
				
				r_start <- try(as.numeric(coef(lm(log(abundance)~time))[2]), silent = T)
				if(class(r_start)=="try-error")r_start <- 0.5
				
				if(is.null(r)){
					start.values = c(K = max(abundance, na.rm = T),
													 r = r_start, 
													 N0 = max(c(min(abundance, na.rm = T),10^-3)))}else{
													 	start.values = c(K = K,
													 									 r = r, 
													 									 N0=N0)}

				
				
				culture.model <- try(nls(formula=abundance ~(K*N0*exp(r*time) ) / (K + N0 * (exp(r*time)-1)),
																 start=start.values,
																 na.action=na.exclude,
																 algorithm="port",
																 lower=c(0,0,0),
																 upper=c(2*max(abundance, na.rm=T),Inf, Inf)))
				
				fitted.readings <- data.frame(time,abundance)
				
				if(class(culture.model)=="try-error"){
					
					fitted.readings$N0<-NA
					fitted.readings$K<-NA
					fitted.readings$K.lower<-NA
					fitted.readings$K.upper<-NA
					fitted.readings$r<-NA
					fitted.readings$r.lower<-NA
					fitted.readings$r.upper<-NA
					fitted.readings$predicted<-NA
					
					
				}else{
					
					#extract the parameters from the model
					parameters<-coef(culture.model)    
					fitted.readings$N0<-parameters["N0"]
					fitted.readings$K<-parameters["K"]
					fitted.readings$r<-parameters["r"]
					fitted.readings$predicted<-predict(culture.model)
					
					
					#add confidence interval
					interval<-try(confint(culture.model))   
					if(all(class(interval)=="try-error", do.interval)){
						fitted.readings$K.lower<-NA
						fitted.readings$K.upper<-NA
						fitted.readings$r.lower<-NA
						fitted.readings$r.upper<-NA}
					
					if(all(!class(interval)=="try-error", do.interval)){
						fitted.readings$r.lower<-interval["r", "2.5%"]
						fitted.readings$r.upper<-interval["r", "97.5%"]
						fitted.readings$K.lower<-interval["K", "2.5%"]
						fitted.readings$K.upper<-interval["K", "97.5%"]}
					
				}
				
				return(fitted.readings)
			}
			
			
		r
	