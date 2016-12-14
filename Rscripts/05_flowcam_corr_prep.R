### Wrangling the new PK temp chlamy data


library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)


#### Step 1 #### 
## in the shell, use this command to copy the summary files from the folder of flow cam run folders to a summary-only file
## cp **/*summary.csv /Users/Joey/Desktop/run-summaries

## step 1b, read in UniqueID key

Unique_ID_key <- read.csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data-raw/PK-temp-UniqueID-key.csv")

#### Step 2: create a list of file names for each of the summaries ####
fnams <- list.files("/Users/Joey/Documents/chlamy-ktemp/k-temp/PK-TEMP-SUMMARIES", full.names = TRUE) ## find out the names of all the files in data-summary, use full.names to get the relative path for each file
# fnams <- list.files("/Users/Joey/Documents/chlamy-ktemp/k-temp/AUG13-summaries", full.names = TRUE) ## find out the names of all the files in data-summary, use full.names to get the relative path for each file

fnams

fnams_no_copy <- fnams[!str_detect(fnams, "copy")]
fnams_no_copy <- fnams_no_copy[!str_detect(fnams_no_copy, "/Users/Joey/Documents/chlamy-ktemp/k-temp/PK-TEMP-SUMMARIES/10_summary.csv")]



# fnams_no_copy <- fnams_no_copy[!str_subset(fnams_no_copy, "/Users/Joey/Documents/chlamy-ktemp/k-temp/PK-TEMP-SUMMARIES/1-R2_copy.csv")]
# fnams_no_copy <- fnams_no_copy[!str_detect(fnams_no_copy, "NA")]
# fnams_no_copy <- fnams[1:612] ## for some reason there's a bunch of NAs here, so I'm eliminating them

## weirdo files:
1-R2_copy.csv
2-R2_copy.csv
6_copy
8_copy
21_copy
27_copy
36_copy
23_R3_copy
37_copy
38_copy
39_copy
44_copy
45_copy
46_copy
47_copy
48_copy
49-R2_copy
50_copy

ptemp_summaries[[165]]
?read.csv
#### Step 3: create a df with the dataset ID and the cell count ####
ptemp_summaries <- fnams_no_copy %>%  
	lapply(FUN = function(p) read.csv(p, nrows = 4)) %>% 
	as.data.frame(.) %>%
	mutate(List.File = as.character(List.File)) %>% 
	filter(List.File == "Particles / ml" | List.File == "Start Time") %>% 
	# filter(List.File %in% c("Particles / ml", "Start Time", "Volume (ABD)")) %>%
	select(- starts_with("List")) %>% 
	t(.) %>% 
	as.data.frame() %>% 
	mutate(dataset = rownames(.)) %>%
	rename(start_time = V1,
				 cell_count = V2) %>% 
	mutate(cell_count = as.numeric(as.character(cell_count)))
	

#### Step 3 -- messing around with the data import code b/c some of the files don't want to load properly!
# file_list <- fnams_no_copy %>%
# 	map(read_csv, n_max = 4) %>% 
# 	map_df( ~ tibble()) %>% View
# 	as.data.frame()
# 	map(.f = filter_(.$`List File` == "Count"))
# 	glimpse(file_list)
# file_list[[12]]

ptemp_sep<- separate(ptemp_summaries, dataset, c("replicate", "date", "copy"), extra = "drop") %>% 
	select(-copy) %>% 
	filter(start_time != "do") %>%
	separate(replicate, c("x", "Unique_ID"), sep = 1) %>%
	select(-x) 

fnams

glimpse(ptemp_sep) ## eesh we notice that the date is not in the right format!

## let's get the date in the right format
ptemp_sep$start_time <- ymd_hms(ptemp_sep$start_time)

glimpse(ptemp_sep)

### now join with the UniqueID key
ptemp_sep$Unique_ID <- as.character(ptemp_sep$Unique_ID)
Unique_ID_key$Unique_ID <- as.character(Unique_ID_key$Unique_ID)
Unique_ID_key$unique_number <- as.numeric(rownames(Unique_ID_key))


length(unique(Unique_ID_key$Unique_ID))
length(unique(ptemp_sep$Unique_ID))

ptemp_all <- left_join(ptemp_sep, Unique_ID_key)

glimpse(ptemp_all)

?lubridate
??duration



# ptemp_all %>% 
# 	mutate(time_since_innoc = duration(start_time, ymd_hms(2016-06-30 14:15:43))) %>% View

ptemp_all$start.time <- ymd_hms("2016-06-30 14:15:43")

ptemp_all$time_since_innoc <- interval(ptemp_all$start.time, ptemp_all$start_time)


ptemp_all_2 <- ptemp_all %>% 
	mutate(time_since_innoc_days = time_since_innoc/ddays(1)) %>% 
	mutate(time_since_innoc_hours = time_since_innoc/dhours(1)) %>% 
	mutate(total_biovolume = cell_count * biovolume)


write_csv(ptemp_all_2, "data/ptemp_all_2.csv")
ptemp_all_2 <- read.csv( "data/ptemp_all_2.csv")

ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	# filter(unique_number < 51) %>% 
	# filter(temperature == 24) %>% 
	# filter(time_since_innoc_days > 40) %>%
	ggplot(data = ., aes(x = time_since_innoc_days, y = total_biovolume, color = factor(temperature))) + geom_point(size = 4) +
	scale_y_log10() +
	theme_bw()


library(plotrix)

ptemp_all_2 %>% 
	mutate(day = day(start_time),
				 month = month(start_time),
				 year = year(start_time)) %>%
	unite(month_day, month, day, year) %>% 
	group_by(month_day, P.treatment, temperature) %>% 
	filter(unique_number < 51) %>% 
	summarize_each(funs(mean, std.error), cell_count) %>%
	mutate(month_date = mdy(month_day)) %>%
	group_by(temperature, P.treatment) %>%
	filter(!is.na(P.treatment)) %>%
	ggplot(data = ., aes(x = month_date, y = mean, color = factor(temperature))) + geom_point(aes(shape = P.treatment), size = 4) +
	geom_errorbar(aes(ymin = mean - std.error, ymax= mean + std.error)) + 
scale_y_log10() +
	theme_bw() +
	geom_line(aes(linetype = P.treatment), size = 1.5) + xlab("date") + ylab("log(abundance)") +
	facet_wrap( ~ P.treatment)
ggsave("figures/log_cell_abundance_vs_time_ktemp.png")


	
library(broom)
ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	filter(unique_number < 51) %>% 
	filter(temperature == 24 | time_since_innoc_hours < 100) %>%
	filter(temperature == 20 | time_since_innoc_hours < 200) %>%
	filter(temperature == 16 | time_since_innoc_hours < 250) %>%
	filter(temperature == 12 | time_since_innoc_hours < 1000) %>%
	filter(temperature == 8 | time_since_innoc_hours < 1000) %>%
	group_by(temperature) %>% 
	do(tidy(lm(log(cell_count) ~ time_since_innoc_days, data = .), conf.int = TRUE)) %>%
	filter(term != "(Intercept)") %>% View





r.24 <- ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	filter(unique_number < 51) %>% 
	filter(temperature == 24) %>% 
	filter(time_since_innoc_hours < 100) %>% 
	group_by(P) %>% 
	do(tidy(lm(log(cell_count) ~ time_since_innoc_days, data = .), conf.int = TRUE)) 


r.20 <- ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	filter(unique_number < 51) %>% 
	filter(temperature == 20) %>% 
	filter(time_since_innoc_hours < 200) %>% 
	group_by(P) %>% 
	do(tidy(lm(log(cell_count) ~ time_since_innoc_days, data = .), conf.int = TRUE)) 

r.16 <- ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	filter(unique_number < 51) %>% 
	filter(temperature == 16) %>% 
	filter(time_since_innoc_hours < 250) %>% 
	group_by(P) %>% 
	do(tidy(lm(log(cell_count) ~ time_since_innoc_days, data = .), conf.int = TRUE)) 


r.12 <- ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	filter(unique_number < 51) %>% 
	filter(temperature == 12) %>% 
	group_by(P) %>% 
	# filter(time_since_innoc_hours < 250) %>% 
	do(tidy(lm(log(cell_count) ~ time_since_innoc_days, data = .), conf.int = TRUE)) 

r.8 <- ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
	# filter(P.treatment == "FULL") %>% 
	filter(unique_number < 51) %>% 
	filter(temperature == 8) %>% 
	group_by(P) %>% 
	# filter(time_since_innoc_hours < 250) %>% 
	do(tidy(lm(log(cell_count) ~ time_since_innoc_days, data = .), conf.int = TRUE))

r.24$temperature <- 24
r.20$temperature <- 20
r.16$temperature <- 16
r.12$temperature <- 12
r.8$temperature <- 8

r.all <- bind_rows(r.24, r.20, r.16, r.12, r.8) %>% 
	filter(term != "(Intercept)")

ggplot(data = r.all, aes(x = temperature, y = estimate, group = P, color = P)) + geom_point(size = 10) +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 1)) + ylab("intrinsic growth rate, r") + xlab("temperature, C") +
	theme_bw() +
	theme(
		# panel.border = element_blank(),
		# legend.key = element_blank(),
		# axis.ticks = element_blank(),
		# axis.text.y = element_blank(),
		# axis.text.x = element_blank(),
		# panel.grid = element_blank(),
		panel.grid.minor = element_blank(), 
		panel.grid.major = element_blank(),
		panel.background = element_blank(),
		plot.background = element_rect(fill = "transparent",colour = NA)) + 
	theme(axis.text=element_text(size=24),
				axis.title=element_text(size=20,face="bold"))

ggsave("test.png", bg = "transparent")







	ptemp_all_2 %>% 
	mutate(P = as.factor(P.treatment)) %>% 
		# filter(P.treatment == "FULL") %>% 
		filter(temperature == 16) %>% 
		filter(unique_number < 51) %>%
	ggplot(data = ., aes(x = time_since_innoc_hours, y = cell_count, color = factor(temperature), group = Unique_ID)) + geom_point(size = 4) +
		geom_line(aes(linetype = P), size = 1.5) +
		scale_y_log10() +
		ylab("log abundance") + xlab("Time, hours") +
		theme_bw() +
		theme(
			axis.ticks = element_line(color = "white"),
			panel.grid.minor = element_blank(), 
			panel.grid.major = element_blank(),
			panel.background = element_blank(),
			panel.border = element_rect(color = "white"),
			plot.background = element_rect(fill = "transparent",colour = NA)) + 
		theme(legend.background = element_rect(fill = "transparent"), legend.margin = unit(1, "cm")) +
		theme(legend.text = element_text(size = 20, colour = "white")) +
		theme(axis.text.x=element_text(size=30, color = "white"),
					axis.text.y=element_text(size=30, color = "white"),
					axis.title=element_text(size=30,face="bold", color = "white"),
					legend.key = element_rect(fill = "transparent", colour = "transparent")) +
		theme(legend.position="top") + 
		theme(legend.title=element_blank())
	
	ggsave("growth_curves.png", bg = "transparent", width = 8, height = 8)
	
	
	ptemp_all_2 %>%
		filter(!is.na(temperature)) %>% 
		mutate(P.treatment = str_replace(P.treatment, "DEF", "low phosphorus")) %>% 
		mutate(P.treatment = str_replace(P.treatment, "FULL", "high phosphorus")) %>% 
		mutate(`nutrient level` = as.factor(P.treatment)) %>% 
		# filter(P.treatment == "FULL") %>% 
		filter(temperature < 20) %>% 
		filter(unique_number < 51) %>%
		ggplot(data = ., aes(x = time_since_innoc_hours, y = cell_count, color = `nutrient level`, group = Unique_ID)) + geom_point(size = 6, alpha = 0.5) +
		geom_line(size = 1.5) +
		scale_y_log10() +
		facet_wrap( ~ temperature) +
		ylab("log population abundance") + xlab("Time, hours") +
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
	
	
	
	
	P.temp16 <- ptemp_all_2 %>% 
		mutate(P = as.factor(P.treatment)) %>% 
		# filter(P.treatment == "FULL") %>% 
		filter(temperature == 16)
	
	levels(P.temp16$P.treatment)[levels(P.temp16$P.treatment) == "DEF"] <- "Nutrient limited"
	levels(P.temp16$P.treatment)[levels(P.temp16$P.treatment) == "FULL"] <- "Nutrient replete"
	

	
	
	P.temp16 %>% 
	ggplot(data = ., aes(x = time_since_innoc_hours, y = cell_count, color = P.treatment, group = Unique_ID)) + geom_point(size = 4) +
		geom_line(size = 1.5) +
		scale_colour_manual(values= c("darkolivegreen3", "cadetblue2")) + 
		scale_y_log10() +
		ylab("log abundance") + xlab("Time, hours") +
		theme_bw() +
		theme(
			axis.ticks = element_line(color = "white"),
			panel.grid.minor = element_blank(), 
			panel.grid.major = element_blank(),
			panel.background = element_blank(),
			panel.border = element_rect(color = "white"),
			plot.background = element_rect(fill = "transparent",colour = NA)) + 
		theme(legend.background = element_rect(fill = "transparent"), legend.margin = unit(1, "cm")) +
		theme(legend.text = element_text(size = 20, colour = "white")) +
		theme(axis.text.x=element_text(size=30, color = "white"),
					axis.text.y=element_text(size=30, color = "white"),
					axis.title=element_text(size=30,face="bold", color = "white"),
					legend.key = element_rect(fill = "transparent", colour = "transparent")) +
		theme(legend.position="top") + 
		theme(legend.title=element_blank())
	
	ggsave("carrying_capacity_16.png", bg = "transparent", width = 8, height = 8)
	
