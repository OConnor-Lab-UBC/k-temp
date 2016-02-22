library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)

chlamy_summary <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data-summary/cl-1-20_summary.csv", skip = 4, n_max = 1)
str(chlamy_summary)


fnams <- list.files("data-summary", full.names = TRUE) ## find out the names of all the files in data-summary, use full.names to get the relative path for each file
fnams

chlamy_summaries <- fnams %>%  
		lapply(FUN = function(p) read.csv(p)) %>% 
		as.data.frame(.) %>% 
	filter(List.File == "Particles / ml") %>% 
	select(- starts_with("List")) %>%
	t(.) %>%
	as.data.frame() %>%
	mutate(dataset = rownames(.)) %>%
	mutate(cell_count = as.numeric(as.character(V1))) %>%
	select(-V1)

chlamy_summaries %>% 
	ggplot(., aes(x = dataset, y = cell_count)) + geom_bar(stat= "identity") + theme(axis.text.x = element_text(size = 20, angle = 90, hjust = 1))
	



# separate(chlamy_summaries, dataset, c("name", "replicate", "temperature", "date", "type"), extra = "drop",  sep = ".") %>% View
