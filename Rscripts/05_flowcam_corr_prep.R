### Wrangling the new PK temp chlamy data


library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)


#### Step 1 #### 
## in the shell, use this command to copy the summary files from the folder of flow cam run folders to a summary-only file
## cp **/*summary.csv /Users/Joey/Desktop/run-summaries

## step 1b, read in UniqueID key

Unique_ID_key <- read_csv("/Users/Joey/Documents/chlamy-ktemp/k-temp/data-raw/PK-temp-UniqueID-key.csv")

#### Step 2: create a list of file names for each of the summaries ####
fnams <- list.files("/Users/Joey/Documents/chlamy-ktemp/k-temp/PK-TEMP-SUMMARIES", full.names = TRUE) ## find out the names of all the files in data-summary, use full.names to get the relative path for each file
fnams
#### Step 3: create a df with the dataset ID and the cell count ####
ptemp_summaries <- fnams %>%  
	lapply(FUN = function(p) read.csv(p, nrows = 6)) %>% 
	as.data.frame(.) %>%
	mutate(List.File = as.character(List.File)) %>% 
	filter(List.File == "Particles / ml" | List.File == "Start Time") %>% 
	select(- starts_with("List")) %>% 
	t(.) %>%
	as.data.frame() %>%
	mutate(dataset = rownames(.)) %>%
	rename(start_time = V1,
				 cell_count = V2) %>% 
	mutate(cell_count = as.numeric(as.character(cell_count))) 
	
ptemp_sep<- separate(ptemp_summaries, dataset, c("replicate", "date", "copy"), extra = "drop") %>% 
	select(-copy) %>%
	separate(replicate, c("x", "Unique_ID"), sep = 1) %>% 
	select(-x)

glimpse(ptemp_sep) ## eesh we notice that the date is not in the right format!

## let's get the date in the right format
ptemp_sep$start_time <- ymd_hms(ptemp_sep$start_time)

glimpse(ptemp_sep)

### now join with the UniqueID key
ptemp_sep$Unique_ID <- as.character(ptemp_sep$Unique_ID)
Unique_ID_key$Unique_ID <- as.character(Unique_ID_key$Unique_ID)

length(unique(Unique_ID_key$Unique_ID))
length(unique(ptemp_sep$Unique_ID))

ptemp_all <- left_join(ptemp_sep, Unique_ID_key)

ptemp_all %>% 
	group_by(Unique_ID) %>% 
ggplot(data = ., aes(x = start_time, y = log(cell_count), color = factor(temperature))) + geom_point()
	

