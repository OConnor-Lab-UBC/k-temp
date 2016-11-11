### Wrangling the new PK temp chlamy data
### updated Nov 10


# load libraries ----------------------------------------------------------


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

cell_files <- list.files("/Users/Joey/Documents/chlamy-ktemp/k-temp/PK-TEMP-SUMMARIES", full.names = TRUE)  ## find out the names of all the files in data-summary, use full.names to get the relative path for each file


names(cell_files) <- cell_files %>% 
	gsub(pattern = ".csv$", replacement = "")

all_cells <- map_df(cell_files, read_csv, col_names = FALSE,
										.id = "file_name")


pk_temp <- all_cells %>% 
	rename(obs_type = X1,
				 value = X2) %>% 
	filter(obs_type %in% c("List File", "Start Time", "Particles / ml", "Volume (ABD)")) %>%
	spread(obs_type, value) %>%
	separate(`List File`, into = c("replicate", "other"), sep = "[:punct:]") %>% 
	rename(start_time = `Start Time`,
				 cell_density = `Particles / ml`,
				 cell_volume = `Volume (ABD)`)

pk_temp$start.time <- ymd_hms("2016-06-30 14:15:43")
pk_temp$start.time[str_detect(pk_temp$replicate, "B")] <- ymd_hms("2016-07-09 9:15:43") ### change the innoculation time for the B cultures, since they were started later

pk_temp$time_since_innoc <- interval(pk_temp$start.time, pk_temp$start_time)



ptemp_all_2 <- pk_temp %>% 
	mutate(time_since_innoc_days = time_since_innoc/ddays(1)) %>% 
	mutate(time_since_innoc_hours = time_since_innoc/dhours(1)) %>%
	mutate(cell_density = as.numeric(cell_density),
				 cell_volume = as.numeric(cell_volume)) %>% 
	mutate(total_biovolume = cell_density * cell_volume) %>% 
	rename(Unique_ID = replicate)

Unique_ID_key <- Unique_ID_key %>% 
	mutate(replicate = as.character(replicate))



ptemp_all <- left_join(ptemp_all_2, Unique_ID_key)


### write out the correct csv file!! yay!
write_csv(ptemp_all, "data/ptemp_all_3.csv")




