## Joey Bernhardt
## February 19 2016
## Loading and cleaning flowcam data



## load packages
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(magrittr)
library(purrr)    ## an elegant package for manipulating vectors and lists
library(readr)    ## (one of) the fastest ways to read in huge csv files


## Step 1: make a list of all the flow cam run files (i.e. all the csv files in data-raw/) 

fnams <- list.files("data-raw", full.names = TRUE) ## find out the names of all the files in data-raw, use full.names to get the relative path for each file
fnams
nfiles <- length(fnams) ## find out how many files there in data-raw
nfiles

## Step 2: Name the the vector with filenames (i.e. fnams) with the basename (i.e file name without the directory) of the data files.
## then get rid of the ".csv" part
names(fnams) <- fnams %>% 
	basename %>% # basename removes all of the path up to and including the last path separator
	gsub(x = ., pattern = "\\.csv", replacement = "") ## remove .csv part of the file name

## Step 3. Read in every file, using the relative path
## keep the filename in a nice column called "dataset".
chlamy_innoc_feb19 <- fnams %>% 
	map_df(read_csv, .id = "dataset") ## use the map function to read in the csvs listed in fnams
## .id argument allows us to create a variable giving the name of the dataset

## Step 4. Separate the dataset column into three columns, each with a separate bit of info
##(i.e. the sample name, the replicate, the temperature, which corresponds to the temperature, the replicate and the date)
## first create a new column with a datasetID, so we retain this info after we split into columns
chlamy_innoc_feb19$datasetID <- chlamy_innoc_feb19$dataset

chlamy_innoc_feb19_sep <- chlamy_innoc_feb19 %>% 
	separate(dataset, c("name", "replicate", "temperature"))

## Step 7. write the data to a new csv (this data file now contains ALL of the individual flow cam csv's and the associated temperatures)
write_csv(chlamy_innoc_feb19_sep, "/Users/Joey/Documents/chlamy-ktemp/k-temp/data/chlamy_innoc_feb19.csv")


