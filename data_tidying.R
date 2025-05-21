# packages ----

library(tidyverse)
library(stringi)
library(lubridate)
library(readxl)

# read in data ----

rm(list=ls()) # clear environment

# dataframe containing all RT files compiled
master_df <- read_csv("C:/Users/johan/Documents/BIO-6019Y local folder/master_df_2.csv") # read in data

head(master_df) # check data read in
glimpse(master_df) # check data format


# dataframe containing information about each tagged bird
birds <- read_csv("C:/Users/johan/Documents/BIO-6019Y local folder/individualPITListUEA_Winter_ListA_2023-24.csv")

head(birds) # check data read in
glimpse(birds) # check data format
birds <- birds %>%
  filter(!is.na(TagID_hex)) #filters out one observation where TagID_hex is NA as is redundant and causes an issue when filtering out unidentified birds later

# tidy master dataframe ----

#filter out redundant files
tidy_master_df <- master_df %>%
  filter(!grepl("^learning set", FileID, ignore.case = TRUE)) %>% # filter out learning set 3 and 4 as not needed for my hypotheses and will make wrangling more manageable
  filter(FileID !="190124_GH2_recovery not sorted") %>% # remove file containing 1 unrecovered data
  mutate(FileID = case_when(
    FileID == "190124_GH2_recovery sorted" ~ "AL 160124 - 260124_190124_GH2", # rename recovered file from same day
    TRUE ~ FileID)) # ensures other files aren't affected by above line

print(unique(tidy_master_df$FileID)) # check files were removed/renamed - this is important for next part to work


# use FileID to create columns seperating by feeder, set and phase
# shared folder readme has since been updated to explain that referencer tags were used to signal start and end of phases
  # can update to use this instead of manually input timeframes if need be, but this is working for me currently
tidy_master_df <- tidy_master_df %>%
  mutate(feeder = stri_sub(FileID, -3, -1)) %>% # extracts only last 3 digits from FileID to give feeder column
  mutate(phase = stri_sub(FileID, 1, 3)) %>% # extracts only first 3 digits to give phase column
  mutate(phase = str_trim(phase)) # removes any blank spaces from phase e.g. "AL "
  
head(tidy_master_df)
print(unique(tidy_master_df$feeder)) # checks feeder column obs
print(unique(tidy_master_df$phase)) # checks phases

tidy_master_df <- tidy_master_df %>%
  mutate(Date = ymd_hms(Date, quiet = TRUE))  # ensure date is read in datetime format
  
# ensure each phase is assigned to the correct set
tidy_master_df <- tidy_master_df %>%
  mutate(set = case_when(
    phase %in% c("AL", "RL1") & !is.na(Date) & Date < ymd_hms("2024-02-04 00:00:00") ~ "1", # assigns AL and RL1 to right learning sets as these are indistinguishable
    phase %in% c("AL", "RL1") & !is.na(Date) & Date >= ymd_hms("2024-02-04 00:00:00") ~ "2",
    phase == "RL2" ~ "1",  # assign RL2 to set 1 as is unrepeated
    TRUE ~ NA_character_ )) # keeps other values as NA

head(tidy_master_df)
print(unique(tidy_master_df$set))
sum(is.na(tidy_master_df$set)) # there are NAs in date times which has caused NAs in sets - will need filtering at some stage

# add species ----
tidy_master_df <- tidy_master_df %>%
  left_join(birds %>% select(TagID_hex, species), by = "TagID_hex") # adds species to corresponding TagID_hex from birds data
head(tidy_master_df)
print(unique(tidy_master_df$species)) 
table(tidy_master_df$species, useNA = "ifany") # checks frequency of each species + NAs

tidy_master_df <- tidy_master_df %>%
  filter(TagID_hex %in% birds$TagID_hex)  # filters only rows where TagID_hex matches those in the birds dataframe into new df (NAs are removed)
print(unique(tidy_master_df$species)) # check no remaining NAs in species

# quantify individual visits ----

# create date column
ind_visits <- tidy_master_df %>%
  rename(date_time = Date) %>% # rename Date column to reflect it shows date and time
  mutate(date_only = as.Date(date_time)) # extract date from date_time and create new column "date_only"
head(ind_visits)

# create time since previous visit column
ind_visits <- ind_visits %>%
  arrange(date_time) %>% # arranges data set in chronological order
  group_by(date_only, feeder, TagID_hex) %>% # groups the data into TagID_hex for each feeder for each date
  mutate(t_since_prev_visit = date_time - lag(date_time)) %>% # creates column with time since previous visit
  ungroup() # removes the grouping
head(ind_visits)
  
ind_visits$t_since_prev_visit <- gsub(' secs', '', ind_visits$t_since_prev_visit) %>% # removes "secs" from values in t_since_prev_visit
  as.numeric(as.character(ind_visits$t_since_prev_visit)) # and converts to numeric format

# plot successive visits
successive_visits <- ind_visits%>% # create new data frame with successive visits less than 20 secs apart to graph how frequently birds are read at the feeder
  filter(t_since_prev_visit <=20)%>%
  select(t_since_prev_visit)

ggplot(successive_visits, aes(t_since_prev_visit, y= 1-..y..))+ # cumulative frequency graph to visualise how frequently birds are read at the feeder
  stat_ecdf(geom = "step", color="purple")

ggplot(successive_visits, aes(x=t_since_prev_visit)) +geom_histogram(binwidth = 1) # histogram of the same

# drop off appears to be between 2 and 3 seconds after - will filter from 3 seconds after
# remove observations which occurred less than 3 seconds apart as these are likely from birds perching and setting off the RFID again
ind_visits <- ind_visits %>%
  filter(t_since_prev_visit>3)

head(ind_visits)

# define visit type ----
# are the birds visiting their assigned (rewarding) feeder?

# read in compiled list of feeder assignments
assigned_feeders <- read_excel("C:/Users/johan/Documents/BIO-6019Y local folder/assigned feeders spreadsheet.xlsx")
head(assigned_feeders)

# ensure compatible formatting
assigned_feeders <- assigned_feeders %>%
  mutate(set = as.character(set)) %>% # change format to character string so compatible with other df
  mutate(TagID_hex = str_remove(TagID_hex, "^\\d+=")) #remove "number=" at start of each TagID_hex so compatible

# add column quantifying visit type
ind_visits <- ind_visits %>%
  left_join(assigned_feeders, by = c("TagID_hex", "set", "phase")) %>%  # add assigned feeder column
  mutate(feeder_correct = as.integer(feeder == assigned_feeder)) %>%  # add a column which indicates if "assigned_feeder" matches "feeder" in binary i.e. 1 = correct, 0 = incorrect
  filter(!is.na(feeder_correct))  # remove birds with no assigned feeder
head(ind_visits)

# quantify learning ----

# function to quantify learning by Vildan Acar

success_check <- function(correct) {
  n <- length(correct)
  if (n < 20) {
    return(NA)
  }
  for (i in 1:(n-19)) {
    window <- correct[i: (i+19)]
    if(window[1] == 1 && sum(window)/ 20 >= 0.8){
      return(i+19)
    }
  }
  return(NA)
}

# apply function to data
learning <- ind_visits %>%
  group_by(TagID_hex, phase, set) %>% # group by bird, set and phase
  summarise(criterion = success_check(feeder_correct), # vildan's function
            .groups = "drop") # ungroup data so it remains easy to use

head(learning)
