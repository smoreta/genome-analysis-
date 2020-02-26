
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")
library(tidyverse)
surveys <- read.csv("data/portal_data_joined.csv")
SNPs <- read_tsv("data/23andMe_complete.txt", skip = 14, col_types = 
cols(chromosome = col_factor()))

#challenge 1
#Using pipes, subset the surveys data to include animals collected before 
#1995 and retain only the columns year, sex, and weight.
surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

#challenge 2
#Create a new data frame from the surveys data that meets the following criteria: 
#contains only the species_id column and a new column called hindfoot_half containing 
#values that are half the hindfoot_length values. In this hindfoot_half column, there 
#are no NAs and all values are less than 30.
surveys_hindfoot_half <- surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  mutate(hindfoot_half = hindfoot_length / 2) %>%
  filter(hindfoot_half < 30) %>%
  select(species_id, hindfoot_half)

#challenge 3 



