# Title: Fix the Errors (Question 1) #####
# Author: Your Name 
# Author's Email: Your Email 
# Date Created: 2021-02-20

# Purpose:
# There are 10 errors in the below code. Find the errors and fix them. You earn 2.5 points per error that you fix. 

#NOTE: You will need to load the fars_accident_2014_2019.Rdata dataset to complete this question. 

# Data
#load('midterm/fars_accident_2014_2019.Rdata')

# Code to Review for Errors V ####

# Set Up####
# Libraries (1 error)
#install.packages('tidyverse' )

# Check the Structure of the Data (1 error) ####
str("fars_accident_2014_2019")

# Check the structure of `DAY_WEEK` in `fars_accident_2014_2019` (1 error) ####
str(DAY_WEEK)

# What years are represented in the data set? (1 error) ####
I don't know'

# Count How Many Accidents per State, arrange in descending order, keep only top 10 (2 errors) ####
fars_accident_2014_2019 %>% 
  group_by(CITY) %>% 
  count() %>% 
  arrange() %>% 
  rename(accidents = n) %>% 
  head(n=10)

# Count How Many Fatal Accidents involved Pedestrians (1 error) ####
fars_accident_2014_2019 %>% 
  filter(PEDS == 0) %>% 
  nrow()

# What percentage of the total number of accidents involved pedestrians, Code as an object: `perc_accidents_pedestrians` (1 error) ####
38729 / 199065 * 100 

# Summarize How Many Fatalities there were per year (1 error)
fars_accident_2014_2019 %>% 
  group_by(YEAR) 
  mutate(Fatalities = sum(FATALS)) %>% 
  rename(Year = YEAR)

# How many fatal accidents had more than 1 fatality (1 error)
fars_accident_2014_2019 %>% 
  filter(FATALS<=1) %>% 
  nrow()

# Copyright (c) Grant Allard, 2021








# MY CORRECTIONS TO THE MIDTERM CODES


# Title: Fix the Errors (Question 1) #####
# Author: Your Name 
# Author's Email: Your Email 
# Date Created: 2021-02-20

# Purpose:
# There are 10 errors in the below code. Find the errors and fix them. You earn 2.5 points per error that you fix. 

#NOTE: You will need to load the fars_accident_2014_2019.Rdata dataset to complete this question. 

# Data
#load('midterm/fars_accident_2014_2019.Rdata')

# Code to Review for Errors V ####

# Set Up####
# Libraries (1 error)
install.packages("tidyverse" )
library(tidyverse)

# Check the Structure of the Data (1 error) ####
fars_accident_2014_2019
str(fars_accident_2014_2019)

# Check the structure of `DAY_WEEK` in `fars_accident_2014_2019` (1 error) ####
fars_df <- fars_df %>% 
select(DAY_WEEK)
str(fars_df)

# What years are represented in the data set? (1 error) ####
summary(fars_df$YEAR)
vector_numeric <- c(2014, 2015, 2016, 2017, 2018, 2019)

# Count How Many Accidents per State, arrange in descending order, keep only top 10 (2 errors) ####
fars_accident_2014_2019 %>% 
  group_by(STATE) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  rename(accidents = n) %>% 
  head(n=10)

# Count How Many Fatal Accidents involved Pedestrians (1 error) ####
fars_accident %>% 
  filter(PEDS == 0 & FATALS == 1) %>% 
  nrow()
  
# What percentage of the total number of accidents involved pedestrians, Code as an object: `perc_accidents_pedestrians` (1 error) ####
perc_accidents_pedestrians <-fars_df %>% 
  group_by(FATALS) %>% 
  summarize(prop=n() / 199065 * 100)

# Summarize How Many Fatalities there were per year (1 error)
fars_accident_2014_2019 %>% 
  group_by(YEAR, FATALS) %>% 
  summarize(fars_accident_2014_2019, fatalities=sum(FATALS)) %>% 
  view(fars_accident_2014_2019)

# How many fatal accidents had more than 1 fatality (1 error)
fars_accident_2014_2019 %>% 
filter(FATALS>1) %>% 
group_by(FATALS) %>% 
count() %>% 
arrange()

fars_accident_2014_2019 %>% 
  group_by(YEAR) %>% 
  count() %>% 
  rename(FATALS = n, Year = YEAR) %>% 
  ggplot(aes(x=Year, y= FATALS, color=Year))+
  ggtitle("FATALITIES PER YEAR")+
  geom_bar(stat= 'identity')


# Copyright (c) John Covington, 2021

