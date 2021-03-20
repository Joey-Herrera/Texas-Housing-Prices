### Load and clean IPUMS data
# Upload libraries
library(reshape2)
library(tidyverse)
library(ipumsr)
library(foreign)
library(dplyr)
library(tidyr)

#Load IPUMS data
#Set working directory to the "Data" directory in the "Texas-Metro-Housing-Prices" repository
IPUMS_ddi <- read_ipums_ddi("usa_00012ddi.xml")
IPUMS_data <- read_ipums_micro(IPUMS_ddi, verbose = FALSE)

# Clean IPUMS data
#Filter the data fpr texas observations
IPUMS_Texas = IPUMS_data %>%
  filter(STATEICP == 49)
#Filter for Travis county only
IPUMS_Travis = IPUMS_Texas %>%
  filter(COUNTYICP == 4530)

#Take out all NA observations 9999999
IPUMS_Texas2  = IPUMS_Texas %>%
  filter(INCTOT != 9999999)

IPUMS_Texas2 = IPUMS_Texas2 %>%
  filter(FTOTINC != 9999999)

#Filter out unnecessary variables
#IPUMS_Texas2 = IPUMS_Texas2 %>%
  #select(- RACED - PERWT - PERNUM - GQ - STRATA - MET2013 - METAREAD - METAREA -CLUSTER - HHWT - SERIAL - CBSERIAL - SAMPLE)

###Select the metropolitan areas that match up with ACS data and Zillow data
#region_list <- c(4, 32, 64, 84, 124, 126, 192, 
 #                  231, 292, 336, 381, 408, 442, 460, 488,
  #               504, 580, 720, 724, 764, 836, 864, 875,
   #              880, 908)

region_list_2013_standards <- c(11110, 12420, 13140, 
                                15180, 18580, 17780,
                                19100, 21140, 26420,
                                28660, 29700, 31180,
                                32580, 33260, 36220,
                                41660, 41700, 46340,
                                47380, 48660)

IPUMS_Texas_filtered_2013 = IPUMS_Texas2 %>%
  filter(MET2013 %in% region_list_2013_standards) %>%
  group_by(YEAR, MET2013) %>%
  summarize(prop_hisp = (sum(HISPAN == 1)/n()),
            prop_male = (sum(SEX==1)/n()),
            mean_age = mean(AGE))





