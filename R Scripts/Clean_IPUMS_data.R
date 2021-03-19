# Upload libraries
library(reshape2)
library(tidyverse)
library(ipumsr)
library(foreign)
library(dplyr)
library(tidyr)

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
IPUMS_Texas2 = IPUMS_Texas2 %>%
  select(- HISPAND - RACED - PERWT - PERNUM - GQ - STRATA - MET2013 - METAREAD - METAREA -CLUSTER - HHWT - SERIAL - CBSERIAL - SAMPLE)



