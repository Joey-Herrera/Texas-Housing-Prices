### Load and clean IPUMS data
#Upload libraries
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

#Clean IPUMS data
#Filter the data fpr texas observations
#IPUMS_Texas = IPUMS_data %>%
#  filter(STATEICP == 49)
#Filter for Travis county only
#IPUMS_Travis = IPUMS_Texas %>%
#  filter(COUNTYICP == 4530)
region_list_2013_standards <- c("11020", "11260", "12420","12020", "12100", "12220", "12620", "12980", "13460",
                                "13780", "14010", "15380", "16620", "16820", "17020", "17140", "17300",
                                "17460", "17820", "18140", "19300", "19340", "19460", "19500", "19660",
                                "20100", "20500","20700", "20940", "21500", "21660", "22180", "22380", "22420",
                                '23460', "24140", "24300", "24340", "24660", "25260", "25420", "25540",
                                "26140", '27060', "27100", "27140", "27180", "27620", "28020", "28700",
                                "29460", "29620", "30340", "31460", "31700", "33260", "33660", "34060",
                                "35840", "36140", "38340", "39140", "39300", "39460",
                                "39540", "40420", "41140", "42540", "44180", "46220", "47220", "48700",
                                "48900", "49180", "49420")

IPUMS_trimmed = IPUMS_data %>%
  filter(MET2013 %in% region_list_2013_standards)

#Take out all NA observations 9999999
IPUMS_trimmed  = IPUMS_trimmed %>%
  filter(INCTOT != 9999999)

IPUMS_trimmed = IPUMS_trimmed %>%
  filter(FTOTINC != 9999999)

#Filter out unnecessary variables
#IPUMS_Texas2 = IPUMS_Texas2 %>%
#select(- RACED - PERWT - PERNUM - GQ - STRATA - MET2013 - METAREAD - METAREA -CLUSTER - HHWT - SERIAL - CBSERIAL - SAMPLE)

###Select the metropolitan areas that match up with ACS data and Zillow data
#region_list <- c(4, 32, 64, 84, 124, 126, 192, 
#                  231, 292, 336, 381, 408, 442, 460, 488,
#               504, 580, 720, 724, 764, 836, 864, 875,
#              880, 908)



IPUMS_trimmed = IPUMS_trimmed %>%
  filter(MET2013 %in% region_list_2013_standards) %>%
  group_by(YEAR, MET2013) %>%
  summarize(prop_hisp = (sum(HISPAN == 1)/n()),
            prop_male = (sum(SEX==1)/n()),
            mean_age = mean(AGE),
            mean_educ = mean(EDUC),
            mean_INCTOT = mean(INCTOT),
            mean_FTOTINC = mean(FTOTINC))
# A vector with 1004 observations and 8 variablesis returned

##############################################################################
####Read in Zillow data and clean it
# Load libraries
library(lubridate)
library(dplyr)

#Read Zillow data in
Zillow_metro = read_csv('/Users/josephherrera/Documents/Github/Texas-Metro-Housing-Prices/Data/Metro_zhvi.csv')

#Zillow data filter for Texas
#Zillow_metro_Texas = Zillow_metro %>%
  #filter(StateName == "TX")

#####Reformat the data frame
#Keep data starting with 2005 and beyond

Zillow_minus_1996 <-dplyr::select(Zillow_metro, -c('1996-01-31', '1996-02-29', '1996-03-31', '1996-04-30',
                                                         '1996-05-31', '1996-06-30', '1996-07-31', '1996-08-31',
                                                         '1996-09-30', '1996-10-31', '1996-11-30', '1996-12-31'))


Zillow_minus_1996_1997 <- dplyr::select(Zillow_minus_1996, -c('1997-01-31', '1997-02-28', '1997-03-31', '1997-04-30',
                                                              '1997-05-31', '1997-06-30', '1997-07-31', '1997-08-31',
                                                              '1997-09-30', '1997-10-31', '1997-11-30', '1997-12-31'))

Zillow_minus_1996_1998 <- dplyr::select(Zillow_minus_1996_1997, -c('1998-01-31', '1998-02-28', '1998-03-31', '1998-04-30',
                                                                   '1998-05-31', '1998-06-30', '1998-07-31', '1998-08-31',
                                                                   '1998-09-30', '1998-10-31', '1998-11-30', '1998-12-31'))

Zillow_minus_1996_1999 <- dplyr::select(Zillow_minus_1996_1998, -c('1999-01-31', '1999-02-28', '1999-03-31', '1999-04-30',
                                                                   '1999-05-31', '1999-06-30', '1999-07-31', '1999-08-31',
                                                                   '1999-09-30', '1999-10-31', '1999-11-30', '1999-12-31'))

Zillow_minus_1996_2000 <- dplyr::select(Zillow_minus_1996_1999, -c('2000-01-31', '2000-02-29', '2000-03-31', '2000-04-30',
                                                                   '2000-05-31', '2000-06-30', '2000-07-31', '2000-08-31',
                                                                   '2000-09-30', '2000-10-31', '2000-11-30', '2000-12-31'))

Zillow_minus_1996_2001 <- dplyr::select(Zillow_minus_1996_2000, -c('2001-01-31', '2001-02-28', '2001-03-31', '2001-04-30',
                                                                   '2001-05-31', '2001-06-30', '2001-07-31', '2001-08-31',
                                                                   '2001-09-30', '2001-10-31', '2001-11-30', '2001-12-31'))

Zillow_minus_1996_2002 <- dplyr::select(Zillow_minus_1996_2001, -c('2002-01-31', '2002-02-28', '2002-03-31', '2002-04-30',
                                                                   '2002-05-31', '2002-06-30', '2002-07-31', '2002-08-31',
                                                                   '2002-09-30', '2002-10-31', '2002-11-30', '2002-12-31'))

Zillow_minus_1996_2003 <- dplyr::select(Zillow_minus_1996_2002, -c('2003-01-31', '2003-02-28', '2003-03-31', '2003-04-30',
                                                                   '2003-05-31', '2003-06-30', '2003-07-31', '2003-08-31',
                                                                   '2003-09-30', '2003-10-31', '2003-11-30', '2003-12-31'))

Zillow_minus_1996_2004 <- dplyr::select(Zillow_minus_1996_2003, -c('2004-01-31', '2004-02-29', '2004-03-31', '2004-04-30',
                                                                   '2004-05-31', '2004-06-30', '2004-07-31', '2004-08-31',
                                                                   '2004-09-30', '2004-10-31', '2004-11-30', '2004-12-31'))

Zillow_minus_1996_2005 <- dplyr::select(Zillow_minus_1996_2004, -c('2005-01-31', '2005-02-28', '2005-03-31', '2005-04-30',
                                                                   '2005-05-31', '2005-06-30', '2005-07-31', '2005-08-31',
                                                                   '2005-09-30', '2005-10-31', '2005-11-30', '2005-12-31'))

### Reformat the data frame from a wide to a long format
# argument 1: new key factor column
# argument 2: new value column
# argument 3: columns used to fill both new columns
Zillow_long <- gather(Zillow_minus_1996_2005, date, price, '2006-01-31':'2021-01-31')
Zillow_long

### Add year variable to dataframe to eventually join with ACS data
Zillow_long <- mutate(Zillow_long, YEAR = format(as.Date(Zillow_long$date, format="%Y-%m-%d"),"%Y"))

# Take out observations in 2020 and 2021
#extra_years = c( '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013',
# '2014', '2015', '2016', '2017', '2018', '2019')


### Code for joining the ACS data and the Zillow data into a single dataset using year as the common variable

#culturalDistrict_regions = c("Abeline, TX", "Amarillo, TX", "Austin, TX", "Beaumont, TX", "Dallas-Fort Worth, TX", "El Paso, TX", "Houston, TX",
 #                            +                              "Huntsville, TX", "Longview, TX", "Lubbock, TX", "McAllen, TX", "San Angelo, TX", "San Antonio, TX",
  #                           +                              "Sherman, TX", "Texarkana, TX", "Waco, TX")

#Zillow_long = Zillow_long %>%
#  +   mutate(culturalDistrict = ifelse(RegionName %in% culturalDistrict_regions, 1, 0))

#View(Zillow_long)

####################################################################################
#Merge data frames

###For Zillow data
#Add year variable to Zillow dataframe to eventually join with ACS data
Zillow_long <- mutate(Zillow_long, YEAR = format(as.Date(Zillow_long$date, format="%Y-%m-%d"),"%Y"))

# Condense data frame by RegionName and year to lower the overall number of observations
region_list_zillow <- c("Altoona, PA", " Anchorage, AK" , "Austin, TX", "Athens-Clarke County, GA", "Atlantic City-Hammonton, NJ",
                        "Auburn-Opelika, AL", "Bangor, ME", "Battle Creek, MI", "Bend-Redmond, OR", "Binghamton, NY", "Bloomington, IL",
                        "Buffalo-Cheektowaga-Niagara Falls, NY", "Charleston, WV", "Charlottesville, VA", "Chico, CA", "Cincinnati, OH-KY-IN",
                        "Clarksville, TN-KY", "Cleveland-Elyria, OH", "Colorado Springs, CO", "Columbus, OH", " Daphne-Fairhope-Foley, AL", " Davenport-Moline-Rock Island, IA-IL",
                        " Decatur, AL", " Decatur, IL", "Deltona-Daytona Beach-Ormond Beach, FL", "  Dover, DE", " Durham-Chapel Hill, NC", 
                        "East Stroudsburg, PA", "El Centro, CA", "Erie, PA", "Eugene, OR", " Fayetteville, NC", " Flagstaff, AZ", "Flint, MI",
                        "Gadsden, AL", "Goldsboro, NC", "Grand Junction, CO", "Grand Rapids-Wyoming, MI", "Greensboro-High Point, NC", " Hanford-Corcoran, CA",
                        " Harrisburg-Carlisle, PA", "Hartford-West Hartford-East Hartford, CT", "Homosassa Springs, FL", "Ithaca, NY", " Jackson, MI",
                        "Jackson, MS", "Jackson, TN", "Jefferson City, MO", "Kalamazoo-Portage, MI", "Kingsport-Bristol-Bristol, TN-VA", "  Lakeland-Winter Haven, FL",
                        "Lansing-East Lansing, MI", "Lewiston-Auburn, ME", "Madera, CA", "Manchester-Nashua, NH", " Midland, TX", " Mobile, AL",
                        "  Morgantown, WV", " North Port-Sarasota-Bradenton, FL", " Ocean City, NJ", "Pittsfield, MA", "Prescott, AZ", " Providence-Warwick, RI-MA",
                        " Punta Gorda, FL", "Racine, WI", "Rocky Mount, NC", "St. Joseph, MO-KS", "Scranton--Wilkes-Barre--Hazleton, PA","Springfield, MO", "Tuscaloosa, AL",
                        "Vineland-Bridgeton, NJ", "Williamsport, PA", "Wilmington, NC", "Winston-Salem, NC", "Yakima, WA")

Zillow_long_condensed = Zillow_long %>%
  filter(RegionName %in% region_list_zillow) %>%
  group_by(RegionName, YEAR) %>%
  summarise( avg_price = mean(price))

#Take out observations in 2020 and 2021
extra_years = c( '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013',
                 '2014', '2015', '2016', '2017', '2018', '2019')

Zillow_long_condensed = Zillow_long_condensed %>%
  filter(YEAR %in% extra_years)

###For IPUMS data
#Create the RegionName variable in IPUMS data

IPUMS_trimmed = IPUMS_trimmed %>%
  mutate(RegionName = factor(MET2013, levels = as.character(c("11020", "11260", "12420","12020", "12100", "12220", "12620", "12980", "13460",
                                                              "13780", "14010", "15380", "16620", "16820", "17020", "17140", "17300",
                                                              "17460", "17820", "18140", "19300", "19340", "19460", "19500", "19660",
                                                              "20100", "20500","20700", "20940", "21500", "21660", "22180", "22380", "22420",
                                                              '23460', "24140", "24300", "24340", "24660", "25260", "25420", "25540",
                                                              "26140", '27060', "27100", "27140", "27180", "27620", "28020", "28700",
                                                              "29460", "29620", "30340", "31460", "31700", "33260", "33660", "34060",
                                                             "35840", "36140", "38340", "39140", "39300", "39460",
                                                              "39540", "40420", "41140", "42540", "44180", "46220", "47220", "48700",
                                                              "48900", "49180", "49420")) ,
                             labels = c("Altoona, PA", " Anchorage, AK" , "Austin, TX", "Athens-Clarke County, GA", "Atlantic City-Hammonton, NJ",
                                        "Auburn-Opelika, AL", "Bangor, ME", "Battle Creek, MI", "Bend-Redmond, OR", "Binghamton, NY", "Bloomington, IL",
                                        "Buffalo-Cheektowaga-Niagara Falls, NY", "Charleston, WV", "Charlottesville, VA", "Chico, CA", "Cincinnati, OH-KY-IN",
                                        "Clarksville, TN-KY", "Cleveland-Elyria, OH", "Colorado Springs, CO", "Columbus, OH", " Daphne-Fairhope-Foley, AL", " Davenport-Moline-Rock Island, IA-IL",
                                        " Decatur, AL", " Decatur, IL", "Deltona-Daytona Beach-Ormond Beach, FL", "  Dover, DE", " Durham-Chapel Hill, NC", 
                                        "East Stroudsburg, PA", "El Centro, CA", "Erie, PA", "Eugene, OR", " Fayetteville, NC", " Flagstaff, AZ", "Flint, MI",
                                        "Gadsden, AL", "Goldsboro, NC", "Grand Junction, CO", "Grand Rapids-Wyoming, MI", "Greensboro-High Point, NC", " Hanford-Corcoran, CA",
                                        " Harrisburg-Carlisle, PA", "Hartford-West Hartford-East Hartford, CT", "Homosassa Springs, FL", "Ithaca, NY", " Jackson, MI",
                                        "Jackson, MS", "Jackson, TN", "Jefferson City, MO", "Kalamazoo-Portage, MI", "Kingsport-Bristol-Bristol, TN-VA", "  Lakeland-Winter Haven, FL",
                                        "Lansing-East Lansing, MI", "Lewiston-Auburn, ME", "Madera, CA", "Manchester-Nashua, NH", " Midland, TX", " Mobile, AL",
                                        "  Morgantown, WV", " North Port-Sarasota-Bradenton, FL", " Ocean City, NJ", "Pittsfield, MA", "Prescott, AZ", " Providence-Warwick, RI-MA",
                                        " Punta Gorda, FL", "Racine, WI", "Rocky Mount, NC", "St. Joseph, MO-KS", "Scranton--Wilkes-Barre--Hazleton, PA","Springfield, MO", "Tuscaloosa, AL",
                                        "Vineland-Bridgeton, NJ", "Williamsport, PA", "Wilmington, NC", "Winston-Salem, NC", "Yakima, WA")))

###Code for joining the ACS data and the Zillow data into a single dataset using year as the common variable
CSRD_housing <- merge(IPUMS_trimmed , Zillow_long_condensed, by= c("YEAR", "RegionName"))

### Take out metropolitan areas with years between 2008 and 2018 as NA
 #levels = data.frame(levels)
#labels = data.frame(labels)

########################################################################################
library(tidyverse)
library(haven)
library(Synth)
library(devtools)
if(!require(SCtools)) devtools::install_github("bcastanho/SCtools")
library(SCtools)




# Turn the RegionName variable into a factor variable
CSRD_synth = CSRD_housing %>%
  mutate(RegionName = as.character(RegionName))

CSRD_synth = as.data.frame(CSRD_synth)

#Metropolitana areas that survived the merging
region_list = c("Austin, TX","Altoona, PA" , "Bangor, ME", "Battle Creek, MI", "Binghamton, NY", "Bloomington, IL",  "Charlottesville, VA", "Chico, CA", 
                           "Colorado Springs, CO", "Columbus, OH", 
                          "East Stroudsburg, PA", "El Centro, CA", "Erie, PA", "Eugene, OR", "Flint, MI",
                          "Gadsden, AL", "Goldsboro, NC", "Grand Junction, CO",
                          "Jackson, MS", "Jackson, TN", 
                          "Madera, CA", 
                          "Pittsfield, MA", "Prescott, AZ", 
                           "Racine, WI", "Rocky Mount, NC","Springfield, MO", 
                          "Williamsport, PA", "Wilmington, NC", "Winston-Salem, NC", "Yakima, WA",
                          "Charleston, WV" , "Homosassa Springs, FL", "Ithaca, NY", "Jefferson City, MO", "Tuscaloosa, AL")

CSRD_synth2 = CSRD_synth %>%
  filter(RegionName %in% region_list)

# Balance the panel with years and average housing prices
region_filter <- c("Altoona, PA",
"Battle Creek, MI",
"Charleston, WV",
"Charlottesville, VA",
"Flint, MI",
"Homosassa Springs, FL",
"Ithaca, NY",
"Jefferson City, MO",	
"Tuscaloosa, AL")

CSRD_synth2 = CSRD_synth2 %>%
  filter(!RegionName %in% region_filter)

# Take out 06, 07, 19
nope_year = c("2006", "2007", "2019")

CSRD_synth2 = CSRD_synth2 %>%
  filter(!YEAR %in% nope_year)
#Synthetic control
dataprep_out <- dataprep(
  foo = CSRD_synth2,
  predictors = c( "mean_age", "mean_educ", "prop_hisp", "prop_male", "avg_price", "mean_INCTOT", "mean_FTOTINC"),
  predictors.op = "mean",
  time.predictors.prior = 2008:2013,
  #special.predictors = list(
   # list("avg_price", c(2010:2012), "mean"),
    #list("mean_INCTOT", c(2008:2012), "mean"),
    #list("mean_FTOTINC", c(2008:2012), "mean")),
  #list("black", 1990:1992, "mean"),
  #list("perc1519", 1990, "mean")),
  dependent = "avg_price",
  unit.variable = "MET2013",
  unit.names.variable = "RegionName",
  time.variable = "YEAR",
  treatment.identifier = 12420, # Austin MET2013 code
  controls.identifier = c( 12620, 
                           13780, 14010,  17020, 17820, 18140, 20700, 20940, 21500, 21660, 
                           23460, 24140, 24300,  27140, 27180, 31460, 38340, 39140,
                           39540,  44180, 
                           49180, 49420),
  time.optimize.ssr = 2008:2013,
  time.plot = 2008:2018
)


controls.identifier = c( 12620, 
13780, 14010,  17020, 17820, 18140, 20700, 20940, 21500, 21660, 
23460, 24140, 24300,  27140, 27180, 31460, 38340, 39140,
39540,  42540,  48700,
48900, 49180, 49420)


# Had to take out San Angelo to balance the number of observations in the pre and post treatment groups (41660)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out, Ylab = c("Average Housing Price"), Main = c("Average Housing Price Over Time")) 



synth.tables <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res = synth_out)
print(synth.tables)

# plot stuff
gaps.plot(synth_out, dataprep_out, Ylab = c("Treated Versus Synthetic Average Housing Price"))


# placebos
placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)



