#Merge data frames

###For Zillow data
#Add year variable to Zillow dataframe to eventually join with ACS data
Zillow_long <- mutate(Zillow_long, YEAR = format(as.Date(Zillow_long$date, format="%Y-%m-%d"),"%Y"))

# Condense data frame by RegionName and year to lower the overall number of observations
region_list_zillow <- c("Amarillo, TX", "Austin, TX", 
                        "Beaumont, TX", "Brownsville, TX",
                        "Corpus Christi", "College Station",
                        "Dallas-Fort Worth, TX", "El Paso, TX",
                        "Houston, TX", "Laredo, TX",
                        "Lubbock, TX", 
                        "Midland, TX", "Odessa, TX", "San Angelo, TX",
                        "San Antonio, TX",
                        "Tyler, TX", "Waco, TX", "Wichita Falls, TX")

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

IPUMS_Texas_filtered_2013 = IPUMS_Texas_filtered_2013 %>%
  mutate(RegionName = factor(MET2013, levels = as.character(c("12420", "13140", "15180", "17780", "18580", "19100",
                                                              "26420", "29700", "31180", "33260", "36220", "41660",
                                                              "41700", "46340", "47380", "48660")) ,
                             labels = c("Amarillo, TX", "Austin, TX", "Beaumont, TX","Brownsville, TX", "Dallas-Fort Worth, TX",
                                        "El Paso, TX", "Houston, TX", "Laredo, TX", "Lubbock, TX", "Midland, TX", "Odessa, TX", "San Angelo, TX",
                                        "San Antonio, TX", "Tyler, TX", "	Waco, TX", "	Wichita Falls, TX")))

###Code for joining the ACS data and the Zillow data into a single dataset using year as the common variable
TX_housing <- merge(IPUMS_Texas_filtered_2013 , Zillow_long_condensed, by= c("YEAR", "RegionName"))
