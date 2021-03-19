#### Read in Zillow data and clean it
# Load libraries
library(lubridate)
library(dplyr)

#Read Zillow data in
Zillow_metro = read_csv('/Users/josephherrera/Documents/Github/Texas-Metro-Housing-Prices/Data/Metro_zhvi.csv')

#Zillow data filter for Texas
Zillow_metro_Texas = Zillow_metro %>%
  filter(StateName == "TX")

##### Reformat the data frame
#Keep data starting with 2005 and beyond

Zillow_minus_1996 <-dplyr::select(Zillow_metro_Texas, -c('1996-01-31', '1996-02-29', '1996-03-31', '1996-04-30',
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

### Condense data frame by RegionName and year to lower the overall number of observations
Zillow_long_condensed = Zillow_long %>%
  group_by(RegionName, YEAR) %>%
  summarise( avg_price = mean(price))

### Code for joining the ACS data and the Zillow data into a single dataset using year as the common variable
TX_housing <- merge(IPUMS_Travis , Zillow_long, by="YEAR")




