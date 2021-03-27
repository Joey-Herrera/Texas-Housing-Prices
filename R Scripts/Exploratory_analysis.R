#Exploratory data analysis

#Look at average age over time by each region in Texas
ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=mean_age)) +
 scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average age") +
  ggtitle("Average Age in TX Metro Regions Over Time")

#Plot proportion male over time by each region in Texas
ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=prop_male)) +
  scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Proportion of men in sample") +
  ggtitle("Proportion of Men in TX Metro Regions Over Time")

#Plot average housing price over time 
ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=avg_price)) +
  scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average housing Price") +
  ggtitle("Average Housing Prices in TX Metro Regions Over Time")

#Focus on the change in housing prices in Austin by year
#Filter housing to only include average housing cost in housing
TX_housing_Austin = TX_housing %>%
  filter(RegionName == "Austin, TX")

ggplot(data = TX_housing_Austin) +
  geom_col(aes(x=YEAR, y=avg_price, fill = YEAR)) +
  scale_x_continuous(breaks = seq(2006, 2019, 2)) +
  scale_y_continuous(breaks = seq(0, 350000, 25000), labels = scales::comma) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average housing Price") +
  ggtitle("Average Housing Prices in Austin Over Time") +
  theme_linedraw()


