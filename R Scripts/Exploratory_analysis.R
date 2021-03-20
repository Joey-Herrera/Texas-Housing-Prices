#Exploratory data analysis

#Look at average age over time by each region in Texas
ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=mean_age)) +
 scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average age")

#Plot proportion male over time by each region in Texas
ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=prop_male)) +
  scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Proportion of men in sample")

#Plot average housing price over time 
ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=avg_price)) +
  scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average housing Price")