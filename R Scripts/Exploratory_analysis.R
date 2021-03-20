#Exploratory data analysis

ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=mean_age)) +
 scale_x_continuous(breaks = seq(2006, 2019, 4)) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average age")