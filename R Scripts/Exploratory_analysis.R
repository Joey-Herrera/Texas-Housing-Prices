#Exploratory data analysis

ggplot(data = TX_housing) +
  geom_line(aes(x=YEAR, y=mean_age)) +
  scale_x_discrete(breaks = c(), labels=c("2006","2007","2008", "2009", "2010", "2011",
                            "2012", "2013", "2014", "2015", "2016", "2017",
                            "2018", "2019")) +
  facet_wrap(~RegionName) +
  xlab("Year (2006 to 2019)") +
  ylab("Average age")