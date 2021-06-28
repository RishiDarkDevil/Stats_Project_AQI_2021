library(tidyverse)
library(lubridate)
library(modelr)
library(psych)
library(car)
library(GGally)
library(ggpubr)
library(ggrepel)
library(purrr)
library(stringr)

# Do all the Stations report same number of observations ?
print(air_data_india %>%
        group_by(Year, City) %>%
        count(), n = 150)
# The number of observation at each City Station is more or less same except some Stations with higher number of observations
obsv_per_station <- air_data_india %>%
  group_by(Year, City) %>%
  count() %>%
  arrange(Year, desc(n)) %>%
  spread(key = Year, value = n)

print(obsv_per_station %>%
  arrange(desc(`2019`,`2020`,`2021`)), n = 30)

# The number of Pollutants measured by each Station
air_data_india_pollutants %>%
  group_by(City) %>%
  summarise(Types = length(unique(Specie))) %>%
  arrange(Types)

# The Station-wise Median values for Pollutants each year
avg_median_per_year_per_station <- air_data_india_pollutants %>%
  group_by(Year, City) %>%
  summarise(Median = mean(median)) %>%
  arrange(desc(Year, Median)) %>%
  spread(key = Year, value = Median) %>%
  arrange(desc(`2021`, `2019`, `2020`, `2018`))

print(avg_median_per_year_per_station, n = 50)

air_data_india_pollutants %>%
  group_by(Year, City) %>%
  summarise(Median = mean(median)) %>%
  arrange(desc(Year, Median)) %>%
  filter(Year != 2014) %>%
  ggplot(aes(Year, Median)) +
  geom_bar(aes(fill = City), position = "dodge", stat = "identity") +
  labs(
    y = "Average Median AQI Levels"
  ) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 2,
      override.aes = list(size = 3)
    )
  )

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017, 2018)), City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, median, fill =cut_width(Year, 1))) +
  geom_boxplot(outlier.shape = NA) + 
  coord_cartesian(ylim = c(0, 350)) +
  labs(y = "Average Median AQI Levels", fill = "Year")

air_data_india_pollutants %>%
  filter(Year %in% c(2014, 2015, 2016, 2017)) %>%
  ggplot(aes(City, median, fill =cut_width(Year, 1))) +
  geom_boxplot(outlier.shape = NA)
