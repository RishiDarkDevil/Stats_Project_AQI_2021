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
library(viridis)
library(formattable)
library(skimr)
library(moderndive)
library(infer)
library(fitdistrplus)
library(pander)
library(stargazer)
library(forcats)

# Apart from Delhi are the counts of other stations same ?
print(obsv_per_station, n = 50)

obsv_per_station %>%
  gather(`2021`, `2020`, key = "Year", value = Observations) %>%
  select(City, Year, Observations) %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Observations, fill = City)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year) +
  coord_flip() +
  labs(
    x = "Number of Observations Recorded"
  ) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 1,
      override.aes = list(size = 3)
    )
  ) +
  scale_color_viridis(discrete = TRUE)

test <- obsv_per_station %>%
  ungroup() %>%
  filter(City != "Delhi" & City %in% avg_median_per_year_per_station$City[1:11]) %>% # Since it is clearly different
  select(`2020`) %>%
  chisq.test()
test
pander(test)

test <- obsv_per_station %>%
  ungroup() %>%
  filter(City != "Delhi" & City %in% avg_median_per_year_per_station$City[1:11]) %>% # Since it is clearly different
  select(`2021`) %>%
  chisq.test()
test

# Testing Equality of Means of different City Stations

Citywise_pollutant_AQI <- air_data_india_pollutants %>%
  group_by(Year, Month, City, Day) %>%
  summarise(Avg_Min = mean(min, na.rm = TRUE), Avg_Median = mean(median, na.rm = TRUE), Avg_Max = mean(max, na.rm = TRUE))
Citywise_pollutant_AQI

Citywise_pollutant_AQI_2020 <- Citywise_pollutant_AQI %>%
  filter(Year == 2020)
Citywise_pollutant_AQI_2021 <- Citywise_pollutant_AQI %>%
  filter(Year == 2021)
Citywise_pollutant_AQI_2019 <- Citywise_pollutant_AQI %>%
  filter(Year == 2019)


Citywise_pollutant_AQI_2019 %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Avg_Median)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = mean(Citywise_pollutant_AQI_2019$Avg_Median))

Citywise_pollutant_AQI_2020 %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Avg_Median)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = mean(Citywise_pollutant_AQI_2020$Avg_Median))

test <- aov(Avg_Median ~ City, filter(Citywise_pollutant_AQI_2020, City %in% avg_median_per_year_per_station$City[1:11] & City != "Delhi"))
summary(test)

Citywise_pollutant_AQI_2021 %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Avg_Median)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = mean(Citywise_pollutant_AQI_2021$Avg_Median))

air_data_all_specie_yearly_city_included <- air_data_india %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017, 2018))) %>%
  group_by(Year, City, Specie) %>%
  summarise(Avg_Min = mean(min, na.rm = TRUE), Avg_Median = mean(median, na.rm = TRUE), Avg_Max = mean(max, na.rm = TRUE)) %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")
air_data_all_specie_yearly_city_included

air_data_pollutants_yearly_city_included <- air_data_all_specie_yearly_city_included %>%
  filter(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25"))
air_data_pollutants_yearly_city_included

air_data_nonpollutants_yearly_city_included <- air_data_all_specie_yearly_city_included %>%
  filter(!(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25")))
air_data_nonpollutants_yearly_city_included

air_data_pollutants_yearly_city_included %>%
  filter(Year == 2019 & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, AQI)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(
    yintercept = mean(filter(air_data_pollutants_yearly_city_included, Year == 2019 & City %in% avg_median_per_year_per_station$City[1:11])$AQI)
  )

test <- aov(AQI ~ City, filter(air_data_pollutants_yearly_city_included, City %in% avg_median_per_year_per_station$City[1:11] & Year == 2019))
summary(test)

air_data_pollutants_yearly_city_included %>%
  filter(Year == 2020 & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, AQI)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(
    yintercept = mean(filter(air_data_pollutants_yearly_city_included, Year == 2020 & City %in% avg_median_per_year_per_station$City[1:11])$AQI)
  )

test <- aov(AQI ~ City, filter(air_data_pollutants_yearly_city_included, City %in% avg_median_per_year_per_station$City[1:11] & Year == 2020))
summary(test)

air_data_pollutants_yearly_city_included %>%
  filter(Year == 2021 & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, AQI)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(
    yintercept = mean(filter(air_data_pollutants_yearly_city_included, Year == 2021 & City %in% avg_median_per_year_per_station$City[1:11])$AQI)
  )

test <- aov(AQI ~ City, filter(air_data_pollutants_yearly_city_included, City %in% avg_median_per_year_per_station$City[1:11] & Year == 2021))
summary(test)
