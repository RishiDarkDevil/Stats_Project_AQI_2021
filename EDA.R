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
library(formattable)

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

# A Better One ------------------------------------------------------Gotta look at this
City_AQI <- air_data_india_pollutants %>%
  group_by(Year, City) %>%
  summarise(Median = mean(median, na.rm = T), Max = mean(max, na.rm = T), Min = mean(min, na.rm = T)) %>%
  gather(Median, Max, Min, key = "Measure", value = "AQI") %>%
  spread(key = Year, value = AQI) %>%
  select(-c(`2014`, `2015`, `2016`, `2017`))
print(City_AQI, n = 100)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

formattable(City_AQI, 
            `2018`= color_tile("blue", "red"),
            `2019`= color_tile(customGreen, customGreen0),
            `2020`= color_tile(customGreen, customGreen0),
            `2021` = color_bar(customRed))

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

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)), City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, median, fill =cut_width(Year, 1))) +
  geom_boxplot(outlier.shape = NA) + 
  coord_cartesian(ylim = c(0, 350)) +
  labs(y = "Average Median AQI Levels", fill = "Year") +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

# Monthly Breakdown of the AQI Levels overall and in each Station
Monthly_Pollutant <- air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month) %>%
  summarise(Avg_Min = mean(min), Avg_Median = mean(median), Avg_Max = mean(max))
Monthly_Pollutant

Monthly_Pollutant %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")

Monthly_Pollutant_1 <- Monthly_Pollutant %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")

Monthly_Pollutant_City <- air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  group_by(Year, Month, City) %>%
  summarise(Avg_Min = mean(min), Avg_Median = mean(median), Avg_Max = mean(max))
Monthly_Pollutant_City

Monthly_Pollutant_City_1 <- Monthly_Pollutant_City %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")
Monthly_Pollutant_City_1

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month) %>%
  summarise(Avg_Median = mean(median, na.rm = TRUE)) %>%
  spread(key = Year, value = Avg_Median)

air_data_india_pollutants %>% # Not at all informative really weird kinda
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  ggplot(aes(cut_width(Month,1), median)) +
  geom_boxplot(aes(fill = cut_width(Year, 1)))

Monthly_Pollutant_1 %>%
  ggplot(aes(Month, AQI)) +
  geom_point(aes(color = Year, shape = Measure), size = 6, alpha = 0.66) +
  labs(
    y = "Average Median AQI Levels"
  ) +
  scale_color_viridis(option = "H")

Monthly_Pollutant_City_1 %>%
  ggplot(aes(Month, AQI)) +
  geom_point(aes(color = City, alpha = 0.7, shape = Measure), size = 4) +
  facet_wrap(~Year) +
  scale_color_viridis(discrete = TRUE, option = "H")
  

# Looking at the Pollutant-Wise Breakdown
# Yearly
Yearly_Specie <- air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Specie) %>%
  summarise(Avg_Min = mean(min), Avg_Median = mean(median), Avg_Max = mean(max))
Yearly_Specie  

Yearly_Specie_1 <- Yearly_Specie %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI") %>%
  spread(key = Specie, value = AQI)
Yearly_Specie_1

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  gather(min, median, max, key = "Measure", value = "AQI") %>%
  ggplot(aes(Specie, AQI, fill = cut_width(Year, 1))) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 300))

# Monthly
Monthly_Specie <- air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month, Specie) %>%
  summarise(Avg_Min = mean(min), Avg_Median = mean(median), Avg_Max = mean(max))
Monthly_Specie  

Monthly_Specie_1 <- Monthly_Specie %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI") %>%
  spread(key = Specie, value = AQI)
Monthly_Specie_1

Monthly_Specie_2 <- Monthly_Specie_1 %>%
  gather(co:so2, key = "Pollutants", value = "AQI")
Monthly_Specie_2

Monthly_Specie_2 %>%
  ggplot(aes(Month, AQI)) +
  geom_point(aes(color = Pollutants, shape = Measure), alpha = 0.66, size = 6) +
  labs(
    y = "Average AQI Levels"
  ) +
  facet_wrap(~Year) +
  scale_color_viridis(discrete = TRUE, option = "B")

Monthly_Specie_2 %>%
  filter(Year %in% c(2020, 2019)) %>%
  ggplot(aes(Month, AQI)) +
  geom_point(aes(color = Year, shape = Measure), alpha = 0.66, size = 6) +
  labs(
    y = "Average AQI Levels"
  ) +
  facet_wrap(~Pollutants) +
  scale_color_viridis(option = "B")

# Station-Wise -- Not much fruitful
Yearly_Specie_City <- air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, City, Specie) %>%
  summarise(Avg_Min = mean(min), Avg_Median = mean(median), Avg_Max = mean(max))
Yearly_Specie_City 

Yearly_Specie_City_1 <- Yearly_Specie_City %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")
Yearly_Specie_City_1

air_data_india_pollutants %>%
  filter( City %in% avg_median_per_year_per_station$City[1:11] & Year == 2019) %>%
  ggplot(aes(City, median, fill = Specie)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 250))

Monthly_Specie_1 <- Monthly_Specie %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI") %>%
  spread(key = Specie, value = AQI)
Monthly_Specie_1


Monthly_Specie_2 <- Monthly_Specie_1 %>%
  gather(co:so2, key = "Pollutants", value = "AQI")
Monthly_Specie_2

# Histograms pertaining to Specie
air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017, 2018, 2018, 2020)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(median, fill = cut_width(Year,1), alpha = 0.5)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 30)) +
  facet_wrap(~Specie)
