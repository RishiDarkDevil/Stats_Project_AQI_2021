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

# This one is Correct
air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(median, ..density.., fill = cut_width(Year,1), group = Year)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 50)) +
  facet_wrap(~Year)

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & (Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(median, ..density.., fill = cut_width(Year,1), group = Year)) +
  geom_histogram(binwidth = 5) +
  coord_cartesian(xlim = c(0, 200)) +
  facet_wrap(~Year + Specie)

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(max, ..density.., fill = cut_width(Year,1), group = Year)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 200)) +
  facet_wrap(~Year)

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(min, ..density.., fill = cut_width(Year,1), group = Year)) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(0, 10)) +
  facet_wrap(~Year)

air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(median, ..density.., fill = cut_width(Year,1))) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 30)) +
  facet_wrap(~Specie)

# This one is correct
air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(median, ..density.., fill = Specie, group = Specie)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 30)) +
  facet_wrap(~Year + Specie)


air_data_india_pollutants_2020 <- air_data_india_pollutants %>%
  filter(Year == 2020)

air_data_india_pollutants_2020 %>%
  filter(Specie == 'co') %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram()

air_data_india_pollutants_2019 <- air_data_india_pollutants %>%
  filter(Year == 2019)

air_data_india_pollutants_2019 %>%
  filter(Specie == 'co') %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram()


# Exploring the relationships between All Species with each other Monthly
air_data_india_All_Specie_Monthly <- air_data_india %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month, Specie) %>%
  summarise(Avg_Min = mean(min, na.rm = TRUE), Avg_Median = mean(median, na.rm = TRUE), Avg_Max = mean(max, na.rm = TRUE))
print(air_data_india_All_Specie_Monthly, n = 1000)

air_data_india_All_Specie_Monthly_1 <- air_data_india_All_Specie_Monthly %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")
air_data_india_All_Specie_Monthly_1

air_data_india_All_Specie_Monthly_2 <- air_data_india_All_Specie_Monthly_1 %>%
  spread(key = Specie, value = AQI)
print(air_data_india_All_Specie_Monthly_2, n = 1000)

air_data_india_All_Specie_Monthly_2[-3] %>%
  drop_na() %>%
  cor()

air_data_india_All_Specie_Monthly_2[-3] %>%
  ggpairs()

# Actually here all the parameters with all the others except Year and Month are really highly correlated. So let's take a look at them 
air_data_india_All_Specie_Monthly_2 %>%
  ggpairs(columns = c("co", "no2", "o3", "so2", "pm10", "pm25"), aes(color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  scale_color_viridis(discrete = TRUE)

air_data_india_All_Specie_Monthly_2 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  ggpairs(columns = c("dew", "humidity", "precipitation", "pressure", "temperature", "wind-gust", "wind-speed"), aes(color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  scale_color_viridis(discrete = TRUE)

air_data_india_Pollutants_nonpollutants_monthly <- air_data_india_All_Specie_Monthly_1 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  spread(key = Specie, value = AQI) %>%
  select(-precipitation) %>%
  gather(dew, humidity, pressure, temperature, `wind-gust`, `wind-speed`, key = "non_pollutants", value = Levels) %>%
  gather(co, so2, no2, o3, pm10, pm25, key = "pollutants", value = "AQI")
air_data_india_Pollutants_nonpollutants_monthly

air_data_india_Pollutants_nonpollutants_monthly %>%
  ggplot(aes(AQI, Levels, color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~pollutants + non_pollutants, scales = "free", labeller = label_wrap_gen(multi_line=FALSE))


# Exploring the relationships between All Species with each other Daily
air_data_india_All_Specie_Daily <- air_data_india %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month, Day,  Specie) %>%
  summarise(Avg_Min = mean(min, na.rm = TRUE), Avg_Median = mean(median, na.rm = TRUE), Avg_Max = mean(max, na.rm = TRUE))
print(air_data_india_All_Specie_Daily, n = 1000)

air_data_india_All_Specie_Daily_1 <- air_data_india_All_Specie_Daily %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")
air_data_india_All_Specie_Daily_1

air_data_india_All_Specie_Daily_2 <- air_data_india_All_Specie_Daily_1 %>%
  spread(key = Specie, value = AQI)
head(air_data_india_All_Specie_Daily_2)
tail(air_data_india_All_Specie_Daily_2)

air_data_india_All_Specie_Daily_2[-4] %>%
  drop_na() %>%
  cor()

air_data_india_All_Specie_Daily_2[-4] %>%
  ggpairs()

# Actually here all the parameters with all the others except Year and Month and Day are really highly correlated. So let's take a look at them  --To cluttered Plots
air_data_india_All_Specie_Daily_2 %>%
  ggpairs(columns = c("co", "no2", "o3", "so2", "pm10", "pm25"), aes(color = cut_width(Year, 1), alpha = 1/20)) +
  scale_color_viridis(discrete = TRUE)

air_data_india_All_Specie_Daily_2 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  ggpairs(columns = c("dew", "humidity", "precipitation", "pressure", "temperature", "wind-gust", "wind-speed"), aes(color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  scale_color_viridis(discrete = TRUE)

air_data_india_Pollutants_nonpollutants_daily <- air_data_india_All_Specie_Daily_1 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  spread(key = Specie, value = AQI) %>%
  select(-precipitation) %>%
  gather(dew, humidity, pressure, temperature, `wind-gust`, `wind-speed`, key = "non_pollutants", value = Levels) %>%
  gather(co, so2, no2, o3, pm10, pm25, key = "pollutants", value = "AQI")
air_data_india_Pollutants_nonpollutants_daily

air_data_india_Pollutants_nonpollutants_daily %>%
  ggplot(aes(AQI, Levels, fill = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  geom_hex() +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~pollutants + non_pollutants, scales = "free", labeller = label_wrap_gen(multi_line=FALSE))


# Monthly Variances among all species
air_data_india_All_Specie_Monthly_var <- air_data_india %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month, Specie) %>%
  summarise(Avg_Variance = mean(variance, na.rm = TRUE))
print(air_data_india_All_Specie_Monthly_var, n = 1000)

air_data_india_All_Specie_Monthly_var_1 <- air_data_india_All_Specie_Monthly_var %>%
  spread(key = Specie, value = Avg_Variance)
print(air_data_india_All_Specie_Monthly_var_1, n = 1000)

air_data_india_All_Specie_Monthly_var_1 %>%
  ggpairs()

# Actually here all the parameter's avg variance with all the others except Year and Month are really highly correlated. So let's take a look at them 
air_data_india_All_Specie_Monthly_var_1 %>%
  ggpairs(columns = c("co", "no2", "o3", "so2", "pm10", "pm25"), aes(color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  scale_color_viridis(discrete = TRUE)

air_data_india_All_Specie_Monthly_var_1 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  ggpairs(columns = c("dew", "humidity", "pressure", "temperature", "wind-gust", "wind-speed"), aes(color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  scale_color_viridis(discrete = TRUE)

air_data_india_Pollutants_nonpollutants_monthly_var_1 <- air_data_india_All_Specie_Monthly_var_1 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  select(-precipitation) %>%
  gather(dew, humidity, pressure, temperature, `wind-gust`, `wind-speed`, key = "non_pollutants", value = "Levels_variance") %>%
  gather(co, so2, no2, o3, pm10, pm25, key = "pollutants", value = "AQI_variance")
air_data_india_Pollutants_nonpollutants_monthly_var_1

air_data_india_Pollutants_nonpollutants_monthly_var_1 %>%
  ggplot(aes(AQI_variance, Levels_variance, color = cut_width(Year, 1), shape = cut_width(Year, 1))) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~pollutants + non_pollutants, scales = "free", labeller = label_wrap_gen(multi_line=FALSE))

# Daily Variances among all species
air_data_india_All_Specie_Daily_var <- air_data_india %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017))) %>%
  group_by(Year, Month, Day, Specie) %>%
  summarise(Avg_Variance = mean(variance, na.rm = TRUE))
air_data_india_All_Specie_Daily_var
tail(air_data_india_All_Specie_Daily_var)

air_data_india_All_Specie_Daily_var_1 <- air_data_india_All_Specie_Daily_var %>%
  spread(key = Specie, value = Avg_Variance)
air_data_india_All_Specie_Daily_var_1
tail(air_data_india_All_Specie_Daily_var_1)

air_data_india_All_Specie_Daily_var_1 %>%
  ggpairs()


# Behaviour of Non-Pollutants with Months
air_data_india_Pollutants_nonpollutants_monthly %>%
  ggplot(aes(Month, Levels, fill = cut_width(Year, 1))) +
  geom_hex() +
  facet_wrap(~non_pollutants, scales = "free") +
  labs(fill = "Year", y = "Levels")+
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) +
  scale_fill_viridis(discrete = TRUE)
  
# Exploring some relation between COVID cases and Pollution levels -- Nothing Fruitful
covid_data
tail(covid_data)

covid_data_states_merged <- covid_data %>%
  group_by(Year, Month, Day) %>%
  summarise(Total_Cured = sum(Cured), Total_Deaths = sum(Deaths), Total_Confirmed = sum(Confirmed))
covid_data_states_merged

covid_data_states_merged_monthly <- covid_data %>%
  group_by(Year, Month) %>%
  summarise(Total_Cured = sum(Cured), Total_Deaths = sum(Deaths), Total_Confirmed = sum(Confirmed))
covid_data_states_merged_monthly

covid_data_states_merged_monthly$Total_Confirmed <- c(covid_data_states_merged_monthly$Total_Confirmed[1], diff(covid_data_states_merged_monthly$Total_Confirmed))
covid_data_states_merged_monthly$Total_Deaths <- c(covid_data_states_merged_monthly$Total_Deaths[1], diff(covid_data_states_merged_monthly$Total_Deaths))
covid_data_states_merged_monthly$Total_Cured <- c(covid_data_states_merged_monthly$Total_Cured[1], diff(covid_data_states_merged_monthly$Total_Cured))
covid_data_states_merged_monthly

covid_pollutant_data <- Monthly_Pollutant %>%
  filter(Year %in% c(2020, 2021)) %>%
  full_join(covid_data_states_merged_monthly, by = c("Year", "Month"))
covid_pollutant_data

covid_pollutant_data %>%
  ggplot(aes(Total_Confirmed, Avg_Median)) +
  geom_point() +
  xlim(min(covid_pollutant_data$Total_Confirmed), 1e8)
