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
library(pander)
library(stargazer)
library(forcats)

# ------------2019-------------
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Max") %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))
air_data_pollutants_2019_avg_median

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm

air_data_pollutants_2019_avg_median_all_pol_summarized_pm10 <- air_data_pollutants_2019_avg_median %>%
  filter((pollutants %in% c("pm10"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2019_avg_median_all_pol_summarized_pm10

air_data_pollutants_2019_avg_median_all_pol_summarized_pm25 <- air_data_pollutants_2019_avg_median %>%
  filter((pollutants %in% c("pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2019_avg_median_all_pol_summarized_pm25

# Total Pollutant - wo pm
air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm %>%
  ggplot(aes(Mean_AQI, ..density..)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 50))

# Normal Data fits well
fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI)

# Pollutant-wise
# co
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "co") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# gamma & lognormal fits really well needs to be compared
fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI)


# no2
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "no2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# All three fits similar need to compare
fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "no2")$AQI)

# o3
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "o3") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None seems to fit well
fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI)

# so2
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "so2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None fits well
fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI)

# pm10
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "pm10") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None well
fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI)

# pm25
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "pm25") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# Normal and weibull fits well needs to be compared
fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI)


# ------------2020-------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Max") %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))
air_data_pollutants_2020_avg_median

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm

air_data_pollutants_2020_avg_median_all_pol_summarized_pm10 <- air_data_pollutants_2020_avg_median %>%
  filter((pollutants %in% c("pm10"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_pm10

air_data_pollutants_2020_avg_median_all_pol_summarized_pm25 <- air_data_pollutants_2020_avg_median %>%
  filter((pollutants %in% c("pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_pm25

# Total Pollutant - wo pm
air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm %>%
  ggplot(aes(Mean_AQI, ..density..)) +
  geom_histogram(binwidth = 0.7)

fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI)

# Pollutant-wise
# co
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "co") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "co")$AQI)

# no2
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "no2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "no2")$AQI)

# o3
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "o3") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal and Gamma fits well rest two also fits not that bad
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "o3")$AQI)

# so2
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "so2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None fits well
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "so2")$AQI)

# pm10
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "pm10") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal fits well except for the tail
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm10")$AQI)

# pm25
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "pm25") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# Okayish fit by all curves
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm25")$AQI)

# First 6 months Only
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Max" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))
air_data_pollutants_2020_avg_median

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm

air_data_pollutants_2020_avg_median_all_pol_summarized_pm10 <- air_data_pollutants_2020_avg_median %>%
  filter((pollutants %in% c("pm10"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_pm10

air_data_pollutants_2020_avg_median_all_pol_summarized_pm25 <- air_data_pollutants_2020_avg_median %>%
  filter((pollutants %in% c("pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_pm25

# Total Pollutant - wo pm
air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm %>%
  ggplot(aes(Mean_AQI, ..density..)) +
  geom_histogram(binwidth = 0.7) +
  coord_cartesian(xlim = c(3, 13)) # Not a very promising histogram

fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI)

# Pollutant-wise
# co
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "co") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "co")$AQI)

# no2
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "no2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "no2")$AQI)

# o3
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "o3") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal and Gamma fits well rest two also fits not that bad
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "o3")$AQI)

# so2
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "so2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None fits well
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "so2")$AQI)

# pm10
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "pm10") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal fits well except for the tail
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm10")$AQI)

# pm25
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "pm25") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# Okayish fit by all curves
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm25")$AQI)

# Last 6 months Only
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Max" & !(Month %in% c(1,2,3,4,5,6))) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))
air_data_pollutants_2020_avg_median

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm

air_data_pollutants_2020_avg_median_all_pol_summarized_pm10 <- air_data_pollutants_2020_avg_median %>%
  filter((pollutants %in% c("pm10"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_pm10

air_data_pollutants_2020_avg_median_all_pol_summarized_pm25 <- air_data_pollutants_2020_avg_median %>%
  filter((pollutants %in% c("pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2020_avg_median_all_pol_summarized_pm25

# Total Pollutant - wo pm
air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm %>%
  ggplot(aes(Mean_AQI, ..density..)) +
  geom_histogram(binwidth = 0.7) +
  coord_cartesian(xlim = c(3, 13)) # Not a very promising histogram

fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI)

# Pollutant-wise
# co
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "co") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "co")$AQI)

# no2
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "no2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "no2")$AQI)

# o3
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "o3") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal and Gamma fits well rest two also fits not that bad
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "o3")$AQI)

# so2
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "so2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None fits well
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "so2")$AQI)

# pm10
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "pm10") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal fits well except for the tail
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm10")$AQI)

# pm25
air_data_pollutants_2020_avg_median %>%
  filter(pollutants == "pm25") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# Okayish fit by all curves
fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm25")$AQI)


# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Max" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))
air_data_pollutants_2021_avg_median

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm

air_data_pollutants_2021_avg_median_all_pol_summarized_pm10 <- air_data_pollutants_2021_avg_median %>%
  filter((pollutants %in% c("pm10"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2021_avg_median_all_pol_summarized_pm10

air_data_pollutants_2021_avg_median_all_pol_summarized_pm25 <- air_data_pollutants_2021_avg_median %>%
  filter((pollutants %in% c("pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_pollutants_2021_avg_median_all_pol_summarized_pm25

# Total Pollutant - wo pm
air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm %>%
  ggplot(aes(Mean_AQI, ..density..)) +
  geom_histogram()

fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI)

# Pollutant-wise
# co
air_data_pollutants_2021_avg_median %>%
  filter(pollutants == "co") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "co")$AQI)

# no2
air_data_pollutants_2021_avg_median %>%
  filter(pollutants == "no2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None follows
fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "no2")$AQI)

# o3
air_data_pollutants_2021_avg_median %>%
  filter(pollutants == "o3") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal and Gamma fits well rest two also fits not that bad
fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "o3")$AQI)

# so2
air_data_pollutants_2021_avg_median %>%
  filter(pollutants == "so2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None fits well
fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "so2")$AQI)

# pm10
air_data_pollutants_2021_avg_median %>%
  filter(pollutants == "pm10") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# lognormal fits well except for the tail
fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "pm10")$AQI)

# pm25
air_data_pollutants_2021_avg_median %>%
  filter(pollutants == "pm25") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# Okayish fit by all curves
fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI)
