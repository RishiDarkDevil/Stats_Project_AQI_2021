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
  filter(Year == 2019 & Measure == "Avg_Median") %>%
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
  coord_cartesian(xlim = c(0, 50)) # Not a very promising histogram

# Pollutant-wise
# co
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "co") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# gamma & lognormal fits really well needs to be compared
fit_g <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI, "gamma") 
fit_l <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI, "lnorm") 
fit_w <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI, "weibull")

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_g, fit_l, fit_w), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w), legendtext = plot.legend)
par(mfrow = c(1,1))

# no2
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "no2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# All three fits similar need to compare
fit_g <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "no2")$AQI, "gamma")
fit_l <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "no2")$AQI, "lnorm")
fit_w <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "no2")$AQI, "weibull")

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_g, fit_l, fit_w), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w), legendtext = plot.legend)
par(mfrow = c(1,1))

# o3
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "o3") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None seems to fit well
fit_g <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "gamma")
fit_l <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "lnorm")
fit_w <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "weibull")
fit_n <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "norm")

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull", "normal")
fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
par(mfrow = c(1,1))

# so2
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "so2") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None fits well
fit_g <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "gamma")
fit_l <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "lnorm")
fit_w <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "weibull")
fit_n <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "norm")

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull", "normal")
fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
par(mfrow = c(1,1))

# pm10
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "pm10") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# None well
fit_g <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "gamma")
fit_l <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "lnorm")
fit_w <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "weibull")
fit_n <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "norm")

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull", "normal")
fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
par(mfrow = c(1,1))

# pm25
air_data_pollutants_2019_avg_median %>%
  filter(pollutants == "pm25") %>%
  ggplot(aes(AQI, ..density..)) +
  geom_histogram()

# Normal and weibull fits well needs to be compared
fit_g <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "gamma")
fit_l <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "lnorm")
fit_w <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "weibull")
fit_n <- fitdistrplus::fitdist(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "norm")

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull", "normal")
fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend)
par(mfrow = c(1,1))
