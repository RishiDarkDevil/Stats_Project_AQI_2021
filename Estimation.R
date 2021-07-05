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

# Trying to fit distribution to these histrograms
air_data_india_pollutants %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017)) & !(Specie %in% c('pm25', 'pm10'))) %>%
  ggplot(aes(median, ..density.., fill = cut_width(Year,1), group = Year)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 50)) +
  facet_wrap(~Year, scales = "free")

# 2019

air_data_median_AQI_2019_wo_pm <- air_data_median_AQI_2019 %>%
  filter(!(Specie %in% c('pm25', 'pm10')))

air_data_median_AQI_2019_wo_pm %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(air_data_median_AQI_2019_wo_pm$median, "gamma")
summary(fitted)
plot(fitted)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(air_data_median_AQI_2019_wo_pm$median, "exp")
summary(fitted)
plot(fitted)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(air_data_median_AQI_2019_wo_pm$median, "lnorm")
summary(fitted)
plot(fitted)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(air_data_median_AQI_2019_wo_pm$median, "weibull")
summary(fitted)
plot(fitted)
fit_4 <- dweibull(x, fitted$estimate[1], fitted$estimate[2])


fit <- tibble(x = x, y1 = fit_1, y2 = fit_2, y3 = fit_3, y4 = fit_4)

air_data_median_AQI_2019_wo_pm %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(bins = 45) +
  geom_line(data = fit, aes(x = x, y = y1), size = 1, color = "blue") +
  geom_line(data = fit, aes(x = x, y = y2), size = 1, color = "red") +
  geom_line(data = fit, aes(x = x, y = y3), size = 1, color = "green") +
  geom_line(data = fit, aes(x = x, y = y4), size = 1, color = "white") +
  coord_cartesian(xlim = c(0, 50)) # None seems to fit well 

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

air_data_median_AQI_2019_wo_pm %>%
  ggplot(aes(log(median), ..density..)) +
  geom_histogram(binwidth = 0.09) +
  coord_cartesian(xlim = c(-1, 4))

x <- seq(-1, 4, by = 0.1)

fitted <- fitdistrplus::fitdist(log(air_data_median_AQI_2019_wo_pm$median), "norm")
summary(fitted)
plot(fitted)
fit_4 <- dnorm(x, fitted$estimate[1], fitted$estimate[2])


fit <- tibble(x = x, y4 = fit_4)

air_data_median_AQI_2019_wo_pm %>%
  ggplot(aes(log(median), ..density..)) +
  geom_histogram(binwidth = 0.09) +
  geom_line(data = fit, aes(x = x, y = y4), size = 1, color = "white") +
  coord_cartesian(xlim = c(-1, 4))



fitdistrplus::descdist(air_data_median_AQI_2019_wo_pm$median, boot = 100)


# Trying with individual pollutants

# 2019
# co
air_data_median_AQI_2019_wo_pm %>%
  filter(Specie == "co") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 30))

x <- seq(0, 30, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "co")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "co")$median, "chisq")
summary(fit_2)
plot(fit_2)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "co")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits really well
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "co")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

#ests <- bootdist(fit_3, niter = 500)
#summary(ests)
#plot(ests)

# no2
air_data_median_AQI_2019_wo_pm %>%
  filter(Specie == "no2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "no2")$median, "gamma")
summary(fit_1)
plot(fit_1) # Gamma Fits okayish
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "no2")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "no2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

#ests <- bootdist(fit_1, niter = 500)
#summary(ests)
#plot(ests)

# so2
air_data_median_AQI_2019_wo_pm %>%
  filter(Specie == "so2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 20))

x <- seq(0, 20, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "so2")$median, "gamma")
summary(fit_1)
plot(fit_1) 
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "so2")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "so2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# o3
air_data_median_AQI_2019_wo_pm %>%
  filter(Specie == "o3") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "o3")$median, "gamma")
summary(fit_1)
plot(fit_1)  # Fits really well
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "o3")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019_wo_pm, Specie == "o3")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# pm10
air_data_median_AQI_2019 %>%
  filter(Specie == "pm10") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 300))

x <- seq(0, 300, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019, Specie == "pm10")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019, Specie == "pm10")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2019, Specie == "pm10")$median, "weibull")
summary(fit_4)
plot(fit_4) # Fits Okayish

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# 2020
air_data_median_AQI_2020 <- air_data_india_pollutants %>%
  filter(Year == 2020)
air_data_median_AQI_2020

air_data_median_AQI_2020_wo_pm <- air_data_median_AQI_2020 %>%
  filter(!(Specie %in% c('pm25', 'pm10')))
air_data_median_AQI_2020_wo_pm

air_data_median_AQI_2020_wo_pm %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "gamma")
summary(fitted)
plot(fitted)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "exp")
summary(fitted)
plot(fitted)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "lnorm")
summary(fitted)
plot(fitted)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "weibull")
summary(fitted)
plot(fitted)
fit_4 <- dweibull(x, fitted$estimate[1], fitted$estimate[2])

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# co
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "co") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 30))

x <- seq(0, 30, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "chisq")
summary(fit_2)
plot(fit_2)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits really well
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

# no2
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "no2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "no2")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "no2")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "no2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

# so2
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "so2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 20))

x <- seq(0, 20, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "so2")$median, "gamma")
summary(fit_1)
plot(fit_1) # Fits okayish
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "so2")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "so2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# o3
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "o3") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "o3")$median, "gamma")
summary(fit_1)
plot(fit_1)  # Fits okayish
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "o3")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "o3")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# pm10
air_data_median_AQI_2020 %>%
  filter(Specie == "pm10") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 300))

x <- seq(0, 300, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020, Specie == "pm10")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020, Specie == "pm10")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits Okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020, Specie == "pm10")$median, "weibull")
summary(fit_4)
plot(fit_4) 

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# 2021
air_data_median_AQI_2021 <- air_data_india_pollutants %>%
  filter(Year == 2021)
air_data_median_AQI_2021

air_data_median_AQI_2021_wo_pm <- air_data_median_AQI_2021 %>%
  filter(!(Specie %in% c('pm25', 'pm10')))
air_data_median_AQI_2021_wo_pm

air_data_median_AQI_2021_wo_pm %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(air_data_median_AQI_2021_wo_pm$median, "gamma")
summary(fitted)
plot(fitted)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(air_data_median_AQI_2021_wo_pm$median, "exp")
summary(fitted)
plot(fitted)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(air_data_median_AQI_2021_wo_pm$median, "lnorm")
summary(fitted)
plot(fitted)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(air_data_median_AQI_2021_wo_pm$median, "weibull")
summary(fitted)
plot(fitted)
fit_4 <- dweibull(x, fitted$estimate[1], fitted$estimate[2])

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# co
air_data_median_AQI_2021_wo_pm %>%
  filter(Specie == "co") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 30))

x <- seq(0, 30, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "co")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "co")$median, "chisq")
summary(fit_2)
plot(fit_2)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "co")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "co")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

# no2
air_data_median_AQI_2021_wo_pm %>%
  filter(Specie == "no2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "no2")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "no2")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "no2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

# so2
air_data_median_AQI_2021_wo_pm %>%
  filter(Specie == "so2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 20))

x <- seq(0, 20, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "so2")$median, "gamma")
summary(fit_1)
plot(fit_1) 
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "so2")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "so2")$median, "weibull")
summary(fit_4)
plot(fit_4)# Fits really well

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# o3
air_data_median_AQI_2021_wo_pm %>%
  filter(Specie == "o3") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "o3")$median, "gamma")
summary(fit_1)
plot(fit_1) 
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "o3")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021_wo_pm, Specie == "o3")$median, "weibull")
summary(fit_4)
plot(fit_4) # Fits really well

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# pm10
air_data_median_AQI_2021 %>%
  filter(Specie == "pm10") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 300))

x <- seq(0, 300, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021, Specie == "pm10")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021, Specie == "pm10")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits Okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2021, Specie == "pm10")$median, "weibull")
summary(fit_4)
plot(fit_4) 

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# 2020 - First 6 months
air_data_median_AQI_2020 <- air_data_india_pollutants %>%
  filter(Year == 2020 & Month %in% c(1,2,3,4,5,6))
air_data_median_AQI_2020

air_data_median_AQI_2020_wo_pm <- air_data_median_AQI_2020 %>%
  filter(!(Specie %in% c('pm25', 'pm10')))
air_data_median_AQI_2020_wo_pm

air_data_median_AQI_2020_wo_pm %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "gamma")
summary(fitted)
plot(fitted)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "exp")
summary(fitted)
plot(fitted)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "lnorm")
summary(fitted)
plot(fitted)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(air_data_median_AQI_2020_wo_pm$median, "weibull")
summary(fitted)
plot(fitted)
fit_4 <- dweibull(x, fitted$estimate[1], fitted$estimate[2])

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# co
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "co") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 30))

x <- seq(0, 30, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_2 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "chisq")
summary(fit_2)
plot(fit_2)
fit_2 <- dexp(x, fitted$estimate[1])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "co")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

# no2
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "no2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "no2")$median, "gamma")
summary(fit_1)
plot(fit_1) # Fits okayish
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "no2")$median, "lnorm")
summary(fit_3)
plot(fit_3) 
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "no2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

# so2
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "so2") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 20))

x <- seq(0, 20, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "so2")$median, "gamma")
summary(fit_1)
plot(fit_1) # Fits well to okayish
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "so2")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "so2")$median, "weibull")
summary(fit_4)
plot(fit_4)

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# o3
air_data_median_AQI_2020_wo_pm %>%
  filter(Specie == "o3") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 2.5) +
  coord_cartesian(xlim = c(0, 50))

x <- seq(0, 50, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "o3")$median, "gamma")
summary(fit_1)
plot(fit_1) 
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "o3")$median, "lnorm")
summary(fit_3)
plot(fit_3)
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020_wo_pm, Specie == "o3")$median, "weibull")
summary(fit_4)
plot(fit_4) # Fits well to okayish

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))


# pm10
air_data_median_AQI_2020 %>%
  filter(Specie == "pm10") %>%
  ggplot(aes(median, ..density..)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0, 300))

x <- seq(0, 300, by = 0.1)
fit_1 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020, Specie == "pm10")$median, "gamma")
summary(fit_1)
plot(fit_1)
fit_1 <- dgamma(x, fitted$estimate[1], fitted$estimate[2])
fit_3 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020, Specie == "pm10")$median, "lnorm")
summary(fit_3)
plot(fit_3) # Fits Okayish
fit_3 <- dlnorm(x, fitted$estimate[1], fitted$estimate[2])
fit_4 <- fitdistrplus::fitdist(filter(air_data_median_AQI_2020, Specie == "pm10")$median, "weibull")
summary(fit_4)
plot(fit_4) 

par(mfrow=c(2,2))
plot.legend <- c("gamma", "lognormal", "weibull")
fitdistrplus::denscomp(list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::cdfcomp (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::qqcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
fitdistrplus::ppcomp  (list(fit_1, fit_3, fit_4), legendtext = plot.legend)
par(mfrow = c(1,1))

