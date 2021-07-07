# ---------------------------------Regression of combined Pollutants on non-pollutants

# --------First 6 Months
# -----Monthly
air_data_india_All_Specie_Monthly_2

air_data_india_All_Specie_Monthly_2_pollutants_combined <- air_data_india_All_Specie_Monthly_2 %>%
  filter(Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Mean_Pollutant_AQI = (co + no2 + o3 + so2 + pm10 + pm25) / 4)
air_data_india_All_Specie_Monthly_2_pollutants_combined

air_data_india_All_Specie_Monthly_2_pollutants_combined %>%
  ggpairs(columns = c("Mean_Pollutant_AQI", "precipitation", "temperature", "wind-gust", "wind-speed", "humidity", "dew", "Year", "Month"))

model <- lm(Mean_Pollutant_AQI ~ temperature + humidity + dew + precipitation + pressure + `wind-gust` + `wind-speed` + Year + Month, data = air_data_india_All_Specie_Monthly_2_pollutants_combined)
summary(model)

grid <- get_regression_points(model)
grid

grid %>%
  ggplot(aes(residual, ..density..)) +
  geom_histogram(bins = 15)

compare_hellinger_fit_v_MLE(grid$residual, "pnorm")


model <- lm(Mean_Pollutant_AQI ~ temperature + humidity + dew + precipitation + pressure + `wind-gust` + `wind-speed` + Year + Month, data = air_data_india_All_Specie_Monthly_2_pollutants_combined %>% filter(Year == 2019))
summary(model)

grid <- get_regression_points(model)
grid

grid %>%
  ggplot(aes(residual, ..density..)) +
  geom_histogram(bins = 15)


predict_2020_based_on_2019 <- air_data_india_All_Specie_Monthly_2_pollutants_combined %>%
  filter(Year == 2020) %>%
  add_predictions(model) %>%
  add_residuals(model)
predict_2020_based_on_2019

predict_2020_based_on_2019 %>%
  ggplot(aes(resid, ..density..)) +
  geom_histogram(bins = 15)


predict_2021_based_on_2019 <- air_data_india_All_Specie_Monthly_2_pollutants_combined %>%
  filter(Year == 2021) %>%
  add_predictions(model) %>%
  add_residuals(model)
predict_2021_based_on_2019

predict_2021_based_on_2019 %>%
  ggplot(aes(resid, ..density..)) +
  geom_histogram(bins = 15)

model <- lm(Mean_Pollutant_AQI ~ temperature + humidity + dew + precipitation + pressure + `wind-gust` + `wind-speed` + Year + Month, data = air_data_india_All_Specie_Monthly_2_pollutants_combined %>% filter(Year == 2021))
summary(model)

grid <- get_regression_points(model)
grid

grid %>%
  ggplot(aes(residual, ..density..)) +
  geom_histogram(bins = 15)


# ----Daily
air_data_india_All_Specie_Daily_2_pollutants_combined <- air_data_india_All_Specie_Daily_2 %>%
  filter(Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Mean_Pollutant_AQI = (co + no2 + o3 + so2 + pm10 + pm25) / 6)
air_data_india_All_Specie_Daily_2_pollutants_combined

air_data_india_All_Specie_Daily_2_pollutants_combined %>%
  ggpairs(columns = c("Mean_Pollutant_AQI", "precipitation", "temperature", "wind-gust", "wind-speed", "humidity", "dew", "Year", "Month"))

model <- lm(Mean_Pollutant_AQI ~ temperature + humidity + dew + precipitation + pressure + `wind-gust` + `wind-speed` + Year + Month + Day, data = air_data_india_All_Specie_Daily_2_pollutants_combined)
summary(model)

grid <- get_regression_points(model)
grid

grid %>%
  ggplot(aes(residual, ..density..)) +
  geom_histogram(bins = 15)

compare_hellinger_fit_v_MLE(grid$residual, "pnorm")


model <- lm(Mean_Pollutant_AQI ~ temperature + humidity + dew + precipitation + pressure + `wind-gust` + `wind-speed` + Year + Month + Day, data = air_data_india_All_Specie_Daily_2_pollutants_combined %>% filter(Year == 2019))
summary(model)

grid <- get_regression_points(model)
grid

grid %>%
  ggplot(aes(residual, ..density..)) +
  geom_histogram(bins = 15)


predict_2020_based_on_2019 <- air_data_india_All_Specie_Daily_2_pollutants_combined %>%
  filter(Year == 2020) %>%
  add_predictions(model) %>%
  add_residuals(model)
predict_2020_based_on_2019

predict_2020_based_on_2019 %>%
  ggplot(aes(resid, ..density..)) +
  geom_histogram(bins = 15)


predict_2021_based_on_2019 <- air_data_india_All_Specie_Daily_2_pollutants_combined %>%
  filter(Year == 2021) %>%
  add_predictions(model) %>%
  add_residuals(model)
predict_2021_based_on_2019

predict_2021_based_on_2019 %>%
  ggplot(aes(resid, ..density..)) +
  geom_histogram(bins = 15)

model <- lm(Mean_Pollutant_AQI ~ temperature + humidity + dew + precipitation + pressure + `wind-gust` + `wind-speed` + Year + Month + Day, data = air_data_india_All_Specie_Daily_2_pollutants_combined %>% filter(Year == 2021))
summary(model)

grid <- get_regression_points(model)
grid

grid %>%
  ggplot(aes(residual, ..density..)) +
  geom_histogram(bins = 15)



# ------------------------------------------------------------------------------
# Prepping Data
air_data_india_All_Specie_Daily_Regress <- air_data_india %>%
  group_by(Year, Month, Day,  Specie, City) %>%
  summarise(Min = mean(min, na.rm = TRUE), Median = mean(median, na.rm = TRUE), Max = mean(max, na.rm = TRUE), Var = mean(variance, na.rm = TRUE))
air_data_india_All_Specie_Daily_Regress

air_data_india_All_Specie_Daily_Regress <- air_data_india_All_Specie_Daily_Regress %>%
  gather(Min, Median, Max, Var, key = "Measure", value = "AQI")
air_data_india_All_Specie_Daily_Regress

air_data_india_All_Specie_Daily_Regress <- air_data_india_All_Specie_Daily_Regress %>%
  spread(key = Specie, value = AQI)
air_data_india_All_Specie_Daily_Regress

# Model Based on 2019 - First 6 Months
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  mutate(Pollutant_AQI = (co + no2 + o3 + so2 + pm10 + pm25) / 6)
air_data_india_All_Specie_Daily_Regress_model

air_data_india_All_Specie_Daily_Regress_model %>%
  select(-City) %>%
  ggpairs()

model <- lm(Pollutant_AQI ~ dew + humidity + temperature + `wind-speed`, data = air_data_india_All_Specie_Daily_Regress_model)
summary(model)
