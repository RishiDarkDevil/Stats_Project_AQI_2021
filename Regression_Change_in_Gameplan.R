# An overall study is lil difficult so we will focus on Top City wise Regression
correct_mumbai_temp <- function(x) return(ifelse(x < 10, x, ))

# Some preliminary cleaning
air_data_india_All_Specie_Daily_AQI_formulated <- air_data_india %>%
  group_by(Year, Month, Day,  Specie, City) %>%
  summarise(Min = mean(min, na.rm = TRUE), Median = mean(median, na.rm = TRUE), Max = mean(max, na.rm = TRUE), Var = mean(variance, na.rm = TRUE)) %>%
  gather(Min, Median, Max, Var, key = "Measure", value = "AQI") %>%
  spread(key = Specie, value = AQI) %>%
  filter(Year %in% c(2019, 2020, 2021), Measure != "Var") %>%
  group_by(Year, Month, Day, City) %>%
  mutate(so2 = mean(so2, na.rm = TRUE), no2 = mean(no2, na.rm = TRUE), co = mean(co, na.rm = TRUE), o3 = mean(o3, na.rm = TRUE), pm10 = mean(pm10, na.rm = TRUE), pm25 = mean(pm25, na.rm = TRUE), dew = mean(dew, na.rm = TRUE), humidity = mean(humidity, na.rm = TRUE), pessure = mean(pressure, na.rm = TRUE), temperature = mean(temperature, na.rm = TRUE), `wind-speed` = mean(`wind-speed`, na.rm = TRUE)) %>%
  mutate(AQI = max(co, no2, o3, so2, pm10, pm25, na.rm = TRUE))

air_data_india_All_Specie_Daily_AQI_formulated <- air_data_india_All_Specie_Daily_AQI_formulated %>%
  filter(City %in% c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai", "Lucknow", "Patna", "Chandigarh", "Gandhinagar", "Jaipur")) %>%
  mutate(Year = as.character(Year)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(City == "Mumbai" & temperature < 15)) %>%
  filter(!((City == "Jaipur" & o3 > 100) | (City == "Lucknow" & o3 > 100) | (City == "Chandigarh" & o3 > 100))) %>%
  filter(!((City == "Mumbai" & co > 50))) %>%
  filter(!((City == "Patna" & no2 > 50))) %>%
  filter(!((City == "Patna" & so2 > 100))) %>%
  filter((humidity >= 20 & humidity <= 100) & (dew >= 0 & dew <= 50) & (temperature >= 0 & temperature <= 55) & (pressure >= 600)) %>%
  filter(Measure == "Median") %>%
  select(-c(Measure, precipitation, `wind-gust`))
  
air_data_india_All_Specie_Daily_Regress <- air_data_india_All_Specie_Daily_Regress %>%
  filter(City %in% c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai", "Lucknow", "Patna", "Chandigarh", "Gandhinagar", "Jaipur")) %>%
  filter(Year %in% c(2019, 2020, 2021), Measure != "Var") %>% #, Month %in% c(1,2,3,4,5,6)
  group_by(Year, Month, Day, City) %>%
  #mutate_at(.vars = c("so2", "no2", "co", "o3", "pm10", "pm25", "humidity", "dew", "temperature", "precipitation", "wind-gust", "wind-speed", "pressure"), ~mean(.,na.rm = TRUE)) %>%
  #summarise(mean_pm25 = mean(pm25, na.rm = TRUE), mean_wind_speed = mean(`wind-speed`, na.rm = TRUE)) %>%
  mutate(Year = as.character(Year)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(City == "Mumbai" & temperature < 15)) %>%
  filter(!((City == "Jaipur" & o3 > 100) | (City == "Lucknow" & o3 > 100) | (City == "Chandigarh" & o3 > 100))) %>%
  filter(!((City == "Mumbai" & co > 50))) %>%
  filter(!((City == "Patna" & no2 > 50))) %>%
  filter(!((City == "Patna" & so2 > 100))) %>%
  filter((humidity >= 20 & humidity <= 100) & (dew >= 0 & dew <= 50) & (temperature >= 0 & temperature <= 55) & (pressure >= 600))

# Visualizing section
air_data_india_All_Specie_Daily_Regress %>%
  filter(Measure == "Max") %>%
  ggplot(aes(no2, so2, color = Year)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~City, scales = "free")

# --------------2019 First 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6))

# Training
full_train_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)

# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# Let's see our model performance - Little more detailed
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

# Effects Plot
effect_plot(model = model_p.1, pred = temperature, interval = TRUE, plot.points = TRUE, int.width = 0.99, int.type = "confidence")
plot_summs(model_p.1, model_p.2, model_p.3, model_p.4, model_p.5, model_p.6, model_p.7, ci_level = 0.99, plot.distributions = TRUE, rescale.distributions = TRUE)

# --------------------Trying to predict First 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(Month %in% c(3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# Prediction visuallization
predictions <- predict_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)
predictions$res_pm25

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter(Month %in% c(3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# --------------2019 Last 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019) %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6)))

# Training
full_train_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)

# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# Let's see our model performance - Little more detailed
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

# --------------------Trying to predict Last 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# --------------2020 First 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6))

# Training
full_train_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)

# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# Let's see our model performance - Little more detailed
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2020 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table


# ------------Training 2020 Last 6 Months
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

full_train_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)
# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# ------------Training 2021 First 6 Months 
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
air_data_india_All_Specie_Daily_Regress_model

# Partial train with only other polutants
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# Partial train with only weather parameters
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Full train with all other parameters
full_train_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)
# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

# Detailed Model Summary
model_summaries <- print_all_city_model_summary()
model_summaries$model_pm10

# predicting for 25th June Kolkata Bidhanagar
predict_data <- tibble(so2 = 5, co = 24, o3 = 34, no2 = 16, pm25 = 57, pm10 = 67)

predict(model_s1, predict_data, interval = "prediction")
predict(model_c1, predict_data, interval = "prediction")
predict(model_o1, predict_data, interval = "prediction")
predict(model_n1, predict_data, interval = "prediction")
predict(model_p1, predict_data, interval = "prediction")
predict(model_p.1, predict_data, interval = "prediction")



model_kol <- train_city_pol_w_pol(air_data_india_All_Specie_Daily_Regress_model %>% filter(City == "Kolkata"), "Kolkata")
model_kol$prediction.table.otherpol
