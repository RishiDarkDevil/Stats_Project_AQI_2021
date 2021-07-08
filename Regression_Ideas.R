# An overall study is lil difficult so we will focus on Top City wise Regression

air_data_india_All_Specie_Daily_Regress %>%
  filter(City %in% c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai", "Lucknow", "Patna", "Chandigarh", "Gandhinagar", "Jaipur")) %>%
  filter(Year == 2019, Measure != "Var") %>% #, Month %in% c(1,2,3,4,5,6)
  group_by(Year, Month, Day, City) %>%
  mutate_at(.vars = c("so2", "no2", "co", "o3", "pm10", "pm25", "humidity", "dew", "temperature", "precipitation", "wind-gust", "wind-speed", "pressure"), ~mean(.,na.rm = TRUE)) %>%
  #summarise(mean_pm25 = mean(pm25, na.rm = TRUE), mean_wind_speed = mean(`wind-speed`, na.rm = TRUE)) %>%
  ggplot(aes(so2, pm25, color = City)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~City, scales = "free")

# --------------2019 First 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar"))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# --------------------Trying to predict First 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Training 2020 First 6 Month Model

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2020 fitted data
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table


# --------------------Training 2021 First 6 Month
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# Let's see our model performance
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

# -------------------------SAME ANALYSIS AS ABOVE WITH THE LAST 6 MONTHS
# --------------2019 Last 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar"))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict Last 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table


# --------------------Training 2020 Last 6 Month Model

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table


# --------------------------------- Pollutants Dependence on Other Pollutants

# --------------2019 First 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar"))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict First 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
prediction$res_pm25

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Training 2020 First 6 Month Model

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2020 fitted data
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table


# --------------------Training 2021 First 6 Month
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# Let's see our model performance
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

# -------------------------SAME ANALYSIS AS ABOVE WITH THE LAST 6 MONTHS
# --------------2019 Last 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar"))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
model_summaries <- print_all_city_model_summary()
model_summaries$model_so2
model_summaries$model_no2
model_summaries$model_co
model_summaries$model_o3
model_summaries$model_pm10
model_summaries$model_pm25

predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table

# --------------------Trying to predict Last 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
# train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table


# --------------------Training 2020 Last 6 Month Model

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(so2)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`)) %>%
  filter(!is.na(so2)) %>%
  filter(!is.na(no2)) %>%
  filter(!is.na(co)) %>%
  filter(!is.na(o3)) %>%
  filter(!is.na(pm25)) %>%
  filter(!(is.na(pm10) & City == "Chandigarh")) %>%
  filter(!(is.na(pm10) & City == "Gandhinagar")) %>%
  filter(!(is.na(dew))) %>%
  filter(!(is.na(humidity))) %>%
  filter(!(is.na(pressure))) %>%
  filter(!(is.na(temperature))) %>%
  filter(!(is.na(`wind-speed`)))
air_data_india_All_Specie_Daily_Regress_model
nrow(air_data_india_All_Specie_Daily_Regress_model)

# Let's See the distrbution of the Pollutants in each city
city_distributions <- fit_distribution_all_cities(air_data_india_All_Specie_Daily_Regress_model)
city_distributions$fit_so2
city_distributions$fit_no2
city_distributions$fit_o3
city_distributions$fit_co
city_distributions$fit_pm10
city_distributions$fit_pm25

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# Let's see our model performance
prediction <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

prediction_R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction_R2$Prediction_Table


# -----------------LOOKING AT ADDED VARIABLE PLOTS
added_variable_plot(air_data_india_All_Specie_Daily_Regress_model, model_s1, model_n1)
