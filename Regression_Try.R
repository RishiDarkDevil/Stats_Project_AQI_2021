# An overall study is lil difficult so we will focus on Top City wise Regression

air_data_india_All_Specie_Daily_Regress %>%
  filter(City %in% c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai", "Lucknow", "Patna", "Chandigarh", "Gandhinagar", "Jaipur")) %>%
  filter(Year == 2019, Measure != "Var") %>% #, Month %in% c(1,2,3,4,5,6)
  group_by(Year, Month, Day, City) %>%
  mutate_at(.vars = c("so2", "no2", "co", "o3", "pm10", "pm25", "humidity", "dew", "temperature", "precipitation", "wind-gust", "wind-speed", "pressure"), ~mean(.,na.rm = TRUE)) %>%
  #summarise(mean_pm25 = mean(pm25, na.rm = TRUE), mean_wind_speed = mean(`wind-speed`, na.rm = TRUE)) %>%
  ggplot(aes(co, no2, color = City)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~City, scales = "free")

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

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  mutate(Pollutant_AQI = (co + no2 + o3 + so2 + pm10 + pm25) / 6)
air_data_india_All_Specie_Daily_Regress_model

# --------------2019 First 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
  #filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Training residuals
residuals <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
residuals$res_so2
residuals$res_no2
residuals$res_co
residuals$res_o3
residuals$res_pm10
residuals$res_pm25

# Training pollutants AQI with other pollutants AQI Levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

# --------------------Trying to predict First 6 Months 2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var", Measure == "Median") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

# Non-pollutants based predictions --- MAKE SURE THE model_s1 and all model used in this part is trained with non-pollutants
residuals <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
residuals$res_so2
residuals$res_no2
residuals$res_co
residuals$res_o3
residuals$res_pm10
residuals$res_pm25

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021, Measure != "Var", Measure == "Median") %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

# Non-pollutants based predictions --- MAKE SURE THE model_s1 and all model used in this part is trained with non-pollutants
residuals <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
residuals$res_so2
residuals$res_no2
residuals$res_co
residuals$res_o3
residuals$res_pm10
residuals$res_pm25


# --------------2019 Last 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019, Measure != "Var") %>%
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>%
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Non-pollutants based predictions --- MAKE SURE THE model_s1 and all model used in this part is trained with non-pollutants
residuals <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
residuals$res_so2
residuals$res_no2
residuals$res_co
residuals$res_o3
residuals$res_pm10
residuals$res_pm25

# Training pollutants AQI with other pollutants AQI Levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)


# --------------------Trying to predict Last 6 Months2020 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>%
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>%
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

# Non-pollutants based predictions --- MAKE SURE THE model_s1 and all model used in this part is trained with non-pollutants
residuals <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
residuals$res_so2
residuals$res_no2
residuals$res_co
residuals$res_o3
residuals$res_pm10
residuals$res_pm25

# --------------2020 First 6 Months Based Model Train

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020, Measure != "Var") %>%
  filter(Month %in% c(1,2,3,4,5,6)) #%>%
#filter(Measure == "Max")
air_data_india_All_Specie_Daily_Regress_model

nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(precipitation)))# -- Too many missing precipitation value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-gust`)))# -- Too many missing wind-gust value let's drop it
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(dew)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(humidity)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(`wind-speed`)))
nrow(filter(air_data_india_All_Specie_Daily_Regress_model, is.na(pm10)))

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress_model %>%
  select(-c(precipitation, `wind-gust`))
air_data_india_All_Specie_Daily_Regress_model

# Training pollutant AQI with non-pollutant levels
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

# Training residuals
residuals <- predict_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
residuals$res_so2
residuals$res_no2
residuals$res_co
residuals$res_o3
residuals$res_pm10
residuals$res_pm25

# Training pollutants AQI with other pollutants AQI Levels
train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

