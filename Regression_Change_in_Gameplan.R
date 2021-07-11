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

suppressMessages(resplot <- predict_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model))
r <- resplot

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
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model)
prediction.R2$Prediction_Table

suppressMessages(resplot <- predict_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model))
resplot

# Prediction visuallization
predictions <- predict_all_cities_pol(air_data_india_All_Specie_Daily_Regress_model)
predictions$res_pm25

# --------------------Trying to predict First 6 Months 2021 AQI Level's of Pollutants from 2019 fitted data

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

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

plot_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, inner_ci_level = .95, plot.distributions = TRUE, rescale.distributions = TRUE)

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


# comparison Random Testing
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019) %>% # ------------LOOOOOOOK
  filter((Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

grid1 <- air_data_india_All_Specie_Daily_Regress_model %>%
  add_residuals(model_s1, var = "2019_resid")


air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter((Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

grid2 <- air_data_india_All_Specie_Daily_Regress_model %>%
  add_residuals(model_s1, var = "2020_resid")

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter((Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

grid3 <- air_data_india_All_Specie_Daily_Regress_model %>%
  add_residuals(model_s1, var = "2021_resid")

grid <- plyr::rbind.fill(as.data.frame(grid1$`2019_resid`),as.data.frame(grid2$`2020_resid`),as.data.frame(grid3$`2021_resid`))
grid <- tibble("resid_19" = as.vector(grid[,1]), "resid_20" = as.vector(grid[,2]), "resid_21" = as.vector(grid[,3]))

grid <- grid %>%
  gather(resid_19, resid_20, resid_21, key = "Residual_Year", value = "residuals")

make_predictions_comparison <- function(residual_data){
  p <- residual_data %>%
    ggplot(aes(residuals, ..density.., fill = Residual_Year)) +
    geom_histogram(position = "identity", alpha = 0.6) + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black"),strip.background = element_blank())
  return(p)
}

make_predictions_comparison(grid)

# ---

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019) %>% # ------------LOOOOOOOK
  filter((Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

grid1 <- air_data_india_All_Specie_Daily_Regress_model %>% filter(City == "Delhi") %>%
  add_residuals(model_p2, var = "2019_resid")


air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>% filter(City == "Delhi") %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter((Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

grid2 <- air_data_india_All_Specie_Daily_Regress_model %>%
  add_residuals(model_p2, var = "2020_resid")

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>% filter(City == "Delhi") %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter((Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

grid3 <- air_data_india_All_Specie_Daily_Regress_model %>%
  add_residuals(model_p2, var = "2021_resid")

grid <- plyr::rbind.fill(as.data.frame(grid1$`2019_resid`),as.data.frame(grid2$`2020_resid`),as.data.frame(grid3$`2021_resid`))
grid <- tibble("resid_19" = as.vector(grid[,1]), "resid_20" = as.vector(grid[,2]), "resid_21" = as.vector(grid[,3]))

grid <- grid %>%
  gather(resid_19, resid_20, resid_21, key = "Residual_Year", value = "residuals")

# --
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2019) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))
prediction.R2$Prediction_Table

t_reg0 <- ggtexttable(prediction.R2$Prediction_Table%>%
                        mutate(across(where(is.numeric), ~ round(., digits = 2))), theme = ttheme("mBlue"), rows = NULL) %>%
  tab_add_title("Model R2 on 2019 Data")

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))
prediction.R2$Prediction_Table

t_reg1 <- ggtexttable(prediction.R2$Prediction_Table%>%
                        mutate(across(where(is.numeric), ~ round(., digits = 2))), theme = ttheme("mViolet"), rows = NULL) %>%
  tab_add_title("Model R2 on 2020 Data")

air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2021) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

# Predicting
# Quick Review on different cities model fit
suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))
prediction.R2$Prediction_Table

t_reg2 <- ggtexttable(prediction.R2$Prediction_Table%>%
                        mutate(across(where(is.numeric), ~ round(., digits = 2))), theme = ttheme("mCyan"), rows = NULL) %>%
  tab_add_title("Model R2 on 2021 Data")

ggarrange(t_reg0, t_reg1, t_reg2, ncol = 3)

test_data <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(City == "Delhi") %>%
  filter(Month %in% c(1,2,3,4,5,6)) %>%  #------------LOOK
  group_by(Year, Month, City) %>%
  summarise(co = mean(co), no2 = mean(no2), o3 = mean(o3), so2 = mean(so2), pm10 = mean(pm10), pm25 = mean(pm25),dew = mean(dew), pressure = mean(pressure), humidity = mean(humidity), temperature = mean(temperature), `wind-speed`=mean(`wind-speed`))

test_data %>%
  spread_predictions(model_s2, model_n2, model_c2, model_o2, model_p2, model_p.2)

as_tibble(predict(model_s2, test_data, interval = "prediction", level = 0.99)) %>%
  mutate(actual = test_data$so2) %>%
  select(actual, everything())

# --
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(Month %in% c(1,2,3,4,5,6)) #%>% # ------------LOOK

header.true <- function(df) {
  colnames(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))

t_reg_wo_pol <- ggtexttable(as_tibble(header.true(t(prediction.R2$Prediction_Table[1:3] %>%
                                                      mutate(across(where(is.numeric), ~ round(., digits = 2))) ))), theme = ttheme("mVioletWhite"), rows = c("Kolkata", "Delhi")) %>%
  tab_add_title("Model R2", face = "bold")

del_f6ms_wo_pol <- plot_summs(model_s2, model_n2, model_c2, model_o2, model_p2, model_p.2, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Delhi - Pollutants On Weather Parameters")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) 
kol_f6ms_wo_pol <- plot_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Kolkata - Pollutants On Weather Parameters")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))

t_reg_pol <- ggtexttable(as_tibble(header.true(t(prediction.R2$Prediction_Table[1:3] %>%
                                         mutate(across(where(is.numeric), ~ round(., digits = 2))) ))), theme = ttheme("mRedWhite"), rows = c("Kolkata", "Delhi")) %>%
  tab_add_title("Model R2", face = "bold")

del_f6ms_pol <- plot_summs(model_s2, model_n2, model_c2, model_o2, model_p2, model_p.2, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Delhi - Pollutants On Other Pollutants")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) 
kol_f6ms_pol <- plot_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Kolkata - Pollutants On Other Parameters")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

p <- annotate_figure(ggarrange(kol_f6ms_wo_pol, kol_f6ms_pol, del_f6ms_wo_pol, del_f6ms_pol, t_reg_wo_pol, t_reg_pol, ncol = 2, nrow = 3, heights = c(5,5,2)), top = text_grob("First 6 Months", face = "bold", size = 14))


air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  filter(!(Month %in% c(1,2,3,4,5,6))) #%>% # ------------LOOK

header.true <- function(df) {
  colnames(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)

suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))

t_reg_wo_pol <- ggtexttable(as_tibble(header.true(t(prediction.R2$Prediction_Table[1:3] %>%
                                                      mutate(across(where(is.numeric), ~ round(., digits = 2))) ))), theme = ttheme("mVioletWhite"), rows = c("Kolkata", "Delhi")) %>%
  tab_add_title("Model R2", face = "bold")

del_f6ms_wo_pol <- plot_summs(model_s2, model_n2, model_c2, model_o2, model_p2, model_p.2, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Delhi - Pollutants On Weather Parameters")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) 
kol_f6ms_wo_pol <- plot_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Kolkata - Pollutants On Weather Parameters")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

train_all_cities_pol_with_otherpol(air_data_india_All_Specie_Daily_Regress_model)

suppressMessages(prediction.R2 <- print_all_cities_R.squared(air_data_india_All_Specie_Daily_Regress_model))

t_reg_pol <- ggtexttable(as_tibble(header.true(t(prediction.R2$Prediction_Table[1:3] %>%
                                                   mutate(across(where(is.numeric), ~ round(., digits = 2))) ))), theme = ttheme("mRedWhite"), rows = c("Kolkata", "Delhi")) %>%
  tab_add_title("Model R2", face = "bold")

del_f6ms_pol <- plot_summs(model_s2, model_n2, model_c2, model_o2, model_p2, model_p.2, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Delhi - Pollutants On Other Pollutants")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) 
kol_f6ms_pol <- plot_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), ci_level = .99, plot.distributions = TRUE, rescale.distributions = TRUE) +theme(legend.position = "bottom") + ggtitle("Kolkata - Pollutants On Other Parameters")+
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

q <- annotate_figure(ggarrange(kol_f6ms_wo_pol, kol_f6ms_pol, del_f6ms_wo_pol, del_f6ms_pol, t_reg_wo_pol, t_reg_pol, ncol = 2, nrow = 3, heights = c(5,5,2)), top = text_grob("Last 6 Months", face = "bold", size = 14))

annotate_figure(ggarrange(p, q, nrow = 2), top = text_grob("Models-2020", face = "bold", size = 16))


# Added-Variable Plots -------------------------------------!!!!!!!!!!!!!!!!!!ISSSSSSSSSSUUUUUUUUUUUUUUEEEEEEEEEEEEEEEEEE!!!!!!!!!!!!!!!!!!!!------------------
# ---------------------------------------------------MULTICOLLINEARITY ISSUE----------------------------
air_data_india_All_Specie_Daily_Regress_model <- air_data_india_All_Specie_Daily_Regress %>%
  filter(Year == 2020) %>% # ------------LOOOOOOOK
  #filter(Month %in% c(1,2,3,4,5,6)) %>% # ------------LOOK
  filter(Measure == "Max")
train_all_cities_pol_with_nonpol(air_data_india_All_Specie_Daily_Regress_model)
add.var.plt <- added_variable_plot_all_cities(air_data_india_All_Specie_Daily_Regress_model)
add.var.plt$resid.plot.Kolkata
add.var.plt$resid.plot.Delhi
add.var.plt$resid.plot.Muzz
add.var.plt$resid.plot.Mumbai
