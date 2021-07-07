air_data_india_Pollutants_nonpollutants_daily <- air_data_india_All_Specie_Daily_1 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  spread(key = Specie, value = AQI) %>%
  select(-precipitation) %>%
  gather(dew, humidity, pressure, temperature, `wind-gust`, `wind-speed`, key = "non_pollutants", value = Levels) %>%
  gather(co, so2, no2, o3, pm10, pm25, key = "pollutants", value = "AQI")

# Max AQI Compared

air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Max") %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))


# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2019", "QQ-Plot")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Max") %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))


invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Max") %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t1[1]), shape = as.numeric(t1[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t1[1]), shape = as.numeric(t1[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_max <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

# Avg_Median Comapared
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Median") %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Median") %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Median") %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_median <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

# Avg_Min Compared
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Min") %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Min") %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Min") %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t3[1]) + 0.5*as.numeric(t3[2])^2), digits = 2), "Var", round(exp(as.numeric(t3[1])*2 + 0.5*4*as.numeric(t3[2])^2) - (exp(as.numeric(t3[1])*1 + 0.5*as.numeric(t3[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_min <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

ggarrange(p_max, p_median, p_min, nrow = 3)

# First 6 Months
aair_data_india_Pollutants_nonpollutants_daily <- air_data_india_All_Specie_Daily_1 %>%
  filter(Year != 2018) %>% # There is a lot of Missing values in this year
  spread(key = Specie, value = AQI) %>%
  select(-precipitation) %>%
  gather(dew, humidity, pressure, temperature, `wind-gust`, `wind-speed`, key = "non_pollutants", value = Levels) %>%
  gather(co, so2, no2, o3, pm10, pm25, key = "pollutants", value = "AQI")

# Max AQI Compared

air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Max" & Month %in% c(1,2,3,4,5,6)) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))


# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2019", "QQ-Plot")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Max" & Month %in% c(1,2,3,4,5,6)) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))


invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Max" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_max <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

# Avg_Median Comapared
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_median <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

# Avg_Min Compared
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Min" & Month %in% c(1,2,3,4,5,6)) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Min" & Month %in% c(1,2,3,4,5,6)) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Min" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t1[1]), shape = as.numeric(t1[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t1[1]), shape = as.numeric(t1[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t3[1]) + 0.5*as.numeric(t3[2])^2), digits = 2), "Var", round(exp(as.numeric(t3[1])*2 + 0.5*4*as.numeric(t3[2])^2) - (exp(as.numeric(t3[1])*1 + 0.5*as.numeric(t3[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_min <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

ggarrange(p_max, p_median, p_min, nrow = 3)

# -------------------------------------------------------

# Hellinger Fit
invisible(capture.output(p1 <- compare_hellinger_fit_v_MLE(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "plnorm")))

t1 <- tibble("MLE_Fit" = c("meanlog", round(fit_MLE$estimate[1], digits = 2), "sdlog", round(fit_MLE$estimate[2], digits = 2)))

t2 <- tibble("Hel_Fit" = c("meanlog", round(fit_hellinger$estimate[1], digits = 2), "sdlog", round(fit_hellinger$estimate[2], digits = 2)))

table1 <- ggtexttable(tibble(MLE_Fit = c(t1[1,1]$MLE_Fit, t1[2,1]$MLE_Fit, t1[3,1]$MLE_Fit, t1[4,1]$MLE_Fit)), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Hel_Fit = c(t2[1,1]$Hel_Fit, t2[2,1]$Hel_Fit, t2[3,1]$Hel_Fit, t2[4,1]$Hel_Fit)), theme = ttheme("mCyan"), rows = NULL)


table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[2,1]) + 0.5*as.numeric(t1[4,1])^2), digits = 2), "Var", round(exp(as.numeric(t1[2,1])*2 + 0.5*4*as.numeric(t1[4,1])^2) - (exp(as.numeric(t1[2,1])*1 + 0.5*as.numeric(t1[4,1])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[2,1]) + 0.5*as.numeric(t2[4,1])^2), digits = 2), "Var", round(exp(as.numeric(t2[2,1])*2 + 0.5*4*as.numeric(t2[4,1])^2) - (exp(as.numeric(t2[2,1])*1 + 0.5*as.numeric(t2[4,1])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)


table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)

table <- ggarrange(table1, table2, nrow = 2)
ggarrange(p1, table, nrow = 1, ncol = 2, widths = c(10,1))
# ---------------------------------------------------------------

# Median AQI - First 6 Months
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# ------------2021-------------
# First 6 months Only
air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2021_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p3 <- fit_distribution(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2021")))

t3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table3 <- ggtexttable(tibble(Fit = c(colnames(t3)[1], as.character(t3[1]),colnames(t3)[2], as.character(t3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)
table3 <- ggarrange(table3, table3a, nrow = 2)

p_max <- ggarrange(p1, table1, p2, table2, p3, table3, nrow = 3, ncol = 2, widths = c(10,1))

# ------------------------------------------------------------------------
# AQI - Last 6 Month
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Max" & !(Month %in% c(1,2,3,4,5,6))) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Max" & !(Month %in% c(1,2,3,4,5,6))) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Maximum AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))


table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)


table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)


table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)

p_max <- ggarrange(p1, table1, p2, table2, nrow = 2, ncol = 2, widths = c(10,1))

# Median AQI - Last 6 Month
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Median" & !(Month %in% c(1,2,3,4,5,6))) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Median" & !(Month %in% c(1,2,3,4,5,6))) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Median AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))


table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)


table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)


table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)

p_median <- ggarrange(p1, table1, p2, table2, nrow = 2, ncol = 2, widths = c(10,1))


# Min AQI - Last 6 Months
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Min" & !(Month %in% c(1,2,3,4,5,6))) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2019_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

# Normal Data fits well
invisible(capture.output(p1 <- fit_distribution(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2019")))

t1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
# -------------2020 ------------------------
air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Min" & !(Month %in% c(1,2,3,4,5,6))) %>% #   ----------------------------------------LOOOOOOOOOOKKKKKKKK!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm <- air_data_pollutants_2020_avg_median %>%
  filter(!(pollutants %in% c("pm10", "pm25"))) %>%
  ungroup() %>%
  group_by(Year, Month, Day) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

invisible(capture.output(p2 <- fit_distribution(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "Min AQI 2020")))

t2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))


table1 <- ggtexttable(tibble(Fit = c(colnames(t1)[1], as.character(t1[1]),colnames(t1)[2], as.character(t1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table2 <- ggtexttable(tibble(Fit = c(colnames(t2)[1], as.character(t2[1]),colnames(t2)[2], as.character(t2[2])[1])), theme = ttheme("mCyan"), rows = NULL)


table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dnorm, mean = as.numeric(t2[1]), sd = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dnorm, mean = as.numeric(t2[1]), sd = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table1 <- ggarrange(table1, table1a, nrow = 2)
table2 <- ggarrange(table2, table2a, nrow = 2)

p_min <- ggarrange(p1, table1, p2, table2, nrow = 2, ncol = 2, widths = c(10,1))

ggarrange(p_max, p_median, p_min, nrow = 3)

# ------------------------------------------------------------------------------------------

# First 6 Month Pollutant Wise
# First 6 Month
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2021_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2021 & Measure == "Avg_Median" & Month %in% c(1,2,3,4,5,6)) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

# Pollutant-wise
# co

# gamma & lognormal fits really well needs to be compared
invisible(capture.output(p_co_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI, "CO AQI - 2019", "QQ-Plot")))

t_co_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_co_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "co")$AQI, "CO AQI - 2020")))

t_co_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_co_3 <- fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "co")$AQI, "CO AQI - 2021")))

t_co_3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_co_1 <- ggtexttable(tibble(Fit = c(colnames(t_co_1)[1], as.character(t_co_1[1]),colnames(t_co_1)[2], as.character(t_co_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_co_2 <- ggtexttable(tibble(Fit = c(colnames(t_co_2)[1], as.character(t_co_2[1]),colnames(t_co_2)[2], as.character(t_co_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_co_3 <- ggtexttable(tibble(Fit = c(colnames(t_co_3)[1], as.character(t_co_3[1]),colnames(t_co_3)[2], as.character(t_co_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_co_1
t2 <- t_co_2
t3 <- t_co_3

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t3[1]) + 0.5*as.numeric(t3[2])^2), digits = 2), "Var", round(exp(as.numeric(t3[1])*2 + 0.5*4*as.numeric(t3[2])^2) - (exp(as.numeric(t3[1])*1 + 0.5*as.numeric(t3[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_co_1 <- ggarrange(table_co_1, table1a, nrow = 2)
table_co_2 <- ggarrange(table_co_2, table2a, nrow = 2)
table_co_3 <- ggarrange(table_co_3, table3a, nrow = 2)

# no2

# All three fits similar need to compare
invisible(capture.output(p_no2_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "no2")$AQI, "NO2 AQI - 2019", "QQ-Plot")))

t_no2_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_no2_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "no2")$AQI, "NO2 AQI - 2020")))

t_no2_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_no2_3 <- fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "no2")$AQI, "NO2 AQI - 2021")))

t_no2_3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_no2_1 <- ggtexttable(tibble(Fit = c(colnames(t_no2_1)[1], as.character(t_no2_1[1]),colnames(t_no2_1)[2], as.character(t_no2_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_no2_2 <- ggtexttable(tibble(Fit = c(colnames(t_no2_2)[1], as.character(t_no2_2[1]),colnames(t_no2_2)[2], as.character(t_no2_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_no2_3 <- ggtexttable(tibble(Fit = c(colnames(t_no2_3)[1], as.character(t_no2_3[1]),colnames(t_no2_3)[2], as.character(t_no2_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_no2_1
t2 <- t_no2_2
t3 <- t_no2_3

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_no2_1 <- ggarrange(table_no2_1, table1a, nrow = 2)
table_no2_2 <- ggarrange(table_no2_2, table2a, nrow = 2)
table_no2_3 <- ggarrange(table_no2_3, table3a, nrow = 2)

# o3

# None seems to fit well
invisible(capture.output(p_o3_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "O3 AQI - 2019", "QQ-Plot")))

t_o3_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_o3_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "o3")$AQI, "O3 AQI - 2020")))

t_o3_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_o3_3 <- fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "o3")$AQI, "O3 AQI - 2021")))

t_o3_3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_o3_1 <- ggtexttable(tibble(Fit = c(colnames(t_o3_1)[1], as.character(t_o3_1[1]),colnames(t_o3_1)[2], as.character(t_o3_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_o3_2 <- ggtexttable(tibble(Fit = c(colnames(t_o3_2)[1], as.character(t_o3_2[1]),colnames(t_o3_2)[2], as.character(t_o3_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_o3_3 <- ggtexttable(tibble(Fit = c(colnames(t_o3_3)[1], as.character(t_o3_3[1]),colnames(t_o3_3)[2], as.character(t_o3_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_o3_1
t2 <- t_o3_2
t3 <- t_o3_3

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_o3_1 <- ggarrange(table_o3_1, table1a, nrow = 2)
table_o3_2 <- ggarrange(table_o3_2, table2a, nrow = 2)
table_o3_3 <- ggarrange(table_o3_3, table3a, nrow = 2)

# so2

# None fits well
invisible(capture.output(p_so2_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "SO2 AQI - 2019", "QQ-Plot")))

t_so2_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_so2_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "so2")$AQI, "SO2 AQI - 2020")))

t_so2_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_so2_3 <- fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "so2")$AQI, "SO2 AQI - 2021")))

t_so2_3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_so2_1 <- ggtexttable(tibble(Fit = c(colnames(t_so2_1)[1], as.character(t_so2_1[1]),colnames(t_so2_1)[2], as.character(t_so2_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_so2_2 <- ggtexttable(tibble(Fit = c(colnames(t_so2_2)[1], as.character(t_so2_2[1]),colnames(t_so2_2)[2], as.character(t_so2_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_so2_3 <- ggtexttable(tibble(Fit = c(colnames(t_so2_3)[1], as.character(t_so2_3[1]),colnames(t_so2_3)[2], as.character(t_so2_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_so2_1
t2 <- t_so2_2
t3 <- t_so2_3

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t1[1]), shape = as.numeric(t1[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t1[1]), shape = as.numeric(t1[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t3[1]) + 0.5*as.numeric(t3[2])^2), digits = 2), "Var", round(exp(as.numeric(t3[1])*2 + 0.5*4*as.numeric(t3[2])^2) - (exp(as.numeric(t3[1])*1 + 0.5*as.numeric(t3[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_so2_1 <- ggarrange(table_so2_1, table1a, nrow = 2)
table_so2_2 <- ggarrange(table_so2_2, table2a, nrow = 2)
table_so2_3 <- ggarrange(table_so2_3, table3a, nrow = 2)

# pm10

# None well
invisible(capture.output(p_pm10_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "PM 10 AQI - 2019", "QQ-Plot")))

t_pm10_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_pm10_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm10")$AQI, "PM 10 AQI - 2020")))

t_pm10_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_pm10_3 <- fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "pm10")$AQI, "PM 10 AQI - 2021")))

t_pm10_3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_pm10_1 <- ggtexttable(tibble(Fit = c(colnames(t_pm10_1)[1], as.character(t_pm10_1[1]),colnames(t_pm10_1)[2], as.character(t_pm10_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_pm10_2 <- ggtexttable(tibble(Fit = c(colnames(t_pm10_2)[1], as.character(t_pm10_2[1]),colnames(t_pm10_2)[2], as.character(t_pm10_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_pm10_3 <- ggtexttable(tibble(Fit = c(colnames(t_pm10_3)[1], as.character(t_pm10_3[1]),colnames(t_pm10_3)[2], as.character(t_pm10_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_pm10_1
t2 <- t_pm10_2
t3 <- t_pm10_3

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t3[1]) + 0.5*as.numeric(t3[2])^2), digits = 2), "Var", round(exp(as.numeric(t3[1])*2 + 0.5*4*as.numeric(t3[2])^2) - (exp(as.numeric(t3[1])*1 + 0.5*as.numeric(t3[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_pm10_1 <- ggarrange(table_pm10_1, table1a, nrow = 2)
table_pm10_2 <- ggarrange(table_pm10_2, table2a, nrow = 2)
table_pm10_3 <- ggarrange(table_pm10_3, table3a, nrow = 2)

# pm25

# Normal and weibull fits well needs to be compared
invisible(capture.output(p_pm25_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "PM 25 AQI - 2019", "QQ-Plot")))

t_pm25_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_pm25_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm25")$AQI, "PM 25 AQI - 2020")))

t_pm25_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_pm25_3 <- fit_distribution(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "PM 25 AQI - 2021")))

t_pm25_3 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_pm25_1 <- ggtexttable(tibble(Fit = c(colnames(t_pm25_1)[1], as.character(t_pm25_1[1]),colnames(t_pm25_1)[2], as.character(t_pm25_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_pm25_2 <- ggtexttable(tibble(Fit = c(colnames(t_pm25_2)[1], as.character(t_pm25_2[1]),colnames(t_pm25_2)[2], as.character(t_pm25_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_pm25_3 <- ggtexttable(tibble(Fit = c(colnames(t_pm25_3)[1], as.character(t_pm25_3[1]),colnames(t_pm25_3)[2], as.character(t_pm25_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_pm25_1
t2 <- t_pm25_2
t3 <- t_pm25_3

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table3a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t3[1]), shape = as.numeric(t3[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_pm25_1 <- ggarrange(table_pm25_1, table1a, nrow = 2)
table_pm25_2 <- ggarrange(table_pm25_2, table2a, nrow = 2)
table_pm25_3 <- ggarrange(table_pm25_3, table3a, nrow = 2)

p1 <- ggarrange(p_co_1, table_co_1, p_co_2, table_co_2, p_co_3, table_co_3, nrow = 3, ncol = 2, widths = c(10,1))
p2 <- ggarrange(p_no2_1, table_no2_1, p_no2_2, table_no2_2, p_no2_3, table_no2_3, nrow = 3, ncol = 2, widths = c(10,1))
p3 <- ggarrange(p_o3_1, table_o3_1, p_o3_2, table_o3_2, p_o3_3, table_o3_3, nrow = 3, ncol = 2, widths = c(10,1))
p4 <- ggarrange(p_so2_1, table_so2_1, p_so2_2, table_so2_2, p_so2_3, table_so2_3, nrow = 3, ncol = 2, widths = c(10,1))
p5 <- ggarrange(p_pm10_1, table_pm10_1, p_pm10_2, table_pm10_2, p_pm10_3, table_pm10_3, nrow = 3, ncol = 2, widths = c(10,1))
p6 <- ggarrange(p_pm25_1, table_pm25_1, p_pm25_2, table_pm25_2, p_pm25_3, table_pm25_3, nrow = 3, ncol = 2, widths = c(10,1))

ggarrange(p1, p2, p3, p4, p5, p6, nrow = 6)

# --------------------

# Last 6 Month Pollutant Wise
# Last 6 Month
air_data_pollutants_2019_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2019 & Measure == "Avg_Median" & !(Month %in% c(1,2,3,4,5,6))) %>% #  -----------------------------------------------LOOOOK!!!!!!!!!!
  select(-c("non_pollutants", "Levels"))

air_data_pollutants_2020_avg_median <- air_data_india_Pollutants_nonpollutants_daily %>%
  filter(Year == 2020 & Measure == "Avg_Median" & !(Month %in% c(1,2,3,4,5,6))) %>% # ------------------------------LOOOOOOOOOOOOOOOOOOK!!!!!!!!
  select(-c("non_pollutants", "Levels"))

# Pollutant-wise
# co

# gamma & lognormal fits really well needs to be compared
invisible(capture.output(p_co_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI, "CO AQI - 2019", "QQ-Plot")))

t_co_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_co_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "co")$AQI, "CO AQI - 2020")))

t_co_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_co_1 <- ggtexttable(tibble(Fit = c(colnames(t_co_1)[1], as.character(t_co_1[1]),colnames(t_co_1)[2], as.character(t_co_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_co_2 <- ggtexttable(tibble(Fit = c(colnames(t_co_2)[1], as.character(t_co_2[1]),colnames(t_co_2)[2], as.character(t_co_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_co_1
t2 <- t_co_2

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_co_1 <- ggarrange(table_co_1, table1a, nrow = 2)
table_co_2 <- ggarrange(table_co_2, table2a, nrow = 2)

# no2

# All three fits similar need to compare
invisible(capture.output(p_no2_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "no2")$AQI, "NO2 AQI - 2019", "QQ-Plot")))

t_no2_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_no2_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "no2")$AQI, "NO2 AQI - 2020")))

t_no2_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_no2_1 <- ggtexttable(tibble(Fit = c(colnames(t_no2_1)[1], as.character(t_no2_1[1]),colnames(t_no2_1)[2], as.character(t_no2_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_no2_2 <- ggtexttable(tibble(Fit = c(colnames(t_no2_2)[1], as.character(t_no2_2[1]),colnames(t_no2_2)[2], as.character(t_no2_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_no2_3 <- ggtexttable(tibble(Fit = c(colnames(t_no2_3)[1], as.character(t_no2_3[1]),colnames(t_no2_3)[2], as.character(t_no2_3[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_no2_1
t2 <- t_no2_2

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_no2_1 <- ggarrange(table_no2_1, table1a, nrow = 2)
table_no2_2 <- ggarrange(table_no2_2, table2a, nrow = 2)

# o3

# None seems to fit well
invisible(capture.output(p_o3_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "O3 AQI - 2019", "QQ-Plot")))

t_o3_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_o3_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "o3")$AQI, "O3 AQI - 2020")))

t_o3_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_o3_1 <- ggtexttable(tibble(Fit = c(colnames(t_o3_1)[1], as.character(t_o3_1[1]),colnames(t_o3_1)[2], as.character(t_o3_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_o3_2 <- ggtexttable(tibble(Fit = c(colnames(t_o3_2)[1], as.character(t_o3_2[1]),colnames(t_o3_2)[2], as.character(t_o3_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_o3_1
t2 <- t_o3_2

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_o3_1 <- ggarrange(table_o3_1, table1a, nrow = 2)
table_o3_2 <- ggarrange(table_o3_2, table2a, nrow = 2)

# so2

# None fits well
invisible(capture.output(p_so2_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "SO2 AQI - 2019", "QQ-Plot")))

t_so2_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_so2_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "so2")$AQI, "SO2 AQI - 2020")))

t_so2_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_so2_1 <- ggtexttable(tibble(Fit = c(colnames(t_so2_1)[1], as.character(t_so2_1[1]),colnames(t_so2_1)[2], as.character(t_so2_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_so2_2 <- ggtexttable(tibble(Fit = c(colnames(t_so2_2)[1], as.character(t_so2_2[1]),colnames(t_so2_2)[2], as.character(t_so2_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_so2_1
t2 <- t_so2_2

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_so2_1 <- ggarrange(table_so2_1, table1a, nrow = 2)
table_so2_2 <- ggarrange(table_so2_2, table2a, nrow = 2)

# pm10

# None well
invisible(capture.output(p_pm10_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "PM 10 AQI - 2019", "QQ-Plot")))

t_pm10_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_pm10_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm10")$AQI, "PM 10 AQI - 2020")))

t_pm10_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_pm10_1 <- ggtexttable(tibble(Fit = c(colnames(t_pm10_1)[1], as.character(t_pm10_1[1]),colnames(t_pm10_1)[2], as.character(t_pm10_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_pm10_2 <- ggtexttable(tibble(Fit = c(colnames(t_pm10_2)[1], as.character(t_pm10_2[1]),colnames(t_pm10_2)[2], as.character(t_pm10_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_pm10_1
t2 <- t_pm10_2

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(t2[1]), shape = as.numeric(t2[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_pm10_1 <- ggarrange(table_pm10_1, table1a, nrow = 2)
table_pm10_2 <- ggarrange(table_pm10_2, table2a, nrow = 2)

# pm25

# Normal and weibull fits well needs to be compared
invisible(capture.output(p_pm25_1 <- fit_distribution(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "PM 25 AQI - 2019", "QQ-Plot")))

t_pm25_1 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

invisible(capture.output(p_pm25_2 <- fit_distribution(filter(air_data_pollutants_2020_avg_median, pollutants == "pm25")$AQI, "PM 25 AQI - 2020")))

t_pm25_2 <- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
  spread(key = names, value = x) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

table_pm25_1 <- ggtexttable(tibble(Fit = c(colnames(t_pm25_1)[1], as.character(t_pm25_1[1]),colnames(t_pm25_1)[2], as.character(t_pm25_1[2])[1])), theme = ttheme("mCyan"), rows = NULL)
table_pm25_2 <- ggtexttable(tibble(Fit = c(colnames(t_pm25_2)[1], as.character(t_pm25_2[1]),colnames(t_pm25_2)[2], as.character(t_pm25_2[2])[1])), theme = ttheme("mCyan"), rows = NULL)

t1 <- t_pm25_1
t2 <- t_pm25_2

table1a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t1[1]) + 0.5*as.numeric(t1[2])^2), digits = 2), "Var", round(exp(as.numeric(t1[1])*2 + 0.5*4*as.numeric(t1[2])^2) - (exp(as.numeric(t1[1])*1 + 0.5*as.numeric(t1[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
table2a <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(t2[1]) + 0.5*as.numeric(t2[2])^2), digits = 2), "Var", round(exp(as.numeric(t2[1])*2 + 0.5*4*as.numeric(t2[2])^2) - (exp(as.numeric(t2[1])*1 + 0.5*as.numeric(t2[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)

table_pm25_1 <- ggarrange(table_pm25_1, table1a, nrow = 2)
table_pm25_2 <- ggarrange(table_pm25_2, table2a, nrow = 2)

p1 <- ggarrange(p_co_1, table_co_1, p_co_2, table_co_2, nrow = 2, ncol = 2, widths = c(10,1))
p2 <- ggarrange(p_no2_1, table_no2_1, p_no2_2, table_no2_2, nrow = 2, ncol = 2, widths = c(10,1))
p3 <- ggarrange(p_o3_1, table_o3_1, p_o3_2, table_o3_2, nrow = 2, ncol = 2, widths = c(10,1))
p4 <- ggarrange(p_so2_1, table_so2_1, p_so2_2, table_so2_2, nrow = 2, ncol = 2, widths = c(10,1))
p5 <- ggarrange(p_pm10_1, table_pm10_1, p_pm10_2, table_pm10_2, nrow = 2, ncol = 2, widths = c(10,1))
p6 <- ggarrange(p_pm25_1, table_pm25_1, p_pm25_2, table_pm25_2, nrow = 2, ncol = 2, widths = c(10,1))

ggarrange(p1, p2, p3, p4, p5, p6, nrow = 6)
