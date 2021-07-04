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
library(fitdistrplus)
library(pander)
library(stargazer)
library(forcats)

# Apart from Delhi are the counts of other stations same ?
print(obsv_per_station, n = 50)

obsv_per_station %>%
  gather(`2021`, `2020`, key = "Year", value = Observations) %>%
  select(City, Year, Observations) %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Observations, fill = City)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year) +
  coord_flip() +
  labs(
    x = "Number of Observations Recorded"
  ) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 1,
      override.aes = list(size = 3)
    )
  ) +
  scale_color_viridis(discrete = TRUE)

test <- obsv_per_station %>%
  ungroup() %>%
  filter(City != "Delhi" & City %in% avg_median_per_year_per_station$City[1:11]) %>% # Since it is clearly different
  select(`2020`) %>%
  chisq.test()
test
print_chisq_prop(test)

test <- obsv_per_station %>%
  ungroup() %>%
  filter(City != "Delhi" & City %in% avg_median_per_year_per_station$City[1:11]) %>% # Since it is clearly different
  select(`2021`) %>%
  chisq.test()
test
print_chisq_prop(test)

# Testing Equality of Mean AQI of different City Stations

Citywise_pollutant_AQI <- air_data_india_pollutants %>%
  group_by(Year, Month, City, Day) %>%
  summarise(Avg_Min = mean(min, na.rm = TRUE), Avg_Median = mean(median, na.rm = TRUE), Avg_Max = mean(max, na.rm = TRUE))
Citywise_pollutant_AQI

Citywise_pollutant_AQI_2020 <- Citywise_pollutant_AQI %>%
  filter(Year == 2020)
Citywise_pollutant_AQI_2021 <- Citywise_pollutant_AQI %>%
  filter(Year == 2021)
Citywise_pollutant_AQI_2019 <- Citywise_pollutant_AQI %>%
  filter(Year == 2019)


Citywise_pollutant_AQI_2019 %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Avg_Median)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = mean(Citywise_pollutant_AQI_2019$Avg_Median))

Citywise_pollutant_AQI_2020 %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Avg_Median)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = mean(Citywise_pollutant_AQI_2020$Avg_Median))

test <- aov(Avg_Median ~ City, filter(Citywise_pollutant_AQI_2020, City %in% avg_median_per_year_per_station$City[1:11] & City != "Delhi"))
summary(test)

Citywise_pollutant_AQI_2021 %>%
  filter(City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(City, Avg_Median)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = mean(Citywise_pollutant_AQI_2021$Avg_Median))

air_data_all_specie_yearly_city_included <- air_data_india %>%
  filter(!(Year %in% c(2014, 2015, 2016, 2017, 2018))) %>%
  group_by(Year, City, Specie) %>%
  summarise(Avg_Min = mean(min, na.rm = TRUE), Avg_Median = mean(median, na.rm = TRUE), Avg_Max = mean(max, na.rm = TRUE)) %>%
  gather(Avg_Min, Avg_Median, Avg_Max, key = "Measure", value = "AQI")
air_data_all_specie_yearly_city_included

air_data_pollutants_yearly_city_included <- air_data_all_specie_yearly_city_included %>%
  filter(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25"))
air_data_pollutants_yearly_city_included

air_data_nonpollutants_yearly_city_included <- air_data_all_specie_yearly_city_included %>%
  filter(!(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25")))
air_data_nonpollutants_yearly_city_included

plt1 <- air_data_pollutants_yearly_city_included %>%
  filter(Year == 2019 & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(AQI, City, fill = City)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  coord_cartesian(xlim = c(0, 200)) +
  scale_y_discrete(labels = NULL) +
  geom_vline(
    xintercept = mean(filter(air_data_pollutants_yearly_city_included, Year == 2019 & City %in% avg_median_per_year_per_station$City[1:11])$AQI), size = 1
  ) +
  theme(legend.position = "None") +
  scale_fill_viridis(discrete = TRUE)
plt1

test3 <- aov(AQI ~ City, filter(air_data_pollutants_yearly_city_included, City %in% avg_median_per_year_per_station$City[1:11] & Year == 2019))
test3 <- summary(test3)
test3 <- unclass(test3)
test3 <- cbind("Source" = rownames(test3[[1]]), test3[[1]])
test3 <- as_tibble(test3)
table1 <- ggtexttable(test3, theme = ttheme("mBlue")) %>%
  tab_add_footnote(text = "One-Factor ANOVA for 2019", size = 10, face = "italic")
table1

plt2 <- air_data_pollutants_yearly_city_included %>%
  filter(Year == 2020 & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(AQI, City, fill = City)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  coord_cartesian(xlim = c(0, 200)) +
  scale_y_discrete(labels = NULL) +
  geom_vline(
    xintercept = mean(filter(air_data_pollutants_yearly_city_included, Year == 2020 & City %in% avg_median_per_year_per_station$City[1:11])$AQI), size = 1
  ) +
  theme(legend.position = "None") +
  scale_color_viridis(discrete = TRUE)
plt2

test4 <- aov(AQI ~ City, filter(air_data_pollutants_yearly_city_included, City %in% avg_median_per_year_per_station$City[1:11] & Year == 2020))
test4 <- summary(test4)
test4 <- unclass(test4)
test4 <- cbind("Source" = rownames(test4[[1]]), test4[[1]])
test4 <- as_tibble(test4)
table2 <- ggtexttable(test4, theme = ttheme("mBlue"))%>%
  tab_add_footnote(text = "One-Factor ANOVA for 2020", size = 10, face = "italic")
table2

plt3 <- air_data_pollutants_yearly_city_included %>%
  filter(Year == 2021 & City %in% avg_median_per_year_per_station$City[1:11]) %>%
  ggplot(aes(AQI, City, fill = City)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  coord_cartesian(xlim = c(0, 200)) +
  scale_y_discrete(labels = NULL) +
  geom_vline(
    xintercept = mean(filter(air_data_pollutants_yearly_city_included, Year == 2021 & City %in% avg_median_per_year_per_station$City[1:11])$AQI), size = 1
  ) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) +
  scale_color_viridis(discrete = TRUE)
plt3

test5 <- aov(AQI ~ City, filter(air_data_pollutants_yearly_city_included, City %in% avg_median_per_year_per_station$City[1:11] & Year == 2021))
test5 <- summary(test5)
test5 <- unclass(test5)
test5 <- cbind("Source" = rownames(test5[[1]]), test5[[1]])
test5 <- as_tibble(test5)
table3 <- ggtexttable(test5, theme = ttheme("mBlue")) %>%
  tab_add_footnote(text = "One-Factor ANOVA for 2021", size = 10, face = "italic")
table3

table <- ggarrange(table1, table2, table3, nrow = 3)
table

ggarrange(plt1, plt2, plt3, table)

# Testing for Equality of Mean of the Year 2020 with past 2 Years and 2021 with 2020 and past 2 Years
air_data_india_Pollutants_Daily <- air_data_india_All_Specie_Daily_1 %>%
  group_by(Year, Month, Day) %>%
  filter(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25")) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))
air_data_india_Pollutants_Daily

plt1 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  coord_cartesian(ylim = c(20, 100)) +
  labs(y = "Mean AQI") +
  geom_hline(
    yintercept = mean(filter(air_data_india_Pollutants_Daily, Year %in% c(2019, 2020))$Mean_AQI, na.rm = TRUE), size = 1
  )+
  theme(legend.position = "None")
plt1

#null_distribution1_sim <- air_data_india_Pollutants_Daily %>%
#  filter(Year %in% c(2019, 2020)) %>%
#  mutate(Year = as.character(Year)) %>%
#  specify(Mean_AQI ~ Year) %>%
#  hypothesise("independence") %>%
#  generate(reps = 1000, type = "permute") %>%
#  calculate(stat = "diff in means", order = c("2020", "2019"))
#null_distribution1_sim

null_distribution1_t <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  hypothesise("independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("2020", "2019"))
null_distribution1_t

obs_diff_means1 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  calculate(stat = "t", order = c("2020", "2019"))
obs_diff_means1

sim_test_statistic1 <- visualise(null_distribution1_t, method = "both") +
  shade_p_value(obs_stat = obs_diff_means1, direction = "left")
sim_test_statistic1

null_distribution1_t %>%
  get_p_value(obs_stat = obs_diff_means1, direction = "left")


test1 <- t.test(filter(air_data_india_Pollutants_Daily, Year == 2020)$Mean_AQI, filter(air_data_india_Pollutants_Daily, Year == 2019)$Mean_AQI,
                alternative = "less", var.equal = TRUE, conf.level = 0.99)
test1

table1 <- ggtexttable(map_df(list(test1), tidy), theme = ttheme("mOrange")) %>%
  tab_add_footnote(text = "two-sample t-test for 2019 & 2020", size = 10, face = "italic")
table1  

plt2 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2020, 2021) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  coord_cartesian(ylim = c(20, 100)) +
  labs(y = "Mean AQI") +
  geom_hline(
    yintercept = mean(filter(air_data_india_Pollutants_Daily, Year %in% c(2020, 2021) & Month %in% c(1,2,3,4,5,6) )$Mean_AQI, na.rm = TRUE), size = 1
  )+
  theme(legend.position = "None")
plt2

null_distribution2_t <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2021, 2020) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  hypothesise("independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("2021", "2020"))
null_distribution2_t

obs_diff_means2 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2021, 2020) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  calculate(stat = "t", order = c("2021", "2020"))
obs_diff_means2

sim_test_statistic2 <- visualise(null_distribution2_t, method = "both") +
  shade_p_value(obs_stat = obs_diff_means2, direction = "right")
sim_test_statistic2

null_distribution2_t %>%
  get_p_value(obs_stat = obs_diff_means2, direction = "right")


test2 <- t.test(filter(air_data_india_Pollutants_Daily, Year == 2021 & Month %in% c(1,2,3,4,5,6))$Mean_AQI, filter(air_data_india_Pollutants_Daily, Year == 2020 & Month %in% c(1,2,3,4,5,6))$Mean_AQI,
                alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test2

table2 <- ggtexttable(map_df(list(test2), tidy), theme = ttheme("mOrange")) %>%
  tab_add_footnote(text = "two-sample t-test for 2021 & 2021", size = 10, face = "italic")
table2

ggarrange(plt1, plt2, sim_test_statistic1, sim_test_statistic2, table1, table2, ncol = 2, nrow = 3, heights = c(3, 3, 1))

# We will explore some details of the above comparison
plt3 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020, 2021) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  coord_cartesian(ylim = c(20, 100)) +
  labs(x = NULL) +
  theme(legend.position = "None") 
plt3

null_distribution1_t <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  hypothesise("independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("2020", "2019"))
null_distribution1_t

obs_diff_means1 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  calculate(stat = "t", order = c("2020", "2019"))
obs_diff_means1

sim_test_statistic1 <- visualise(null_distribution1_t, method = "both") +
  shade_p_value(obs_stat = obs_diff_means1, direction = "left")
sim_test_statistic1

null_distribution1_t %>%
  get_p_value(obs_stat = obs_diff_means1, direction = "left")


test1 <- t.test(filter(air_data_india_Pollutants_Daily, Year == 2020 & Month %in% c(1,2,3,4,5,6))$Mean_AQI, filter(air_data_india_Pollutants_Daily, Year == 2019 & Month %in% c(1,2,3,4,5,6))$Mean_AQI,
                alternative = "less", var.equal = TRUE, conf.level = 0.99)
test1

table1 <- ggtexttable(map_df(list(test1), tidy), theme = ttheme("mOrange")) %>%
  tab_add_footnote(text = "two-sample t-test for 2019 & 2020", size = 10, face = "italic")
table1 

null_distribution2_t <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2021, 2019) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  hypothesise("independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("2021", "2019"))
null_distribution2_t

obs_diff_means2 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2021, 2019) & Month %in% c(1,2,3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  calculate(stat = "t", order = c("2021", "2019"))
obs_diff_means2

sim_test_statistic2 <- visualise(null_distribution2_t, method = "both") +
  shade_p_value(obs_stat = obs_diff_means2, direction = "both")
sim_test_statistic2

null_distribution2_t %>%
  get_p_value(obs_stat = obs_diff_means2, direction = "both")


test2 <- t.test(filter(air_data_india_Pollutants_Daily, Year == 2021 & Month %in% c(1,2,3,4,5,6))$Mean_AQI, filter(air_data_india_Pollutants_Daily, Year == 2019 & Month %in% c(1,2,3,4,5,6))$Mean_AQI,
                alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test2

table2 <- ggtexttable(map_df(list(test2), tidy), theme = ttheme("mOrange")) %>%
  tab_add_footnote(text = "two-sample t-test for 2021 & 2019", size = 10, face = "italic")
table2

p1 <- ggarrange(plt3, ncol = 1)
p2 <- ggarrange(sim_test_statistic1, sim_test_statistic2, ncol = 2)
p3 <- ggarrange(table1, table2, nrow = 2) 
ggarrange(p1, p2, p3, nrow = 3, heights = c(4, 3, 2))



plt4 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020) & Month %in% c(3,4,5,6)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  coord_cartesian(ylim = c(20, 100)) +
  scale_x_discrete(labels = NULL) +
  geom_hline(
    yintercept = mean(filter(air_data_india_Pollutants_Daily, Year %in% c(2019, 2020) & Month %in% c(3,4,5,6) )$Mean_AQI, na.rm = TRUE), size = 1
  )+
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) +
  scale_color_viridis(discrete = TRUE)
plt4


plt5 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020) & Month %in% c(7,8,9,10, 11, 12)) %>%
  mutate(Year = as.character(Year)) %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot() +
  stat_summary(fun = "mean") +
  coord_cartesian(ylim = c(20, 100)) +
  geom_hline(
    yintercept = mean(filter(air_data_india_Pollutants_Daily, Year %in% c(2019, 2020) & Month %in% c(7,8,9,10, 11, 12))$Mean_AQI, na.rm = TRUE), size = 1
  )+
  labs(x = NULL, y = "Mean AQI") +
  theme(legend.position = "None") 
plt5

null_distribution1_t <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020) & Month %in% c(7,8,9,10, 11, 12)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  hypothesise("independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("2020", "2019"))
null_distribution1_t

obs_diff_means1 <- air_data_india_Pollutants_Daily %>%
  filter(Year %in% c(2019, 2020) & Month %in% c(7,8,9,10, 11, 12)) %>%
  mutate(Year = as.character(Year)) %>%
  specify(Mean_AQI ~ Year) %>%
  calculate(stat = "t", order = c("2020", "2019"))
obs_diff_means1

sim_test_statistic1 <- visualise(null_distribution1_t, method = "both") +
  shade_p_value(obs_stat = obs_diff_means1, direction = "right")
sim_test_statistic1

null_distribution1_t %>%
  get_p_value(obs_stat = obs_diff_means1, direction = "right")


test1 <- t.test(filter(air_data_india_Pollutants_Daily, Year == 2020 & Month %in% c(7,8,9,10, 11, 12))$Mean_AQI, filter(air_data_india_Pollutants_Daily, Year == 2019 & Month %in% c(7,8,9,10, 11, 12))$Mean_AQI,
                alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test1

table1 <- ggtexttable(map_df(list(test1), tidy), theme = ttheme("mOrange")) %>%
  tab_add_footnote(text = "two-sample t-test for 2019 & 2020", size = 10, face = "italic")
table1 

p1 <- ggarrange(plt5, sim_test_statistic1, ncol = 2)
p3 <- ggarrange(table1, nrow = 1) 
ggarrange(p1, p3, nrow = 2, heights = c(6, 1))


# So now we will provide Pollutant-wise Inference

# First 6 Months
air_data_india_first6month_pollutants_daily <- air_data_india_All_Specie_Daily_1 %>%
  group_by(Year, Month, Day, Specie) %>%
  filter(Year != 2018) %>%
  mutate(Year = as.character(Year)) %>%
  filter(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25")) %>%
  filter(Month %in% c(1,2,3,4,5,6)) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

plt1 <- air_data_india_first6month_pollutants_daily %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  scale_x_discrete(labels = NULL) +
  facet_wrap(~Specie, scales = "free") +
  scale_fill_brewer(palette = "Dark2")
plt1

plt2 <- air_data_india_first6month_pollutants_daily %>%
  filter(Specie %in% c("co", "no2", "o3")) %>%
  ggplot(aes(Year, Mean_AQI, fill = Specie)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  labs(y = "Mean AQI") +
  facet_wrap(~Specie, scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), legend.position = c(0.5, 0.025)) +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )
plt2


test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "co")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "co")$Mean_AQI,
                alternative = "less", var.equal = TRUE, conf.level = 0.99)
test_1

test_2 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "co")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "co")$Mean_AQI,
                   alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test_2

test_3 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "co")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "co")$Mean_AQI,
                   alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test_3

table.fit <- full_join(full_join(select(map_df(list(test_1), tidy), -c(parameter, method, statistic)), select(map_df(list(test_2), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative")), select(map_df(list(test_3), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative"))

table1 <- ggtexttable(table.fit, theme = ttheme("mGreen"))
table1

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "no2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "no2")$Mean_AQI,
                 alternative = "less", var.equal = TRUE, conf.level = 0.99)
test_1

test_2 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "no2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "no2")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test_2

test_3 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "no2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "no2")$Mean_AQI,
                 alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test_3

table.fit <- full_join(full_join(select(map_df(list(test_1), tidy), -c(parameter, method, statistic)), select(map_df(list(test_2), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative")), select(map_df(list(test_3), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative"))

table2 <- ggtexttable(table.fit, theme = ttheme("mGreen"))
table2

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "o3")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "o3")$Mean_AQI,
                 alternative = "less", var.equal = TRUE, conf.level = 0.99)
test_1

test_2 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "o3")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "o3")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test_2

test_3 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "o3")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "o3")$Mean_AQI,
                 alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test_3

table.fit <- full_join(full_join(select(map_df(list(test_1), tidy), -c(parameter, method, statistic)), select(map_df(list(test_2), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative")), select(map_df(list(test_3), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative"))

table3 <- ggtexttable(table.fit, theme = ttheme("mGreen"))
table3

table <- ggarrange(table1, table2, table3, ncol = 3)
p1 <- ggarrange(plt2, table, nrow = 2, heights = c(6, 3))
p1


plt3 <- air_data_india_first6month_pollutants_daily %>%
  filter(Specie %in% c("so2", "pm10", "pm25")) %>%
  ggplot(aes(Year, Mean_AQI, fill = Specie)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  labs(y = "Mean AQI") +
  facet_wrap(~Specie, scales = "free") +
  scale_fill_brewer(palette = "RdBu") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), legend.position = c(0.5, 0.025)) +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )
plt3


test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "pm10")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "pm10")$Mean_AQI,
                 alternative = "less", var.equal = TRUE, conf.level = 0.99)
test_1

test_2 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "pm10")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "pm10")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test_2

test_3 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "pm10")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "pm10")$Mean_AQI,
                 alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test_3


table.fit <- full_join(full_join(select(map_df(list(test_1), tidy), -c(parameter, method, statistic)), select(map_df(list(test_2), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative")), select(map_df(list(test_3), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative"))

table1 <- ggtexttable(table.fit, theme = ttheme("mGreen"))
table1

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "pm25")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "pm25")$Mean_AQI,
                 alternative = "less", var.equal = TRUE, conf.level = 0.99)
test_1


test_2 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "pm25")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "pm25")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test_2


test_3 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "pm25")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "pm25")$Mean_AQI,
                 alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test_3

table.fit <- full_join(full_join(select(map_df(list(test_1), tidy), -c(parameter, method, statistic)), select(map_df(list(test_2), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative")), select(map_df(list(test_3), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative"))

table2 <- ggtexttable(table.fit, theme = ttheme("mGreen"))
table2

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "so2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "so2")$Mean_AQI,
                 alternative = "less", var.equal = TRUE, conf.level = 0.99)
test_1

test_2 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "so2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "so2")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)
test_2

test_3 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2021 & Specie == "so2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "so2")$Mean_AQI,
                 alternative = "two.sided", var.equal = TRUE, conf.level = 0.99)
test_3

table.fit <- full_join(full_join(select(map_df(list(test_1), tidy), -c(parameter, method, statistic)), select(map_df(list(test_2), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative")), select(map_df(list(test_3), tidy), -c(parameter, method, statistic)), by = c("estimate", "estimate1", "estimate2", "p.value", "conf.low", "conf.high", "alternative"))

table3 <- ggtexttable(table.fit, theme = ttheme("mGreen"))
table3

table <- ggarrange(table1, table2, table3, ncol = 3)
p2 <- ggarrange(plt3, table, nrow = 2, heights = c(6, 3))
p2

ggarrange(p1, p2, nrow = 2)

# Last 6 Months
air_data_india_All_Specie_Daily_1 %>%
  group_by(Year, Month, Day, Specie) %>%
  filter(Year != 2018) %>%
  mutate(Year = as.character(Year)) %>%
  filter(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25")) %>%
  filter(Month %in% c(7,8,9,10,11,12)) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE)) %>%
  ggplot(aes(Year, Mean_AQI, fill = Year)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  facet_wrap(~Specie, scales = "free")

air_data_india_first6month_pollutants_daily <- air_data_india_All_Specie_Daily_1 %>% # The name of this variable should be last6month but I was too reluctant to change this code
  group_by(Year, Month, Day, Specie) %>%
  filter(Year != 2018) %>%
  mutate(Year = as.character(Year)) %>%
  filter(Specie %in% c("co", "no2", "o3", "so2", "pm10", "pm25")) %>%
  filter(Month %in% c(7, 8, 9, 10, 11, 12)) %>%
  summarise(Mean_AQI = mean(AQI, na.rm = TRUE))

label <- tibble(label = "Row-1: t-test 2020 & 2019\nRow-2: t-test 2021 & 2020\nRow-1: t-test 2019 & 2021")

plt2 <- air_data_india_first6month_pollutants_daily %>%
  filter(Specie %in% c("co", "no2", "o3")) %>%
  ggplot(aes(Year, Mean_AQI, fill = Specie)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  labs(y = NULL, x = NULL) +
  facet_wrap(~Specie, scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), legend.position = c(0.5, 0.025), 
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "co")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "co")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)

table1 <- ggtexttable(select(map_df(list(test_1), tidy), -c(parameter, method, statistic, estimate1, estimate2))%>% 
                        mutate(across(where(is.numeric), ~ round(., digits = 3))), theme = ttheme("mGreen"), rows = NULL)

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "no2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "no2")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)

table2 <- ggtexttable(select(map_df(list(test_1), tidy), -c(parameter, method, statistic, estimate1, estimate2))%>% 
                        mutate(across(where(is.numeric), ~ round(., digits = 3))), theme = ttheme("mGreen"), rows = NULL)

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "o3")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "o3")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)

table3 <- ggtexttable(select(map_df(list(test_1), tidy), -c(parameter, method, statistic, estimate1, estimate2))%>% 
                        mutate(across(where(is.numeric), ~ round(., digits = 3))), theme = ttheme("mGreen"), rows = NULL)

table <- ggarrange(table1, table2, table3, ncol = 3)
p1 <- ggarrange(plt2, table, nrow = 2, heights = c(6, 3))

plt3 <- air_data_india_first6month_pollutants_daily %>%
  filter(Specie %in% c("so2", "pm10", "pm25")) %>%
  ggplot(aes(Year, Mean_AQI, fill = Specie)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean") +
  labs(y = NULL, x = NULL) +
  facet_wrap(~Specie, scales = "free") +
  scale_fill_brewer(palette = "RdBu") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), legend.position = c(0.5, 0.025),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  )

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "pm10")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "pm10")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)

table1 <- ggtexttable(select(map_df(list(test_1), tidy), -c(parameter, method, statistic, estimate1, estimate2))%>% 
                        mutate(across(where(is.numeric), ~ round(., digits = 3))), theme = ttheme("mGreen"), rows = NULL)

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "pm25")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "pm25")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)

table2 <- ggtexttable(select(map_df(list(test_1), tidy), -c(parameter, method, statistic, estimate1, estimate2))%>% 
                        mutate(across(where(is.numeric), ~ round(., digits = 3))), theme = ttheme("mGreen"), rows = NULL)

test_1 <- t.test(filter(air_data_india_first6month_pollutants_daily, Year == 2020 & Specie == "so2")$Mean_AQI, filter(air_data_india_first6month_pollutants_daily, Year == 2019 & Specie == "so2")$Mean_AQI,
                 alternative = "greater", var.equal = TRUE, conf.level = 0.99)

table3 <- ggtexttable(select(map_df(list(test_1), tidy), -c(parameter, method, statistic, estimate1, estimate2))%>% 
                        mutate(across(where(is.numeric), ~ round(., digits = 3))), theme = ttheme("mGreen"), rows = NULL)

table <- ggarrange(table1, table2, table3, ncol = 3)
p2 <- ggarrange(plt3, table, nrow = 2, heights = c(6, 3))

ggarrange(p1, p2, nrow = 2)