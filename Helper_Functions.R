# Function to print chi-sq test for proportionality in nice format
print_chisq_prop <- function(test){
  table <- tibble(
    `Test Statistic` = test$statistic,
    df = test$parameter,
    `p-value` = test$p.value,
  )
  return(list("test_table" = table))
}

print_aov_onefactor <- function(test){
  test <- summary(test)
  test <- unclass(test)
  test <- cbind("Source" = rownames(test[[1]]), test[[1]])
  table <- as_tibble(test)
  return(list("test_table" = table))
}

fit_distribution <- function(data, title_name = "Model & Data", qqtitle = NULL){
  fit_g <<- invisible(fitdistrplus::fitdist(data, "gamma"))
  fit_l <<- invisible(fitdistrplus::fitdist(data, "lnorm"))
  fit_w <<- invisible(fitdistrplus::fitdist(data, "weibull"))
  fit_n <<- invisible(fitdistrplus::fitdist(data, "norm"))
  fit_e <<- invisible(fitdistrplus::fitdist(data, "exp"))
  
  plot.legend <- c("gamma", "lognormal", "weibull", "normal", "exponential")
  gof <<- fitdistrplus::gofstat(list(fit_g, fit_l, fit_w, fit_n, fit_e), fitnames = plot.legend)
  print(gof)
  print(gof$chisqpvalue)
  
  p1 <- fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend, fitlwd = c(2,2,2,2,2), xlegend = 0.007, plotstyle = "ggplot") #, fit_e), legendtext = plot.legend, fitlwd = c(3,3.5,4,4.5,1), xlegend = 0.007)
  #fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend)
  p2 <- fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend, plotstyle = "ggplot") # , fit_e), legendtext = plot.legend, xlegend = 0.01, )
  #fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend)
  
  p1 <- p1 +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ggtitle(title_name) + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  p2 <- p2  +
    theme(legend.position = "None") + ggtitle(qqtitle) + labs(subtitle = paste(rownames(as.data.frame(gof$ks[which.min(gof$ks)])), "seems to fit the data better.")) + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  
  #print(paste(rownames(as.data.frame(gof$ks[which.min(gof$ks)])), "seems to fit the data well"))
  print(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))])
  #distfit.table <- tibble("Distribution" = c("Gamma", "Log-Norm", "Weibull", "Norm", "Expo"), "value" = gof$chisqpvalue) %>%
  #  spread(key = Distribution, value = value) %>%
  #  mutate(across(where(is.numeric), ~ round(., digits = 3)))
  #return(distfit.table)
  p <- ggarrange(p1, p2, ncol = 2)
  print(p)
  return(p)
}

proper_fit_distribution <- function(data, heading = "Model & Data"){
  
  invisible(capture.output(p <- fit_distribution(data, heading)))
  
  tf <<- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
    spread(key = names, value = x) %>% 
    mutate(across(where(is.numeric), ~ round(., digits = 2)))
  
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "exp")
    table <- ggtexttable(tibble(Fit = c(colnames(tf)[1], as.character(tf[1]))), theme = ttheme("mCyan"), rows = NULL)
  else
    table <- ggtexttable(tibble(Fit = c(colnames(tf)[1], as.character(tf[1]),colnames(tf)[2], as.character(tf[2])[1])), theme = ttheme("mCyan"), rows = NULL)
  
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "weibull")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "lnorm")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(tf[1]) + 0.5*as.numeric(tf[2])^2), digits = 2), "Var", round(exp(as.numeric(tf[1])*2 + 0.5*4*as.numeric(tf[2])^2) - (exp(as.numeric(tf[1])*1 + 0.5*as.numeric(tf[2])^2))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "gamma")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "norm")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dnorm, mean = as.numeric(tf[1]), sd = as.numeric(tf[2]))$value, digits = 2), "Var", round(find_variance(dnorm, mean = as.numeric(tf[1]), sd = as.numeric(tf[2]))$value, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "exp")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(1/as.numeric(tf[1]), digits = 2), "Var", round(1/(as.numeric(tf[1]))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  
  table <- ggarrange(table, tablea, nrow = 2)
  
  p <- ggarrange(p, table, nrow = 1, ncol = 2, widths = c(10,1))
  return(p)
}

# Fit Distribution Curve to All Cities
fit_distribution_all_cities <- function(data){
  # So2 Level prediction
  p1 <- proper_fit_distribution((data %>% filter(City == "Kolkata"))$so2, "Kolkata")
  p2 <- proper_fit_distribution((data %>% filter(City == "Delhi"))$so2, "Delhi")
  p3 <- proper_fit_distribution((data %>% filter(City == "Muzaffarnagar"))$so2, "Muzaffarnagar")
  p4 <- proper_fit_distribution((data %>% filter(City == "Mumbai"))$so2, "Mumbai")
  p5 <- proper_fit_distribution((data %>% filter(City == "Lucknow"))$so2, "Lucknow")
  p6 <- proper_fit_distribution((data %>% filter(City == "Patna"))$so2, "Patna")
  p7 <- proper_fit_distribution((data %>% filter(City == "Chandigarh"))$so2, "Chandigarh")
  p8 <- proper_fit_distribution((data %>% filter(City == "Gandhinagar"))$so2, "Gandhinagar")
  p9 <- proper_fit_distribution((data %>% filter(City == "Jaipur"))$so2, "Jaipur")
  
  fit_so2 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # No2 Level prediction
  p1 <- proper_fit_distribution((data %>% filter(City == "Kolkata"))$no2, "Kolkata")
  p2 <- proper_fit_distribution((data %>% filter(City == "Delhi"))$no2, "Delhi")
  p3 <- proper_fit_distribution((data %>% filter(City == "Muzaffarnagar"))$no2, "Muzaffarnagar")
  p4 <- proper_fit_distribution((data %>% filter(City == "Mumbai"))$no2, "Mumbai")
  p5 <- proper_fit_distribution((data %>% filter(City == "Lucknow"))$no2, "Lucknow")
  p6 <- proper_fit_distribution((data %>% filter(City == "Patna"))$no2, "Patna")
  p7 <- proper_fit_distribution((data %>% filter(City == "Chandigarh"))$no2, "Chandigarh")
  p8 <- proper_fit_distribution((data %>% filter(City == "Gandhinagar"))$no2, "Gandhinagar")
  p9 <- proper_fit_distribution((data %>% filter(City == "Jaipur"))$no2, "Jaipur")
  
  fit_no2 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # CO Level prediction
  p1 <- proper_fit_distribution((data %>% filter(City == "Kolkata"))$co, "Kolkata")
  p2 <- proper_fit_distribution((data %>% filter(City == "Delhi"))$co, "Delhi")
  p3 <- proper_fit_distribution((data %>% filter(City == "Muzaffarnagar"))$co, "Muzaffarnagar")
  p4 <- proper_fit_distribution((data %>% filter(City == "Mumbai"))$co, "Mumbai")
  p5 <- proper_fit_distribution((data %>% filter(City == "Lucknow"))$co, "Lucknow")
  p6 <- proper_fit_distribution((data %>% filter(City == "Patna"))$co, "Patna")
  p7 <- proper_fit_distribution((data %>% filter(City == "Chandigarh"))$co, "Chandigarh")
  p8 <- proper_fit_distribution((data %>% filter(City == "Gandhinagar"))$co, "Gandhinagar")
  p9 <- proper_fit_distribution((data %>% filter(City == "Jaipur"))$co, "Jaipur")
  
  fit_co <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # O3 Level prediction
  p1 <- proper_fit_distribution((data %>% filter(City == "Kolkata"))$o3, "Kolkata")
  p2 <- proper_fit_distribution((data %>% filter(City == "Delhi"))$o3, "Delhi")
  p3 <- proper_fit_distribution((data %>% filter(City == "Muzaffarnagar"))$o3, "Muzaffarnagar")
  p4 <- proper_fit_distribution((data %>% filter(City == "Mumbai"))$o3, "Mumbai")
  p5 <- proper_fit_distribution((data %>% filter(City == "Lucknow"))$o3, "Lucknow")
  p6 <- proper_fit_distribution((data %>% filter(City == "Patna"))$o3, "Patna")
  p7 <- proper_fit_distribution((data %>% filter(City == "Chandigarh"))$o3, "Chandigarh")
  p8 <- proper_fit_distribution((data %>% filter(City == "Gandhinagar"))$o3, "Gandhinagar")
  p9 <- proper_fit_distribution((data %>% filter(City == "Jaipur"))$o3, "Jaipur")
  
  fit_o3 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # pm10 Level prediction
  p1 <- proper_fit_distribution((data %>% filter(City == "Kolkata"))$pm10, "Kolkata")
  p2 <- proper_fit_distribution((data %>% filter(City == "Delhi"))$pm10, "Delhi")
  p3 <- proper_fit_distribution((data %>% filter(City == "Muzaffarnagar"))$pm10, "Muzaffarnagar")
  p4 <- proper_fit_distribution((data %>% filter(City == "Mumbai"))$pm10, "Mumbai")
  #p5 <- proper_fit_distribution((data %>% filter(City == "Lucknow"))$pm10, "Lucknow")
  #p6 <- proper_fit_distribution((data %>% filter(City == "Patna"))$pm10, "Patna")
  p7 <- proper_fit_distribution((data %>% filter(City == "Chandigarh"))$pm10, "Chandigarh")
  p8 <- proper_fit_distribution((data %>% filter(City == "Gandhinagar"))$pm10, "Gandhinagar")
  p9 <- proper_fit_distribution((data %>% filter(City == "Jaipur"))$pm10, "Jaipur")
  
  fit_pm10 <- ggarrange(p1,p2,p3,p4,p7,p8,p9, nrow = 3, ncol = 3)
  
  # pm25 Level prediction
  p1 <- proper_fit_distribution((data %>% filter(City == "Kolkata"))$pm25, "Kolkata")
  p2 <- proper_fit_distribution((data %>% filter(City == "Delhi"))$pm25, "Delhi")
  p3 <- proper_fit_distribution((data %>% filter(City == "Muzaffarnagar"))$pm25, "Muzaffarnagar")
  p4 <- proper_fit_distribution((data %>% filter(City == "Mumbai"))$pm25, "Mumbai")
  p5 <- proper_fit_distribution((data %>% filter(City == "Lucknow"))$pm25, "Lucknow")
  p6 <- proper_fit_distribution((data %>% filter(City == "Patna"))$pm25, "Patna")
  p7 <- proper_fit_distribution((data %>% filter(City == "Chandigarh"))$pm25, "Chandigarh")
  p8 <- proper_fit_distribution((data %>% filter(City == "Gandhinagar"))$pm25, "Gandhinagar")
  p9 <- proper_fit_distribution((data %>% filter(City == "Jaipur"))$pm25, "Jaipur")
  
  fit_pm25 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  return(list("fit_so2"=fit_so2, "fit_no2"=fit_no2, "fit_co"=fit_co, "fit_o3"=fit_o3, "fit_pm10"=fit_pm10, "fit_pm25"=fit_pm25))
}

# Was trying to implement my own ggplot printing but apparently it's already there in qqcomp 
present_model_fit <- function(data, smoothness = 0.1){
  data1 <<- tibble("x" = data)
  fit_distribution(data)
  
  x <<- seq(min(data), max(data), by = smoothness)
  
  val <<- as.list(c(NA, as.numeric(fit_g$estimate)))
  val[[1]] <<- x
  
  g <<- do.call("dgamma", val)
  
  val <<- as.list(c(NA, as.numeric(fit_l$estimate)))
  val[[1]] <<- x
  
  l <- do.call("dlnorm", val)
  
  val <<- as.list(c(NA, as.numeric(fit_w$estimate)))
  val[[1]] <<- x
  
  w <- do.call("dweibull", val)
  
  val <<- as.list(c(NA, as.numeric(fit_n$estimate)))
  val[[1]] <<- x
  
  n <- do.call("dnorm", val)
  
  line_fit <<- tibble("x" = x, "g" = g, "l" = l, "w" = w, "n" = n)
  
  p1 <- data1 %>%
    ggplot(aes(x, ..density..)) +
    geom_histogram() +
    geom_line(data = line_fit, aes(x, g), size = 2, color = "red") +
    geom_line(data = line_fit, aes(x, l), size = 2, color = "green") +
    geom_line(data = line_fit, aes(x, w), size = 2, color = "blue") +
    geom_line(data = line_fit, aes(x, n), size = 2, color = "pink") +
    theme(legend.position = "None")
  
  ggplot(data1, aes(sample = x)) + 
    stat_qq(distribution = qgamma, dparams = as.list(fit_g$estimate), color = "red") +
    stat_qq(distribution = qlnorm, dparams = as.list(fit_l$estimate), color = "green") +
    stat_qq(distribution = qweibull, dparams = as.list(fit_w$estimate), color = "blue") +
    stat_qq(distribution = qnorm, dparams = as.list(fit_n$estimate), color = "pink")
  
}

# Usage
present_model_fit(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI)


# Making Predictions and checking distribution of residuals
make_predictions <- function(trained_model, test_data){
  predictions <- test_data %>%
    add_predictions(trained_model) %>%
    add_residuals(trained_model)
  p <- predictions %>%
    ggplot(aes(resid, ..density..)) +
    geom_histogram() + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black"),strip.background = element_blank())
  return(p)
}

# Training for 1 city
train_city_pol <- function(data, city = "City"){
  # so2 on non-pollutants
  model_s1 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data)
  
  # no2 on non-pollutants
  model_n1 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data)
  
  # co on non-pollutants
  model_c1 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data)
  
  # o3 on non-pollutants
  model_o1 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data)
  
  # pm10 on non-pollutants
  model_p1 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data)
  
  # pm25 on non-pollutants
  model_p.1 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data)
  
  R2.so2 <- tibble(
    city = find_R.squared(model_s1, data)
  )
  R2.no2 <- tibble(
    city = find_R.squared(model_n1, data)
  )
  R2.co <- tibble(
    city = find_R.squared(model_c1, data)
  )
  R2.o3 <- tibble(
    city = find_R.squared(model_o1, data)
  )
  R2.pm10 <- tibble(
    city = find_R.squared(model_p1, data)
  )
  R2.pm25 <- tibble(
    city = find_R.squared(model_p.1, data)
  )
  
  suppressMessages(Prediction_Table <- full_join(full_join(full_join(full_join(full_join(R2.so2, R2.no2), R2.co), R2.o3), R2.pm10), R2.pm25))
  Prediction_Table.nonpol <- Prediction_Table %>%
    mutate(Pollutant = c("so2", "no2", "co", "o3", "pm10", "pm25")) %>%
    select(Pollutant, everything())
  
  model_summary.nonpol <- export_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, error_format = "[{conf.low}, {conf.high}]", model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), number_format = "%.2f")
  
 
  
  return(list("prediction.table.nonpol"=Prediction_Table.nonpol, "nonpolsummary"=model_summary.nonpol))
}

train_city_pol_w_pol <- function(data, city = "City"){
  # so2 on pollutants
  model_s1 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data)
  
  # no2 on pollutants
  model_n1 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data)
  
  
  # co on pollutants
  model_c1 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data)
  
  
  # o3 on pollutants
  model_o1 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data)
  
  # pm10 on pollutants
  model_p1 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data)
  
  # pm25 on pollutants
  model_p.1 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data)
  
  R2.so2 <- tibble(
    city = find_R.squared(model_s1, data)
  )
  R2.no2 <- tibble(
    city = find_R.squared(model_n1, data)
  )
  R2.co <- tibble(
    city = find_R.squared(model_c1, data)
  )
  R2.o3 <- tibble(
    city = find_R.squared(model_o1, data)
  )
  R2.pm10 <- tibble(
    city = find_R.squared(model_p1, data)
  )
  R2.pm25 <- tibble(
    city = find_R.squared(model_p.1, data)
  )
  
  suppressMessages(Prediction_Table <- full_join(full_join(full_join(full_join(full_join(R2.so2, R2.no2), R2.co), R2.o3), R2.pm10), R2.pm25))
  Prediction_Table.otherpol <- Prediction_Table %>%
    mutate(Pollutant = c("so2", "no2", "co", "o3", "pm10", "pm25")) %>%
    select(Pollutant, everything())
  
  model_summary.otherpol <- export_summs(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1, error_format = "[{conf.low}, {conf.high}]", model.names = c("SO2", "NO2", "CO", "O3", "PM10", "PM25"), number_format = "%.2f")
  
  return(list("prediction.table.otherpol"=Prediction_Table.otherpol, "otherpolsummary"=model_summary.otherpol))
}

# Training top 9 cities
train_all_cities_pol_with_nonpol <- function(data){
  # so2 on non-pollutants
  model_s1 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # no2 on non-pollutants
  model_n1 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # co on non-pollutants
  model_c1 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # o3 on non-pollutants
  model_o1 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_o2 <<- lm(o3~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # pm10 on non-pollutants
  model_p1 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # pm25 on non-pollutants
  model_p.1 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
}

train_all_cities_pol_with_otherpol <- function(data){
  # so2 on pollutants
  model_s1 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  
  # no2 on pollutants
  model_n1 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  
  
  # co on pollutants
  model_c1 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  
  
  # o3 on pollutants
  model_o1 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_o2 <<- lm(o3~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  
  # pm10 on pollutants
  model_p1 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Mumbai"))

  # pm25 on non-pollutants
  model_p.1 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Mumbai"))
  
}

full_train_all_cities_pol <- function(data){
  # so2 on non-pollutants
  model_s1 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # no2 on non-pollutants
  model_n1 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # co on non-pollutants
  model_c1 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # o3 on non-pollutants
  model_o1 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_o2 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))

  # pm10 on non-pollutants
  model_p1 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  
  # pm25 on non-pollutants
  model_p.1 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10 + dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))

}

# Training top 9 cities
train_all_cities_pol_with_nonpol_wl <- function(data){
  # so2 on non-pollutants
  model_s1 <<- lm(log(so2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(log(so2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_s5 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_s6 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_s7 <<- lm(log(so2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_s8 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_s9 <<- lm(log(so2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
  summary(model_s1)
  summary(model_s2)
  summary(model_s3)
  summary(model_s4)
  summary(model_s5)
  summary(model_s6)
  summary(model_s7)
  summary(model_s8)
  summary(model_s9)
  
  # no2 on non-pollutants
  model_n1 <<- lm(log(no2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(log(no2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(log(no2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_n5 <<- lm(log(no2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_n6 <<- lm(log(no2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_n7 <<- lm(log(no2) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_n8 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_n9 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
  summary(model_n1)
  summary(model_n2)
  summary(model_n3)
  summary(model_n4)
  summary(model_n5)
  summary(model_n6)
  summary(model_n7)
  summary(model_n8)
  summary(model_n9)
  
  # co on non-pollutants
  model_c1 <<- lm(log(co) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(log(co) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(log(co) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(log(co) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_c5 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_c6 <<- lm(log(co) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_c7 <<- lm(log(co) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_c8 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_c9 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
  summary(model_c1)
  summary(model_c2)
  summary(model_c3)
  summary(model_c4)
  summary(model_c5)
  summary(model_c6)
  summary(model_c7)
  summary(model_c8)
  summary(model_c9)
  
  # o3 on non-pollutants
  model_o1 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_o2 <<- lm(log(o3)~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(log(o3) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_o5 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_o6 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_o7 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_o8 <<- lm(log(o3) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_o9 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
  summary(model_o1)
  summary(model_o2)
  summary(model_o3)
  summary(model_o4)
  summary(model_o5)
  summary(model_o6)
  summary(model_o7)
  summary(model_o8)
  summary(model_o9)
  
  # pm10 on non-pollutants
  model_p1 <<- lm(log(pm10) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(log(pm10) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(log(pm10) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(log(pm10) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  #model_p5 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  #model_p6 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_p7 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_p8 <<- lm(log(pm10) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_p9 <<- lm(log(pm10) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
  summary(model_p1)
  summary(model_p2)
  summary(model_p3)
  summary(model_p4)
  #summary(model_p5)
  #summary(model_p6)
  summary(model_p7)
  summary(model_p8)
  summary(model_p9)
  
  # pm25 on non-pollutants
  model_p.1 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_p.5 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_p.6 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_p.7 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_p.8 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_p.9 <<- lm(log(pm25) ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
  summary(model_p.1)
  summary(model_p.2)
  summary(model_p.3)
  summary(model_p.4)
  summary(model_p.5)
  summary(model_p.6)
  summary(model_p.7)
  summary(model_p.8)
  invisible(summary(model_p.9))
}

train_all_cities_pol_with_otherpol_wl <- function(data){
  # so2 on pollutants
  model_s1 <<- lm(log(so2) ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(log(so2) ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_s5 <<- lm(so2 ~ no2 + co + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  model_s6 <<- lm(so2 ~ no2 + co + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_s7 <<- lm(log(so2) ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_s8 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_s9 <<- lm(log(so2) ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
  summary(model_s1)
  summary(model_s2)
  summary(model_s3)
  summary(model_s4)
  summary(model_s5)
  summary(model_s6)
  summary(model_s7)
  summary(model_s8)
  summary(model_s9)
  
  # no2 on pollutants
  model_n1 <<- lm(no2 ~ log(so2) + co + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(no2 ~ log(so2) + co + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_n5 <<- lm(no2 ~ so2 + co + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  model_n6 <<- lm(no2 ~ so2 + co + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_n7 <<- lm(no2 ~ log(so2) + co + o3 + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_n8 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_n9 <<- lm(no2 ~ log(so2) + co + o3 + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
  summary(model_n1)
  summary(model_n2)
  summary(model_n3)
  summary(model_n4)
  summary(model_n5)
  summary(model_n6)
  summary(model_n7)
  summary(model_n8)
  summary(model_n9)
  
  # co on pollutants
  model_c1 <<- lm(co ~ log(so2) + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(co ~ log(so2) + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_c5 <<- lm(co ~ so2 + no2 + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  model_c6 <<- lm(co ~ so2 + no2 + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_c7 <<- lm(co ~ log(so2) + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_c8 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_c9 <<- lm(co ~ log(so2) + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
  summary(model_c1)
  summary(model_c2)
  summary(model_c3)
  summary(model_c4)
  summary(model_c5)
  summary(model_c6)
  summary(model_c7)
  summary(model_c8)
  summary(model_c9)
  
  # o3 on pollutants
  model_o1 <<- lm(o3 ~ log(so2) + no2 + co + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_o2 <<- lm(o3~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ log(so2) + no2 + co + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_o5 <<- lm(o3 ~ so2 + no2 + co + pm25, data = data %>% filter(City == "Lucknow"))
  model_o6 <<- lm(o3 ~ so2 + no2 + co + pm25, data = data %>% filter(City == "Patna"))
  model_o7 <<- lm(o3 ~ log(so2) + no2 + co + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_o8 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_o9 <<- lm(o3 ~ log(so2) + no2 + co + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
  summary(model_o1)
  summary(model_o2)
  summary(model_o3)
  summary(model_o4)
  summary(model_o5)
  summary(model_o6)
  summary(model_o7)
  summary(model_o8)
  summary(model_o9)
  
  # pm10 on pollutants
  model_p1 <<- lm(pm10 ~ log(so2) + no2 + co + o3 + pm25, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(pm10 ~ log(so2) + no2 + co + o3 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Mumbai"))
  #model_p5 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  #model_p6 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_p7 <<- lm(pm10 ~ log(so2) + no2 + co + o3 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_p8 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_p9 <<- lm(pm10 ~ log(so2) + no2 + co + o3 + pm25, data = data %>% filter(City == "Jaipur"))
  
  summary(model_p1)
  summary(model_p2)
  summary(model_p3)
  summary(model_p4)
  #summary(model_p5)
  #summary(model_p6)
  summary(model_p7)
  summary(model_p8)
  summary(model_p9)
  
  # pm25 on non-pollutants
  model_p.1 <<- lm(pm25 ~ log(so2) + no2 + co + o3 + pm10, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(pm25 ~ log(so2) + no2 + co + o3 + pm10, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Mumbai"))
  model_p.5 <<- lm(pm25 ~ so2 + no2 + co + o3, data = data %>% filter(City == "Lucknow"))
  model_p.6 <<- lm(pm25 ~ so2 + no2 + co + o3, data = data %>% filter(City == "Patna"))
  model_p.7 <<- lm(pm25 ~ log(so2) + no2 + co + o3 + pm10, data = data %>% filter(City == "Chandigarh"))
  model_p.8 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Gandhinagar"))
  model_p.9 <<- lm(pm25 ~ log(so2) + no2 + co + o3 + pm10, data = data %>% filter(City == "Jaipur"))
  
  summary(model_p.1)
  summary(model_p.2)
  summary(model_p.3)
  summary(model_p.4)
  summary(model_p.5)
  summary(model_p.6)
  summary(model_p.7)
  summary(model_p.8)
  invisible(summary(model_p.9))
}

# Print all City Model Summaries
print_all_city_model_summary <- function(){

  model_so2 <- export_summs(model_s1, model_s2, model_s3, model_s4, error_format = "[{conf.low}, {conf.high}]", model.names = c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai"))
  model_no2 <- export_summs(model_n1, model_n2, model_n3, model_n4, error_format = "[{conf.low}, {conf.high}]", model.names = c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai"))
  model_co <- export_summs(model_c1, model_c2, model_c3, model_c4, error_format = "[{conf.low}, {conf.high}]", model.names = c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai"))
  model_o3 <- export_summs(model_o1, model_o2, model_o3, model_o4, error_format = "[{conf.low}, {conf.high}]", model.names = c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai"))
  model_pm10 <- export_summs(model_p1, model_p2, model_p3, model_p4, error_format = "[{conf.low}, {conf.high}]", model.names = c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai"))
  model_pm25 <- export_summs(model_p.1, model_p.2, model_p.3, model_p.4, error_format = "[{conf.low}, {conf.high}]", model.names = c("Kolkata", "Delhi", "Muzaffarnagar", "Mumbai"))

  return(list("model_so2"=model_so2, "model_no2"=model_no2, "model_co"=model_co, "model_o3"=model_o3, "model_pm10"=model_pm10, "model_pm25"=model_pm25))
}

# Predicting top 9 cities pollutants from non pol trained data
predict_all_cities_pol <- function(data){
  # Non-pollutants based predictions --- MAKE SURE THE model_s1 and all model used in this part is trained with non-pollutants
  
  # So2 Level prediction
  p1 <- make_predictions(model_s1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_s2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  #p3 <- make_predictions(model_s3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  #p4 <- make_predictions(model_s4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  
  res_so2 <- annotate_figure(ggarrange(p1,p2, nrow = 1, ncol = 2), top = text_grob("SO2 Residuals", face = "bold", size = 16))
  
  # No2 Level prediction
  p1 <- make_predictions(model_n1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_n2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  #p3 <- make_predictions(model_n3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  #p4 <- make_predictions(model_n4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  
  res_no2 <- annotate_figure(ggarrange(p1,p2, nrow = 1, ncol = 2), top = text_grob("NO2 Residuals", face = "bold", size = 16))
  
  # CO Level prediction
  p1 <- make_predictions(model_c1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_c2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  #p3 <- make_predictions(model_c3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  #p4 <- make_predictions(model_c4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  
  res_co <- annotate_figure(ggarrange(p1,p2, nrow = 1, ncol = 2), top = text_grob("CO Residuals", face = "bold", size = 16))
  
  # O3 Level prediction
  p1 <- make_predictions(model_o1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_o2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  #p3 <- make_predictions(model_o3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  #p4 <- make_predictions(model_o4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  
  res_o3 <- annotate_figure(ggarrange(p1,p2, nrow = 1, ncol = 2), top = text_grob("O3 Residuals", face = "bold", size = 16))
  
  # pm10 Level prediction
  p1 <- make_predictions(model_p1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_p2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  #p3 <- make_predictions(model_p3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  #p4 <- make_predictions(model_p4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  
  res_pm10 <- annotate_figure(ggarrange(p1,p2, nrow = 1, ncol = 2), top = text_grob("PM10 Residuals", face = "bold", size = 16))
  
  # pm25 Level prediction
  p1 <- make_predictions(model_p.1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_p.2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  #p3 <- make_predictions(model_p.3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  #p4 <- make_predictions(model_p.4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  
  res_pm25 <- annotate_figure(ggarrange(p1,p2, nrow = 1, ncol = 2), top = text_grob("PM25 Residuals", face = "bold", size = 16))
  
  return(annotate_figure(ggarrange(res_so2, res_no2, res_co, res_o3, res_pm10, res_pm25), top = text_grob("Residuals", face = "bold", size = 18)))
}

# Test Data R^2
find_R.squared <- function(model, data){
  predictions <- data %>%
    add_predictions(model)
  predictions <- predictions %>%
    filter(!is.na(pred))
  y <- as.data.frame(predictions[,model$terms[[2]]])[,1]
  R.squared <- cor(predictions$pred, y)^2
  return(R.squared)
}

# Test Data R^2 Print
print_all_cities_R.squared <- function(data){
  R2.so2 <- tibble(
    "Kolkata" = find_R.squared(model_s1, data %>% filter(City == "Kolkata")),
    "Delhi" = find_R.squared(model_s2, data %>% filter(City == "Delhi")),
    "Muzaffarnagar" = find_R.squared(model_s3, data %>% filter(City == "Muzaffarnagar")),
    "Mumbai" = find_R.squared(model_s4, data %>% filter(City == "Mumbai")),
  )
  R2.no2 <- tibble(
    "Kolkata" = find_R.squared(model_n1, data %>% filter(City == "Kolkata")),
    "Delhi" = find_R.squared(model_n2, data %>% filter(City == "Delhi")),
    "Muzaffarnagar" = find_R.squared(model_n3, data %>% filter(City == "Muzaffarnagar")),
    "Mumbai" = find_R.squared(model_n4, data %>% filter(City == "Mumbai")),
  )
  R2.co <- tibble(
    "Kolkata" = find_R.squared(model_c1, data %>% filter(City == "Kolkata")),
    "Delhi" = find_R.squared(model_c2, data %>% filter(City == "Delhi")),
    "Muzaffarnagar" = find_R.squared(model_c3, data %>% filter(City == "Muzaffarnagar")),
    "Mumbai" = find_R.squared(model_c4, data %>% filter(City == "Mumbai")),
  )
  R2.o3 <- tibble(
    "Kolkata" = find_R.squared(model_o1, data %>% filter(City == "Kolkata")),
    "Delhi" = find_R.squared(model_o2, data %>% filter(City == "Delhi")),
    "Muzaffarnagar" = find_R.squared(model_o3, data %>% filter(City == "Muzaffarnagar")),
    "Mumbai" = find_R.squared(model_o4, data %>% filter(City == "Mumbai")),
  )
  R2.pm10 <- tibble(
    "Kolkata" = find_R.squared(model_p1, data %>% filter(City == "Kolkata")),
    "Delhi" = find_R.squared(model_p2, data %>% filter(City == "Delhi")),
    "Muzaffarnagar" = find_R.squared(model_p3, data %>% filter(City == "Muzaffarnagar")),
    "Mumbai" = find_R.squared(model_p4, data %>% filter(City == "Mumbai")),
  )
  R2.pm25 <- tibble(
    "Kolkata" = find_R.squared(model_p.1, data %>% filter(City == "Kolkata")),
    "Delhi" = find_R.squared(model_p.2, data %>% filter(City == "Delhi")),
    "Muzaffarnagar" = find_R.squared(model_p.3, data %>% filter(City == "Muzaffarnagar")),
    "Mumbai" = find_R.squared(model_p.4, data %>% filter(City == "Mumbai")),
  )
  
  Prediction_Table <- full_join(full_join(full_join(full_join(full_join(R2.so2, R2.no2), R2.co), R2.o3), R2.pm10), R2.pm25)
  Prediction_Table <- Prediction_Table %>%
    mutate(Pollutant = c("so2", "no2", "co", "o3", "pm10", "pm25")) %>%
    select(Pollutant, everything())
  return(list("R2.so2" = R2.so2, "R2.no2" = R2.no2, "R2.co" = R2.co, "R2.o3" = R2.o3, "R2.pm10" = R2.pm10, "R2.pm25" = R2.pm25, "Prediction_Table" = Prediction_Table))
}

# Added Variable Plot
added_variable_plot <- function(data, model_1, model_2){
  residuals <- data %>%
    add_residuals(model_1, var = "resid_1") %>%
    add_residuals(model_2, var = "resid_2")
  
  p1 <- residuals %>%
    ggplot(aes(resid_1, resid_2)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y~x) +
    labs(
      x = paste(model_1$terms[[2]], "resid"),
      y = paste(model_2$terms[[2]], "resid")
    ) + 
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black"),strip.background = element_blank())
  return(p1)
}

# Added Variable Plots All Cities
added_variable_plot_all_cities <- function(data){
  p1 <- added_variable_plot(data, model_s1, model_n1)
  p2 <- added_variable_plot(data, model_s1, model_c1)
  p3 <- added_variable_plot(data, model_s1, model_o1)
  p4 <- added_variable_plot(data, model_s1, model_p1)
  p5 <- added_variable_plot(data, model_s1, model_p.1)
  r1 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_n1, model_s1)
  p2 <- added_variable_plot(data, model_n1, model_c1)
  p3 <- added_variable_plot(data, model_n1, model_o1)
  p4 <- added_variable_plot(data, model_n1, model_p1)
  p5 <- added_variable_plot(data, model_n1, model_p.1)
  r2 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_c1, model_s1)
  p2 <- added_variable_plot(data, model_c1, model_n1)
  p3 <- added_variable_plot(data, model_c1, model_o1)
  p4 <- added_variable_plot(data, model_c1, model_p1)
  p5 <- added_variable_plot(data, model_c1, model_p.1)
  r3 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_o1, model_s1)
  p2 <- added_variable_plot(data, model_o1, model_c1)
  p3 <- added_variable_plot(data, model_o1, model_n1)
  p4 <- added_variable_plot(data, model_o1, model_p1)
  p5 <- added_variable_plot(data, model_o1, model_p.1)
  r4 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p1, model_s1)
  p2 <- added_variable_plot(data, model_p1, model_c1)
  p3 <- added_variable_plot(data, model_p1, model_n1)
  p4 <- added_variable_plot(data, model_p1, model_o1)
  p5 <- added_variable_plot(data, model_p1, model_p.1)
  r5 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p.1, model_s1)
  p2 <- added_variable_plot(data, model_p.1, model_c1)
  p3 <- added_variable_plot(data, model_p.1, model_n1)
  p4 <- added_variable_plot(data, model_p.1, model_o1)
  p5 <- added_variable_plot(data, model_p.1, model_p1)
  r6 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  resid.plot.Kolkata <- ggarrange(r1,r2,r3,r4,r5,r6,nrow = 6) 
  
  p1 <- added_variable_plot(data, model_s2, model_n2)
  p2 <- added_variable_plot(data, model_s2, model_c2)
  p3 <- added_variable_plot(data, model_s2, model_o2)
  p4 <- added_variable_plot(data, model_s2, model_p2)
  p5 <- added_variable_plot(data, model_s2, model_p.2)
  r1 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_n2, model_s2)
  p2 <- added_variable_plot(data, model_n2, model_c2)
  p3 <- added_variable_plot(data, model_n2, model_o2)
  p4 <- added_variable_plot(data, model_n2, model_p2)
  p5 <- added_variable_plot(data, model_n2, model_p.2)
  r2 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_c2, model_s2)
  p2 <- added_variable_plot(data, model_c2, model_n2)
  p3 <- added_variable_plot(data, model_c2, model_o2)
  p4 <- added_variable_plot(data, model_c2, model_p2)
  p5 <- added_variable_plot(data, model_c2, model_p.2)
  r3 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_o2, model_s2)
  p2 <- added_variable_plot(data, model_o2, model_c2)
  p3 <- added_variable_plot(data, model_o2, model_n2)
  p4 <- added_variable_plot(data, model_o2, model_p2)
  p5 <- added_variable_plot(data, model_o2, model_p.2)
  r4 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p2, model_s2)
  p2 <- added_variable_plot(data, model_p2, model_c2)
  p3 <- added_variable_plot(data, model_p2, model_n2)
  p4 <- added_variable_plot(data, model_p2, model_o2)
  p5 <- added_variable_plot(data, model_p2, model_p.2)
  r5 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p.2, model_s2)
  p2 <- added_variable_plot(data, model_p.2, model_c2)
  p3 <- added_variable_plot(data, model_p.2, model_n2)
  p4 <- added_variable_plot(data, model_p.2, model_o2)
  p5 <- added_variable_plot(data, model_p.2, model_p2)
  r6 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  resid.plot.Delhi <- ggarrange(r1,r2,r3,r4,r5,r6,nrow = 6) 
  
  p1 <- added_variable_plot(data, model_s3, model_n3)
  p2 <- added_variable_plot(data, model_s3, model_c3)
  p3 <- added_variable_plot(data, model_s3, model_o3)
  p4 <- added_variable_plot(data, model_s3, model_p3)
  p5 <- added_variable_plot(data, model_s3, model_p.3)
  r1 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_n3, model_s3)
  p2 <- added_variable_plot(data, model_n3, model_c3)
  p3 <- added_variable_plot(data, model_n3, model_o3)
  p4 <- added_variable_plot(data, model_n3, model_p3)
  p5 <- added_variable_plot(data, model_n3, model_p.3)
  r2 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_c3, model_s3)
  p2 <- added_variable_plot(data, model_c3, model_n3)
  p3 <- added_variable_plot(data, model_c3, model_o3)
  p4 <- added_variable_plot(data, model_c3, model_p3)
  p5 <- added_variable_plot(data, model_c3, model_p.3)
  r3 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_o3, model_s3)
  p2 <- added_variable_plot(data, model_o3, model_c3)
  p3 <- added_variable_plot(data, model_o3, model_n3)
  p4 <- added_variable_plot(data, model_o3, model_p3)
  p5 <- added_variable_plot(data, model_o3, model_p.3)
  r4 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p3, model_s3)
  p2 <- added_variable_plot(data, model_p3, model_c3)
  p3 <- added_variable_plot(data, model_p3, model_n3)
  p4 <- added_variable_plot(data, model_p3, model_o3)
  p5 <- added_variable_plot(data, model_p3, model_p.3)
  r5 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p.3, model_s3)
  p2 <- added_variable_plot(data, model_p.3, model_c3)
  p3 <- added_variable_plot(data, model_p.3, model_n3)
  p4 <- added_variable_plot(data, model_p.3, model_o3)
  p5 <- added_variable_plot(data, model_p.3, model_p3)
  r6 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  resid.plot.Muzz <- ggarrange(r1,r2,r3,r4,r5,r6,nrow = 6) 
  
  p1 <- added_variable_plot(data, model_s4, model_n4)
  p2 <- added_variable_plot(data, model_s4, model_c4)
  p3 <- added_variable_plot(data, model_s4, model_o4)
  p4 <- added_variable_plot(data, model_s4, model_p4)
  p5 <- added_variable_plot(data, model_s4, model_p.4)
  r1 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_n4, model_s4)
  p2 <- added_variable_plot(data, model_n4, model_c4)
  p3 <- added_variable_plot(data, model_n4, model_o4)
  p4 <- added_variable_plot(data, model_n4, model_p4)
  p5 <- added_variable_plot(data, model_n4, model_p.4)
  r2 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_c4, model_s4)
  p2 <- added_variable_plot(data, model_c4, model_n4)
  p3 <- added_variable_plot(data, model_c4, model_o4)
  p4 <- added_variable_plot(data, model_c4, model_p4)
  p5 <- added_variable_plot(data, model_c4, model_p.4)
  r3 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_o4, model_s4)
  p2 <- added_variable_plot(data, model_o4, model_c4)
  p3 <- added_variable_plot(data, model_o4, model_n4)
  p4 <- added_variable_plot(data, model_o4, model_p4)
  p5 <- added_variable_plot(data, model_o4, model_p.4)
  r4 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p4, model_s4)
  p2 <- added_variable_plot(data, model_p4, model_c4)
  p3 <- added_variable_plot(data, model_p4, model_n4)
  p4 <- added_variable_plot(data, model_p4, model_o4)
  p5 <- added_variable_plot(data, model_p4, model_p.4)
  r5 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  p1 <- added_variable_plot(data, model_p.4, model_s4)
  p2 <- added_variable_plot(data, model_p.4, model_c4)
  p3 <- added_variable_plot(data, model_p.4, model_n4)
  p4 <- added_variable_plot(data, model_p.4, model_o4)
  p5 <- added_variable_plot(data, model_p.4, model_p4)
  r6 <- ggarrange(p1,p2, p3, p4, p5, nrow = 1)
  resid.plot.Mumbai <- ggarrange(r1,r2,r3,r4,r5,r6,nrow = 6) 
  
  return(list("resid.plot.Kolkata" = resid.plot.Kolkata, "resid.plot.Delhi"=resid.plot.Delhi, "resid.plot.Muzz"=resid.plot.Muzz, "resid.plot.Mumbai"=resid.plot.Mumbai))
}


format_reg_table <- function(model, digs = 2){
  mod <- tidy(model, conf.level = .99, conf.int = TRUE) %>%
    mutate(across(where(is.numeric), ~ round(., digits = digs)))
  mod <- mod %>%
    mutate(estimate = as.character(estimate)) %>%
    mutate(estimate = paste(paste(estimate, ifelse(p.value < 0.05, "*",""), ifelse(p.value < 0.01, "*",""), ifelse(p.value < 0.001, "*",""), sep = ""), paste("[", conf.low, ", ", conf.high, "]", sep = ""), sep = "\n"))
  mod <- mod %>%
    select(term, estimate)
  return(mod)
}

present_all_pols_model <- function(data, model_s, model_n, model_c, model_o, model_p, model_p.){
  mod1 <- format_reg_table(model_s) %>% rename(SO2 = estimate)
  mod2 <- format_reg_table(model_n) %>% select(estimate) %>% rename(NO2 = estimate)
  mod3 <- format_reg_table(model_c) %>% select(estimate) %>% rename(CO = estimate)
  mod4 <- format_reg_table(model_o) %>% select(estimate) %>% rename(O3 = estimate)
  mod5 <- format_reg_table(model_p) %>% select(estimate) %>% rename(PM10 = estimate)
  mod6 <- format_reg_table(model_p.) %>% select(estimate) %>% rename(PM25 = estimate)
  
  R2table <<- tibble(term = c("R2"), SO2 = c(find_R.squared(model_s, data)), NO2 = c(find_R.squared(model_n, data)), CO = c(find_R.squared(model_c, data)), O3 = c(find_R.squared(model_o, data)), PM10 = c(find_R.squared(model_p, data)), PM25 = c(find_R.squared(model_p., data)))
  R2table <<- R2table %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    mutate(across(where(is.numeric), ~ as.character(.)))
  
  suppressMessages(fin.table <<- as_tibble(cbind(mod1, mod2, mod3, mod4, mod5, mod6)) %>%
    full_join(R2table))
  return(fin.table)
}

present_all_pols_otherpols_model <- function(data, model_s, model_n, model_c, model_o, model_p, model_p.){
  mod1 <- format_reg_table(model_s) %>% select(estimate) %>% rename(SO2 = estimate)
  mod2 <- format_reg_table(model_n) %>% select(estimate) %>% rename(NO2 = estimate)
  mod3 <- format_reg_table(model_c) %>% select(estimate) %>% rename(CO = estimate)
  mod4 <- format_reg_table(model_o) %>% select(estimate) %>% rename(O3 = estimate)
  mod5 <- format_reg_table(model_p) %>% select(estimate) %>% rename(PM10 = estimate)
  mod6 <- format_reg_table(model_p.) %>% select(estimate) %>% rename(PM25 = estimate)
  
  mod <- tibble(
    term = c("(Intercept)", "SO2", "NO2", "CO", "O3", "PM10", "PM25"),
    SO2 = c(mod1$SO2[1], NA, mod1$SO2[2:nrow(mod1)]),
    NO2 = c(mod2$NO2[1:2], NA, mod2$NO2[3:nrow(mod2)]),
    CO = c(mod3$CO[1:3], NA, mod3$CO[4:nrow(mod3)]),
    O3 = c(mod4$O3[1:4], NA, mod4$O3[5:nrow(mod4)]),
    PM10 = c(mod5$PM10[1:5], NA, mod5$PM10[6:nrow(mod5)]),
    PM25 = c(mod6$PM25[1:nrow(mod6)], NA),
  )
  
  R2table <<- tibble(term = c("R2"), SO2 = c(find_R.squared(model_s, data)), NO2 = c(find_R.squared(model_n, data)), CO = c(find_R.squared(model_c, data)), O3 = c(find_R.squared(model_o, data)), PM10 = c(find_R.squared(model_p, data)), PM25 = c(find_R.squared(model_p., data)))
  R2table <<- R2table %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    mutate(across(where(is.numeric), ~ as.character(.)))
  
  suppressMessages(fin.table <<- mod %>%
                     full_join(R2table))
  return(fin.table)
}

# Presenting Model Summary Concisely.
present_model_summary <- function(data, city_name = "Kolkata"){ # The Data must be Yearly
  
  # Kolkata
  # On weather params
  invisible(capture.output(kol <- train_city_pol(data %>% filter(City == city_name, Month %in% c(1,2,3,4,5,6)))))
  nonpol_model_1 <<- present_all_pols_model(data %>% filter(City == city_name, Month %in% c(1,2,3,4,5,6)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  invisible(capture.output(kol <- train_city_pol(data %>% filter(City == city_name, Month %in% c(7,8,9,10,11,12)))))
  nonpol_model_2 <<- present_all_pols_model(data %>% filter(City == city_name, Month %in% c(7,8,9,10,11,12)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  nonpol_model_1 <- ggtexttable(nonpol_model_1, rows = NULL, theme = ttheme("mGreen")) %>%
    tab_add_title("First Half of the Year (Pollutant Levels On Weather Parameters)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  nonpol_model_2 <- ggtexttable(nonpol_model_2, rows = NULL, theme = ttheme("mBlue")) %>%
    tab_add_title("Second Half of the Year (Pollutant Levels on Weather Parameters)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  
  # On other Pols
  invisible(capture.output(kol <- train_city_pol_w_pol(data %>% filter(City == city_name, Month %in% c(1,2,3,4,5,6)))))
  pol_model_1 <<- present_all_pols_otherpols_model(data %>% filter(City == city_name, Month %in% c(1,2,3,4,5,6)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  invisible(capture.output(kol <- train_city_pol_w_pol(data %>% filter(City == city_name, Month %in% c(7,8,9,10,11,12)))))
  pol_model_2 <<- present_all_pols_otherpols_model(data %>% filter(City == city_name, Month %in% c(7,8,9,10,11,12)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  pol_model_1 <- ggtexttable(pol_model_1, rows = NULL, theme = ttheme("mOrange")) %>%
    tab_add_title("First Half of the Year (Pollutant Levels on Other Pollutant Levels)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  pol_model_2 <- ggtexttable(pol_model_2, rows = NULL, theme = ttheme("mRed")) %>%
    tab_add_title("Second Half of the Year (Pollutant Levels on Other Pollutant Levels)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  
  
  kolkata <- annotate_figure(ggarrange(nonpol_model_1, pol_model_1, nonpol_model_2, pol_model_2, nrow = 2, ncol = 2), top = text_grob(city_name, face = "bold", size = 14))
  kolkata
  
}

present_model_summary_Delhi <- function(data){ # The Data must be Yearly
  
  # Delhi
  # On weather params
  invisible(capture.output(kol <- train_city_pol(data %>% filter(City == "Delhi", Month %in% c(1,2,3,4,5,6)))))
  nonpol_model_1 <<- present_all_pols_model(data %>% filter(City == "Delhi", Month %in% c(1,2,3,4,5,6)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  invisible(capture.output(kol <- train_city_pol(data %>% filter(City == "Delhi", Month %in% c(7,8,9,10,11,12)))))
  nonpol_model_2 <<- present_all_pols_model(data %>% filter(City == "Delhi", Month %in% c(7,8,9,10,11,12)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  nonpol_model_1 <- ggtexttable(nonpol_model_1, rows = NULL, theme = ttheme("mGreen")) %>%
    tab_add_title("First Half of the Year (Pollutant Levels On Weather Parameters)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  nonpol_model_2 <- ggtexttable(nonpol_model_2, rows = NULL, theme = ttheme("mBlue")) %>%
    tab_add_title("Second Half of the Year (Pollutant Levels on Weather Parameters)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  
  # On other Pols
  invisible(capture.output(kol <- train_city_pol_w_pol(data %>% filter(City == "Delhi", Month %in% c(1,2,3,4,5,6)))))
  pol_model_1 <<- present_all_pols_otherpols_model(data %>% filter(City == "Delhi", Month %in% c(1,2,3,4,5,6)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  invisible(capture.output(kol <- train_city_pol_w_pol(data %>% filter(City == "Delhi", Month %in% c(7,8,9,10,11,12)))))
  pol_model_2 <<- present_all_pols_otherpols_model(data %>% filter(City == "Delhi", Month %in% c(7,8,9,10,11,12)), model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  pol_model_1 <- ggtexttable(pol_model_1, rows = NULL, theme = ttheme("mOrange")) %>%
    tab_add_title("First Half of the Year (Pollutant Levels on Other Pollutant Levels)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  pol_model_2 <- ggtexttable(pol_model_2, rows = NULL, theme = ttheme("mRed")) %>%
    tab_add_title("Second Half of the Year (Pollutant Levels on Other Pollutant Levels)", face = "bold") %>%
    tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% confidence Intervals", size = 10, face = "italic")
  
  
  Delhi <- annotate_figure(ggarrange(nonpol_model_1, pol_model_1, nonpol_model_2, pol_model_2, nrow = 2, ncol = 2), top = text_grob("Delhi", face = "bold", size = 14))
  Delhi
  
}

# Usage
present_model_summary(air_data_india_All_Specie_Daily_Regress %>% filter(Year == 2019), city_name = "Muzaffarnagar")

# Printing Residual Difference Among Years
make_predictions_comparison <- function(residual_data){
  p <- residual_data %>%
    ggplot(aes(residuals, ..density.., fill = Residual_Year)) +
    geom_histogram(position = "identity", alpha = 0.2) + 
    geom_density(alpha = 0.4) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
    scale_fill_viridis(discrete = TRUE)
  return(p)
}

comparing_year_residuals <- function(full_data, model_train_year, model_train_month){
  
  test_region <- full_data %>% filter(City == "Kolkata", Month %in% model_train_month)
  
  grid <- test_region %>%
    gather_residuals(model_s1, model_n1, model_c1, model_o1, model_p1, model_p.1)
  
  test_region <- full_data %>% filter(City == "Delhi", Month %in% model_train_month)
  
  grid <- grid %>%
    full_join(
      test_region %>%
        gather_residuals(model_s2, model_n2, model_c2, model_o2, model_p2, model_p.2)
    )
  
  grid %>%
    ggplot(aes(resid,..density.., fill = Year)) +
    geom_histogram(position = "identity", alpha = 0.3) +
    geom_density(alpha = 0.4) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),strip.background = element_blank(), legend.position = "bottom") +
    scale_y_continuous(labels = NULL) +
    facet_wrap(~City + model, scales = "free", labeller = label_wrap_gen(multi_line=FALSE), ncol = 3) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(paste("Residuals from the Model Fit with", model_train_year, ifelse(identical(model_train_month, c(1,2,3,4,5,6)), ", First 6 Months Weather Parameters", "Last 6 Months Weather Parameters")))
}

comparing_year_residuals(air_data_india_All_Specie_Daily_Regress, "2019", c(1,2,3,4,5,6))
