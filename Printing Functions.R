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
  
  gof <<- fitdistrplus::gofstat(list(fit_g, fit_l, fit_w, fit_n, fit_e), fitnames = plot.legend)
  print(gof)
  print(gof$chisqpvalue)
  
  plot.legend <- c("gamma", "lognormal", "weibull", "normal")#, "exponential")
  p1 <- fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend, fitlwd = c(2,2,2,2), xlegend = 0.007, plotstyle = "ggplot") #, fit_e), legendtext = plot.legend, fitlwd = c(3,3.5,4,4.5,1), xlegend = 0.007)
  #fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend)
  p2 <- fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n), legendtext = plot.legend, plotstyle = "ggplot") # , fit_e), legendtext = plot.legend, xlegend = 0.01, )
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
    ggplot(aes(resid)) +
    geom_histogram()
  return(p)
}

# Training top 9 cities
train_all_cities_pol_with_nonpol <- function(data){
  # so2 on non-pollutants
  model_s1 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_s5 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_s6 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_s7 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_s8 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_s9 <<- lm(so2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
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
  model_n1 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_n5 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_n6 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_n7 <<- lm(no2 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
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
  model_c1 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_c5 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_c6 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_c7 <<- lm(co ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
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
  model_o2 <<- lm(o3~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_o5 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_o6 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_o7 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_o8 <<- lm(o3 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
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
  model_p1 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  #model_p5 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  #model_p6 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_p7 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_p8 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_p9 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
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
  model_p.1 <<- lm(pm10 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Mumbai"))
  model_p.5 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Lucknow"))
  model_p.6 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Patna"))
  model_p.7 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Chandigarh"))
  model_p.8 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Gandhinagar"))
  model_p.9 <<- lm(pm25 ~ dew + humidity + pressure + temperature + `wind-speed`, data = data %>% filter(City == "Jaipur"))
  
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

train_all_cities_pol_with_otherpol <- function(data){
  # so2 on pollutants
  model_s1 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_s2 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_s3 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_s4 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_s5 <<- lm(so2 ~ no2 + co + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  model_s6 <<- lm(so2 ~ no2 + co + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_s7 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_s8 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_s9 <<- lm(so2 ~ no2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
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
  model_n1 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_n2 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_n3 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_n4 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_n5 <<- lm(no2 ~ so2 + co + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  model_n6 <<- lm(no2 ~ so2 + co + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_n7 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_n8 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_n9 <<- lm(no2 ~ so2 + co + o3 + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
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
  model_c1 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_c2 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_c3 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_c4 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_c5 <<- lm(co ~ so2 + no2 + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  model_c6 <<- lm(co ~ so2 + no2 + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_c7 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_c8 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_c9 <<- lm(co ~ so2 + no2 + o3 + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
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
  model_o1 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Kolkata"))
  model_o2 <<- lm(o3~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Delhi"))
  model_o3 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_o4 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Mumbai"))
  model_o5 <<- lm(o3 ~ so2 + no2 + co + pm25, data = data %>% filter(City == "Lucknow"))
  model_o6 <<- lm(o3 ~ so2 + no2 + co + pm25, data = data %>% filter(City == "Patna"))
  model_o7 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_o8 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_o9 <<- lm(o3 ~ so2 + no2 + co + pm10 + pm25, data = data %>% filter(City == "Jaipur"))
  
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
  model_p1 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Kolkata"))
  model_p2 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Delhi"))
  model_p3 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Muzaffarnagar"))
  model_p4 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Mumbai"))
  #model_p5 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Lucknow"))
  #model_p6 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Patna"))
  model_p7 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Chandigarh"))
  model_p8 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Gandhinagar"))
  model_p9 <<- lm(pm10 ~ so2 + no2 + co + o3 + pm25, data = data %>% filter(City == "Jaipur"))
  
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
  model_p.1 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Kolkata"))
  model_p.2 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Delhi"))
  model_p.3 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Muzaffarnagar"))
  model_p.4 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Mumbai"))
  model_p.5 <<- lm(pm25 ~ so2 + no2 + co + o3, data = data %>% filter(City == "Lucknow"))
  model_p.6 <<- lm(pm25 ~ so2 + no2 + co + o3, data = data %>% filter(City == "Patna"))
  model_p.7 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Chandigarh"))
  model_p.8 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Gandhinagar"))
  model_p.9 <<- lm(pm25 ~ so2 + no2 + co + o3 + pm10, data = data %>% filter(City == "Jaipur"))
  
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

# Predicting top 9 cities pollutants from non pol trained data
predict_all_cities_pol_with_nonpol <- function(data){
  # Non-pollutants based predictions --- MAKE SURE THE model_s1 and all model used in this part is trained with non-pollutants
  
  # So2 Level prediction
  p1 <- make_predictions(model_s1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_s2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  p3 <- make_predictions(model_s3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  p4 <- make_predictions(model_s4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  p5 <- make_predictions(model_s5, data %>% filter(City == "Lucknow")) + ggtitle("Lucknow")
  p6 <- make_predictions(model_s6, data %>% filter(City == "Patna")) + ggtitle("Patna")
  p7 <- make_predictions(model_s7, data %>% filter(City == "Chandigarh")) + ggtitle("Chandigarh")
  p8 <- make_predictions(model_s8, data %>% filter(City == "Gandhinagar")) + ggtitle("Gandhinagar")
  p9 <- make_predictions(model_s9, data %>% filter(City == "Jaipur")) + ggtitle("Jaipur")
  
  res_so2 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # No2 Level prediction
  p1 <- make_predictions(model_n1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_n2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  p3 <- make_predictions(model_n3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  p4 <- make_predictions(model_n4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  p5 <- make_predictions(model_n5, data %>% filter(City == "Lucknow")) + ggtitle("Lucknow")
  p6 <- make_predictions(model_n6, data %>% filter(City == "Patna")) + ggtitle("Patna")
  p7 <- make_predictions(model_n7, data %>% filter(City == "Chandigarh")) + ggtitle("Chandigarh")
  p8 <- make_predictions(model_n8, data %>% filter(City == "Gandhinagar")) + ggtitle("Gandhinagar")
  p9 <- make_predictions(model_n9, data %>% filter(City == "Jaipur")) + ggtitle("Jaipur")
  
  res_no2 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # CO Level prediction
  p1 <- make_predictions(model_c1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_c2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  p3 <- make_predictions(model_c3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  p4 <- make_predictions(model_c4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  p5 <- make_predictions(model_c5, data %>% filter(City == "Lucknow")) + ggtitle("Lucknow")
  p6 <- make_predictions(model_c6, data %>% filter(City == "Patna")) + ggtitle("Patna")
  p7 <- make_predictions(model_c7, data %>% filter(City == "Chandigarh")) + ggtitle("Chandigarh")
  p8 <- make_predictions(model_c8, data %>% filter(City == "Gandhinagar")) + ggtitle("Gandhinagar")
  p9 <- make_predictions(model_c9, data %>% filter(City == "Jaipur")) + ggtitle("Jaipur")
  
  res_co <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # O3 Level prediction
  p1 <- make_predictions(model_o1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_o2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  p3 <- make_predictions(model_o3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  p4 <- make_predictions(model_o4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  p5 <- make_predictions(model_o5, data %>% filter(City == "Lucknow")) + ggtitle("Lucknow")
  p6 <- make_predictions(model_o6, data %>% filter(City == "Patna")) + ggtitle("Patna")
  p7 <- make_predictions(model_o7, data %>% filter(City == "Chandigarh")) + ggtitle("Chandigarh")
  p8 <- make_predictions(model_o8, data %>% filter(City == "Gandhinagar")) + ggtitle("Gandhinagar")
  p9 <- make_predictions(model_o9, data %>% filter(City == "Jaipur")) + ggtitle("Jaipur")
  
  res_o3 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  # pm10 Level prediction
  p1 <- make_predictions(model_p1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_p2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  p3 <- make_predictions(model_p3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  p4 <- make_predictions(model_p4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  #p5 <- make_predictions(model_p.5, data %>% filter(City == "Lucknow")) + ggtitle("Lucknow")
  #p6 <- make_predictions(model_p.6, data %>% filter(City == "Patna")) + ggtitle("Patna")
  p7 <- make_predictions(model_p7, data %>% filter(City == "Chandigarh")) + ggtitle("Chandigarh")
  p8 <- make_predictions(model_p8, data %>% filter(City == "Gandhinagar")) + ggtitle("Gandhinagar")
  p9 <- make_predictions(model_p9, data %>% filter(City == "Jaipur")) + ggtitle("Jaipur")
  
  res_pm10 <- ggarrange(p1,p2,p3,p4,p7,p8,p9, nrow = 3, ncol = 3)
  
  # pm25 Level prediction
  p1 <- make_predictions(model_p.1, data %>% filter(City == "Kolkata")) + ggtitle("Kolkata")
  p2 <- make_predictions(model_p.2, data %>% filter(City == "Delhi")) + ggtitle("Delhi")
  p3 <- make_predictions(model_p.3, data %>% filter(City == "Muzaffarnagar")) + ggtitle("Muzaffarnagar")
  p4 <- make_predictions(model_p.4, data %>% filter(City == "Mumbai")) + ggtitle("Mumbai")
  p5 <- make_predictions(model_p.5, data %>% filter(City == "Lucknow")) + ggtitle("Lucknow")
  p6 <- make_predictions(model_p.6, data %>% filter(City == "Patna")) + ggtitle("Patna")
  p7 <- make_predictions(model_p.7, data %>% filter(City == "Chandigarh")) + ggtitle("Chandigarh")
  p8 <- make_predictions(model_p.8, data %>% filter(City == "Gandhinagar")) + ggtitle("Gandhinagar")
  p9 <- make_predictions(model_p.9, data %>% filter(City == "Jaipur")) + ggtitle("Jaipur")
  
  res_pm25 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
  
  return(list("res_so2"=res_so2, "res_no2"=res_no2, "res_co"=res_co, "res_o3"=res_o3, "res_pm10"=res_pm10, "res_pm25"=res_pm25))
}
