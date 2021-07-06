
shape <- 6.12530314; rate <- 0.02886038

# Calculates the sqrt(f).sqrt(g) things that needs to be maximized for minimum hellinger distance
hellinger_distance.minimizer <- function(...) {
  y <<- as.list(...) 
  x <<- as.list(c(NA, unlist(y)))
  x[[1]] <<- c(-Inf, gof$chisqbreaks, Inf)
  hellinger <<- sum(sqrt(diff(do.call(distribution, x)))*sqrt(as.data.frame(gof$chisqtable)[1:(length(gof$chisqbreaks)+1), 3] / sum(as.data.frame(gof$chisqtable)[1:(length(gof$chisqbreaks)+1), 3])))
  return(hellinger)
}

# Fits Model based on Minimizing Hellinger Distance
hellinger_fit <- function(data, distribution){
  data1 <<- data
  distribution <<- distribution
  fit <<- fitdistrplus::fitdist(data, sub('.', '', distribution))
  gof <<- fitdistrplus::gofstat(fit, fitnames = c(sub('.', '', distribution))) 
  estimates <<- optim(par = as.vector(fit$estimate), fn = hellinger_distance.minimizer, control = list(fnscale=-1))
  sq_hellinger_dist <<- sum(diff(do.call(distribution, x))) + sum(as.data.frame(gof$chisqtable)[1:(length(gof$chisqbreaks)+1), 3] / sum(as.data.frame(gof$chisqtable)[1:(length(gof$chisqbreaks)+1), 3])) - 2*hellinger_distance.minimizer(estimates$par)
  return(list("Hellinger_Estimates" = estimates$par, "Square_Hellinger_Distance" = sq_hellinger_dist))
}

# Usage
hellinger_fit(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "pgamma")

hellinger_fit(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "pexp")

hellinger_fit(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "pnorm")

hellinger_fit(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "pnorm") #

hellinger_fit(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "plnorm")
hellinger_fit(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "pexp")
hellinger_fit(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "pweibull")
hellinger_fit(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "pgamma")


distribution <- "pnorm"
hellinger_distance.minimizer(c(254.86186, 58.02709))
distribution <- "pexp"
hellinger_distance.minimizer(0.003743891)

hellinger_distance.minimizer(0.003913581)

# Comparing Hellinger Fit with MLE Fit
compare_hellinger_fit_v_MLE <- function(data, distribution){
  fit_MLE <<- fitdistrplus::fitdist(data, sub('.', '', distribution))
  fit_hellinger <<- fitdistrplus::fitdist(data, sub('.', '', distribution))
  fit_hellinger$estimate <<- hellinger_fit(data, distribution)$Hellinger_Estimates
  
  print(fit_hellinger$estimate)
  print("Squared Hellinger-Distance")
  print(hellinger_fit(data, distribution)$Square_Hellinger_Distance)
  
  plot.legend <- c("MLE_Fit", "Hellinger_Fit")
  p1 <- fitdistrplus::denscomp(list(fit_MLE, fit_hellinger), legendtext = plot.legend, fitlwd = c(2, 2), xlegend = 0.007, plotstyle = "ggplot")
  p2 <- fitdistrplus::qqcomp  (list(fit_MLE, fit_hellinger), legendtext = plot.legend, plotstyle = "ggplot")
  p1 <- p1 + theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ggtitle("Hellinger Fit vs MLE Fit Model")
  p2 <- p2 + theme(legend.position = "None")
  ggarrange(p1, p2, ncol = 2)
}

# Usage
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "pnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "co")$AQI, "plnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, "pweibull")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "plnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "pgamma")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "pweibull")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "pnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "pm10")$AQI, "pgamma")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "pm25")$AQI, "plnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2021_avg_median, pollutants == "no2")$AQI, "plnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, "plnorm")
compare_hellinger_fit_v_MLE(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "plnorm")
compare_hellinger_fit_v_MLE(air_data_pollutants_2021_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "pweibull")
compare_hellinger_fit_v_MLE(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "pweibull")
compare_hellinger_fit_v_MLE(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "pgamma")
compare_hellinger_fit_v_MLE(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "pnorm")
compare_hellinger_fit_v_MLE(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "plnorm")
compare_hellinger_fit_v_MLE(air_data_pollutants_2019_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "plnorm")
compare_hellinger_fit_v_MLE(air_data_pollutants_2020_avg_median_all_pol_summarized_wo_pm$Mean_AQI, "plnorm")
