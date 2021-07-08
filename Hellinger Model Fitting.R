
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
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2021_avg_median, pollutants == "pm25")$AQI, "pnorm")
compare_hellinger_fit_v_MLE(filter(air_data_pollutants_2020_avg_median, pollutants == "pm25")$AQI, "pnorm")
compare_hellinger_fit_v_MLE((air_data_india_Pollutants_nonpollutants_daily %>%
                    filter(Measure == "Avg_Min" & !(pollutants %in% c("pm10", "pm25"))))$AQI, "plnorm")
compare_hellinger_fit_v_MLE((air_data_india_Pollutants_nonpollutants_daily %>%
                               filter(Measure == "Avg_Median" & !(pollutants %in% c("pm10", "pm25"))))$AQI, "pnorm")
compare_hellinger_fit_v_MLE((air_data_india_Pollutants_nonpollutants_daily %>%
                               filter(Measure == "Avg_Max" & !(pollutants %in% c("pm10", "pm25"))))$AQI, "plnorm")
compare_hellinger_fit_v_MLE((air_data_india_All_Specie_Monthly_2_pollutants_combined)$Mean_Pollutant_AQI, "pweibull")

# A Customized Versatile Version of lnorm - Source: KM Assignment 6 Prob Theory 1st Year, 2nd Sem
customized_lnorm_fit <- function(data, guess, smoothness = 0.1){
  lnorm_tweaked <- function(theta, x) ((1/(sqrt(2*pi*theta[2]^2)*x))*exp(-(1/(2*(theta[2]^2)))*(log(x)-theta[1])^2))*(1 + theta[3]*sin(2*pi*theta[4]*x))
  
  lnorm_tweaked_likelihood <- function(theta, data) {
    log(prod(lnorm_tweaked(theta = theta, x = data)))
  }
  
  lnorm_tweaked_log_likelihood <- function(theta, data){
    -length(data)*log(sqrt(2*pi)*theta[2]) - sum(log(data)) - (1/(2*theta[2]^2))*sum((log(data)-theta[1])^2) + sum(log(1+theta[3]*sin(2*theta[4]*pi*data)))
  }
  
  different_lognormal_fit <- function(data, guess, smoothness = 0.1){
    data1 <<- tibble(x = data)
    
    fitted_params <<- optim(as.vector(guess), lnorm_tweaked_log_likelihood, data = data, control = list(fnscale=-1))
    theta1 <<- fitted_params
    
    x <- seq(min(data), max(data), by = smoothness)
    tweaked_lnorm <- lnorm_tweaked(theta1$par, x)
    
    plot_data <- tibble(x1 = x, y1 = tweaked_lnorm)
    
    p1 <<- data1 %>%
      ggplot(aes(x, ..density..)) +
      geom_histogram() +
      geom_line(data = plot_data, aes(x1, y1), size = 2, color = "blue")
    
    estimates <<- tibble(
      mu = fitted_params$par[1],
      sigma = fitted_params$par[2],
      sine_coeff = fitted_params$par[3],
      theta_mult = fitted_params$par[4]
    )
  }
  different_lognormal_fit(data, guess, smoothness)
  print(p1)
  return(estimates)
}

#Usage
customized_lnorm_fit(filter(air_data_pollutants_2019_avg_median, pollutants == "so2")$AQI, c(2,1,0.1,1), 0.001) # The Function Attains minima at many points so be careful
customized_lnorm_fit(filter(air_data_pollutants_2019_avg_median, pollutants == "o3")$AQI, c(1,1,0.1,1), 0.01)
