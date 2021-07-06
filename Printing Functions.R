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

data1 %>%
  ggplot(aes(x, ..density..)) +
  geom_histogram() +
  geom_line(data = line_fit, aes(x = x, y = n), size = 1, color = "red")
