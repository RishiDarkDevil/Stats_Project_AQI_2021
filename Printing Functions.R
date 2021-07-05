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

fit_distribution <- function(data){
  fit_g <- fitdistrplus::fitdist(data, "gamma")
  fit_l <- fitdistrplus::fitdist(data, "lnorm")
  fit_w <- fitdistrplus::fitdist(data, "weibull")
  fit_n <- fitdistrplus::fitdist(data, "norm")
  fit_e <- fitdistrplus::fitdist(data, "exp")
  
  par(mfrow=c(1,2))
  plot.legend <- c("gamma", "lognormal", "weibull", "normal", "exponential")
  fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend, fitlwd = c(1,1.5,2,2.5,3), xlegend = 0.8)
  #fitdistrplus::cdfcomp (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend)
  fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend)
  #fitdistrplus::ppcomp  (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend)
  par(mfrow = c(1,1))
  
  gof <<- fitdistrplus::gofstat(list(fit_g, fit_l, fit_w, fit_n, fit_e), fitnames = plot.legend)
  print(gof)
  
  
}

