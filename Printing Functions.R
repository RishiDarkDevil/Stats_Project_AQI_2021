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
