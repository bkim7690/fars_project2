# This script will include code to define 3 functions:
## 1. perc_cis
## 2. test_trend_ca 
## 3. test_trend_log_reg brian focus on third
# These functions will be used to analyze the clean fars data (clean_fars in the data subdirectory)

#Function 1 perc_cis
perc_cis <- function(x, n) {
  
  p <- x/n
  p_perc <- round(p * 100, digits = 1)
  
  se_p <- sqrt((p * (1-p))/n)
  
  upper_ci <- round(p_perc + (1.96 * se_p) * 100, digits = 1)
  lower_ci <- round(p_perc - (1.96 * se_p) * 100, digits = 1)
  
  paste0(p_perc, "% (", lower_ci, "%, ", upper_ci, "%)")
}
perc_cis(x = 9000, n = 23000)

#Function 2 test_trend_ca
# second function
test_trend_ca <- function(drug, data = clean_fars){
  
  if (drug == "Nonalcohol") {
    simple_fars2 <- data %>%
      mutate(drug_type = as.character(drug_type)) %>%
      filter(drug_type != "Alcohol") %>%
      group_by(unique_id, year) %>%
      summarize(positive_for_drug = any(positive_for_drug)) %>%
      ungroup() %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    
    ca_result1 <- prop.trend.test(x = simple_fars2$positive,
                                  n = simple_fars2$trials)
    
    ca_result <- as_tibble(data.frame(Z = round(sqrt(ca_result1$statistic), 1),
                                      p.value = round(ca_result1$p.value, 3)))
    
  }
  
  else {
    simple_fars <- data %>%
      mutate(drug_type = as.character(drug_type)) %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    
    ca_result2 <- prop.trend.test(x = simple_fars$positive,
                                  n = simple_fars$trials)
    
    ca_result <- as_tibble(data.frame(Z = round(sqrt(ca_result2$statistic), 1),
                                      p.value = round(ca_result2$p.value, 3)))
    
  } 
  
  row.names(ca_result) <- NULL
  return(ca_result)
  
}


#Function 3 test_trend_log_reg
test_trend_log_reg <- function(drug, df = clean_fars){
 
  if(drug == "Nonalcohol"){
    test_na <- df %>%
      mutate(drug_type = as.character(drug_type)) %>%
      filter(drug_type != "Alcohol")
    log_reg_na <- glm(positive_for_drug ~ year, data = test_na,
                        family = binomial(link = "logit"))
    result1 <- summary(log_reg_na)$coefficients
    
    result <- as_tibble(data.frame(Z = round(result1[2, 3] ,1),
                        p.value = round(result1[2, 4] ,3)))
  }
  else {
    test <- df %>%
      filter(drug_type == drug)
    log_reg <- glm(positive_for_drug ~ year, data = test,
                   family = binomial(link = "logit"))
   result2 <- summary(log_reg)$coefficients
   
   result <- as_tibble(data.frame(Z = round(result2[2, 3] ,1),
                       p.value = round(result2[2, 4] ,3)))

  }
  row.names(result) <- NULL
  return(result)
}
  
