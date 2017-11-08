#Fars_Project Script

library(foreign)
library(tidyverse)
library(tidyselect)
library(knitr)

# Use the following code only if you downloaded .dbf files by hand, instead of
# writing them out as .csv files using R. The fars_dbf_to_csv function reads
# in .dbf files as data frames, and then writes them as csv files to the
# "data-raw/yearly_person_data" directory. The `map` step iterates the
# function across all of the .dbf files saved in the
# "data-raw/yearly_person_data" directory.

fars_dbf_to_csv <- function(year) {
  # Save the directory where .dbf files are saved.
  dir <- "data_raw/yearly_person_data"
  # Read the .dbf file for a year into R.
  person_data <- foreign::read.dbf(paste0(dir,"/PERSON_", year, ".dbf"))
  # Save each file as a csv to the "data-raw/yearly_person_data" directory.
  person_file <- paste0("data_raw/yearly_person_data/person_", year, ".csv")
  readr::write_csv(person_data,
                   path = person_file)
  # Return NULL so that the function doesn't print out anything.
  return(NULL)
}


setwd("C:/Users/bkim7/R/fars_project")
clean_yearly_person_file <- function(year) {
  # 1. Read data in.
  person_file <- paste0("data_raw/yearly_person_data/person_", year, ".csv")
  df <- readr::read_csv(person_file)
  # 2. Convert all column names to lowercase.
  colnames(df) <- tolower(colnames(df))
  df <- df %>%
    # 3. Limit variables.
    dplyr::select(st_case, veh_no, per_no, state, per_typ, lag_hrs, lag_mins,
                  inj_sev, age, alc_res, contains("drugres"), sex) %>%
    # 4. Limit to relevant `per_typ` and `inj_sev` values, then remove those variables.
    dplyr::filter(per_typ == 1 & inj_sev == 4) %>%
    dplyr::select(-per_typ, -inj_sev) %>%
    # 5. Create a `unique_id`. Note: to be unique, `year` needs to be pasted on.
    tidyr::unite(unique_id, st_case, veh_no, per_no) %>%
    dplyr::mutate(year = year,
                  unique_id = paste(unique_id, year, sep = "_")) %>%
    # 6. Limit to study states and then remove the `state` variable.
    dplyr::filter(state %in% c(6,
                               15,
                               17,
                               33,
                               44,
                               54)) %>%
    dplyr::select(-state) %>%
    # 7. Convert `sex` to a factor with levels "Male" and "Female".
    dplyr::mutate(sex = ifelse(sex == 9, NA, sex),
                  sex = factor(sex, levels = c(1, 2),
                               labels = c("Male", "Female"))) %>%
    # 8. Use measured alcohol blood level to create `Alcohol` (logical for whether
    # alcohol was present). Then remove the `alc_res` variable.
    dplyr::mutate(alc_res = ifelse(alc_res > 94, NA, alc_res / 10),
                  Alcohol = alc_res >= 0.01) %>%
    dplyr::select(-alc_res) %>%
    # 9. Specify missing values for the lag minutes.
    dplyr::mutate(lag_mins = ifelse(lag_mins == 99, NA, lag_mins))
  # 10. Save lag hours coded as missing as `NA`.
  if(year <= 2008){
    df <- df %>%
      dplyr::mutate(lag_hrs = ifelse(lag_hrs %in% c(99, 999), NA, lag_hrs))
  } else {
    df <- df %>%
      dplyr::mutate(lag_hrs = ifelse(lag_hrs == 999, NA, lag_hrs))
  }
  # 11. Limit to deaths within an hour of the accident then remove those variables.
  df <- df %>%
    dplyr::filter((lag_hrs < 1) | (lag_hrs == 1 & lag_mins == 0)) %>%
    dplyr::select(-lag_hrs, -lag_mins)
  # 12. Save age values coded as missing as `NA`.
  if(year <= 2008){
    df <- df %>%
      dplyr::mutate(age = ifelse(age == 99, NA, age))
  } else {
    df <- df %>%
      dplyr::mutate(age = ifelse(age %in% c(998, 999), NA, age))
  }
  # 13. Use age to create age categories and then remove `age` variable.
  df <- df %>%
    dplyr::mutate(agecat = cut(age, breaks = c(0, 25, 45, 65, 1000),
                               labels = c("< 25 years",
                                          "25--44 years",
                                          "45--64 years",
                                          "65 years +"),
                               include.lowest = TRUE, right = FALSE)) %>%
    dplyr::select(-age)
  # 14. Gather all the columns with different drug listings (i.e., `drugres1`,
  # `drugres2`, `drugres3`). Convert from the numeric code listings to
  # drug categories.
  gathered_df <- df %>%
    tidyr::gather(drug_number, drug_type_raw, contains("drugres")) %>%
    dplyr::mutate(drug_type = ifelse(drug_type_raw %in% 100:295,
                                     "Narcotic", NA),
                  drug_type = ifelse(drug_type_raw %in% 300:395,
                                     "Depressant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 400:495,
                                     "Stimulant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 600:695,
                                     "Cannabinoid", drug_type),
                  drug_type = ifelse(drug_type_raw %in% c(500:595, 700:996),
                                     "Other", drug_type),
                  drug_type = ifelse(drug_type_raw == 1,
                                     "None", drug_type),
                  drug_type = factor(drug_type)) %>%
    dplyr::select(-drug_type_raw, -drug_number) %>%
    # 15. Filter out any observations where both alcohol and drug data is missing.
    dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))
  # 16. Create a subset with only individuals with at least one non-missing
  # listing for drugs. (Write a sentence or two for each step in this pipe chain.)
  non_missing_drugs <- gathered_df %>%
    filter(!is.na(drug_type)) %>%
    group_by(unique_id, drug_type) %>%
    summarize(has_drug = TRUE) %>%
    ungroup() %>%
    mutate(row_num = 1:n()) %>%
    spread(drug_type, has_drug, fill = FALSE) %>%
    select(-row_num)
  # 17. Join this back into the full dataset. (Write a sentence or two for each
  # step in this pipe chain.)
  df <- df %>%
    dplyr::select(-contains("drugres")) %>%
    dplyr::full_join(non_missing_drugs, by = "unique_id") %>%
    dplyr::select(-None) %>%
    tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                  Depressant, Narcotic, Other, Stimulant) %>%
    dplyr::mutate(drug_type = factor(drug_type)) %>%
    unique()
  return(df)
}
# 18. Iterate the clean_yearly_person_file function across study years to
# create and save a single dataset.
# Note: map_df() is similar to map(), but it binds elements of the
# list resulting from map() together. To understand this step, try
# running this code with map instead of map_df, check out documentation
# for map and map_df, and look at the map_df() function by typing
# `map_df` in your console.
clean_fars <- purrr::map_df(1999:2010, clean_yearly_person_file)
save(clean_fars, file = "data/clean_fars.RData")

load("C:/Users/bkim7/R/fars_project/data/clean_fars.RData")
dim(clean_fars)
length(unique(clean_fars$unique_id))
summary(clean_fars)

#show measurements of prevalence of positive drug tests
clean_fars %>%
  mutate(year_cat = cut(year, breaks = c(1999, 2002, 2006, 2010),
                        labels = c("1999-2002", "2003-2006",
                                   "2007-2010"),
                        include.lowest = TRUE, right = TRUE)) %>%
  filter(!is.na(sex)) %>%
  group_by(drug_type, sex, year_cat) %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1)) %>%
  select(drug_type, sex, year_cat, perc_positive) %>%
  unite(sex_year_cat, sex, year_cat) %>%
  spread(sex_year_cat, perc_positive) %>%
  knitr::kable(col.names = c("Drug type", "F 1999-2002",
                             "F 2003-2006", "F 2007-2010",
                             "M 1999-2002", "M 2003-2006",
                             "M 2007-2010"))
