#Fars_Project Script

library(foreign)
getwd()

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
##Three figures
#create dataset without alcohol called clean_fars2
library(tidyverse)

clean_fars2 <- clean_fars %>%
  group_by(drug_type, year, agecat) %>%
  filter(drug_type != "Alcohol") %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1))
  
save(clean_fars2, file = "data/clean_fars2.RData")

#Figure 1 still need to set theme
#Total percent positive for non-alcohol drugs for each year grouped by agecat
fars_plot1 <- clean_fars %>%
  filter(as.character(drug_type) != "Alcohol") %>%
  filter(!is.na(agecat)) %>%
  group_by(year, agecat, unique_id) %>%
  summarize(positive_test = any(positive_for_drug, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, agecat) %>%
  summarize(perc_positive = mean(positive_test) * 100)

ggplot(fars_plot1, aes(x = year, y = perc_positive, shape = agecat)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 35)) +
  theme_classic() +
  labs(x = "Year", y = "Positive for Nonalcohol Drugs, %", shape = "Age") 

#Figure 2 still need to set theme
#Total percent positive for non-alcohol drugs for each year grouped by drug_type
fars_plot2 <- clean_fars %>%
  filter(as.character(drug_type) != "Alcohol") %>%
  filter(!is.na(agecat)) %>%
  group_by(year, drug_type) %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1))

ggplot(fars_plot2, aes(x = year, y = perc_positive, shape = drug_type)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x = "Year", y = "Positive for Drugs, %", shape = "Drug type") 

#Figure 3 still need to set theme
#Percent positive for cannabinoid for each year grouped by agecat
fars_plot3 <- clean_fars %>%
  filter(as.character(drug_type) %in% "Cannabinoid",
         !is.na(agecat)) %>%
  group_by(year, agecat) %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1))

ggplot(fars_plot3, aes(x = year, y = perc_positive, shape = agecat)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x = "Year", y = "Positive for Cannabinoid, %", shape = "Age") 




