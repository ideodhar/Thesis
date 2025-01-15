library(tidyverse)
library(DescTools)

setwd("D:/Thesis/")

files.asi.e <- list.files("D:/Thesis/Data/ASI_blockE")

asi_e <- files.asi.e %>%
  map_df(~read_delim(paste0("D:/Thesis/Data/ASI_blockE/",.), col_names = F))
columns <- c("year", "block", "dsl", "n_no", 
             "mandays_worked_manu", "mandays_worked_nonman", "mandays_worked_total", 
             "employment", "no_mandays_paid", "wages")

names(asi_e) <- columns

add_prefix_numeric <- function(year) {
  if (year < 100) { # Assuming 2-digit years are less than 100
    return(as.numeric(paste0("20", year)))
  } else {
    return(year)
  }
}

asi_e <- asi_e[asi_e$n_no==9,]

asi_e$year <- sapply(asi_e$year, add_prefix_numeric)
asi_e <- asi_e %>%
  dplyr::select(year, dsl, employment, wages)

msme <- read.csv("msme_classification_asi.csv")
msme_employment <- left_join(msme, asi_e) %>% drop_na()

msme_employment <- msme_employment %>% 
  group_by(year, state) %>% 
  filter(employment != 0) %>%
  filter(wages != 0) %>%
  mutate(wage_pc = as.numeric(wages/employment))%>%
  as.data.frame()

msme_wage_employment <- msme_employment %>%
  group_by(year, state) %>%
  # Step 1: Create categorized columns for employment and wages using case_when
  mutate(employment_micro = case_when(cat == "micro" ~ employment, TRUE ~ NA_real_),
         wages_micro = case_when(cat == "micro" ~ wage_pc, TRUE ~ NA_real_),
         employment_small = case_when(cat == "small" ~ employment, TRUE ~ NA_real_),
         wages_small = case_when(cat == "small" ~ wage_pc, TRUE ~ NA_real_),
         employment_medium = case_when(cat == "medium" ~ employment, TRUE ~ NA_real_),
         wages_medium = case_when(cat == "medium" ~ wage_pc, TRUE ~ NA_real_)) %>%
  # Step 2: Summarize with weighted.mean for each category, handling NAs
  summarise(employment = round(DescTools::Mean(employment, na.rm = TRUE, weight = E)),
            wages = round(weighted.mean(wage_pc, w = E, na.rm = TRUE)),
            employment_micro = round(weighted.mean(employment_micro, w = E, na.rm = TRUE)),
            wages_micro = round(weighted.mean(wages_micro, w = E, na.rm = TRUE)),
            employment_small = round(weighted.mean(employment_small, w = E, na.rm = TRUE)),
            wages_small = round(weighted.mean(wages_small, w = E, na.rm = TRUE)),
            employment_medium = round(weighted.mean(employment_medium, w = E, na.rm = TRUE)),
            wages_medium = round(weighted.mean(wages_medium, w = E, na.rm = TRUE)))
          


write.csv(msme_wage_employment, "msme_wage_employment.csv")
