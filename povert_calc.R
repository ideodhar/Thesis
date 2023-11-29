library(tidyverse)
library(data.table)
library(zoo)
library(priceR)


rm(list = ls())
setwd("D:/Thesis")

cphs_exp <- fread("cphs_con_m.txt", sep = "|")

cphs_exp <- cphs_exp %>%
  filter(TOT_EXP != -99)

cphs_exp$YEAR <- str_sub(cphs_exp$MONTH_SLOT, 4)
cphs_exp$YEAR <- as.numeric(cphs_exp$YEAR)

month <- cphs_exp %>%
group_by(YEAR, HH_ID) %>%
summarise(hh_count = n())

# sample

small_trial <- sample_n(cphs_exp, 100000)

#making a year column 
small_trial$YEAR <- str_sub(small_trial$MONTH_SLOT, 4)
small_trial$YEAR <- as.numeric(small_trial$YEAR)
#colnames(small_trial)[8] <- "year"
colnames(small_trial) <- tolower(colnames(small_trial))



#size of the household

small_trial <- small_trial %>%
  mutate(size_group = case_when(
    size_group == "8-10 Members" ~ 9,
    size_group == "11-15 Members" ~ 13,
    size_group == "> 15 Members" ~ 15,
    TRUE ~ as.numeric(substr(size_group, 1, 1))
  ))

### adjusting for inflation 

#getting the inflation data
inflation_dataframe <- retrieve_inflation_data("IN")
countries_dataframe <- show_countries()

#adjusting tot_exp
small_trial <- small_trial %>%
  mutate(
  real_tot_exp = adjust_for_inflation(tot_exp, 
                                      year, 
                                      "IN", 
                                      2014, 
                                      inflation_dataframe = inflation_dataframe, 
                                      countries_dataframe = countries_dataframe)
)


### poverty calculation 
small_trial <- small_trial %>%
  group_by(year, hh_id) %>%
  mutate(hh_me = mean(real_tot_exp)) 

## test 

hh_pre_count <- small_trial %>%
  group_by(hh_id, year) %>%
  summarise(count = n())

hh_pre_count <- hh_pre_count %>%
  filter(count > 1)

hh_id_dont_match <- small_trial[small_trial$real_tot_exp!=small_trial$hh_me,] 

hh_post_count <- hh_id_dont_match %>%
  group_by(hh_id, year) %>%
  summarise(count = n()) %>%
  filter(count > 1)

nrow(intersect(hh_post_count, hh_pre_count))/nrow(hh_pre_count)

#calculating HH per capita expenditure

small_trial<- small_trial %>%
  group_by(district, year, hh_id) %>%
  mutate(
    hhi_mpce = hh_me/size_group 
  ) 

# Create a new column 'dummy_POOR' based on the poverty thresholds
small_trial <- small_trial %>%
  mutate(poor = ifelse(region_type == "URBAN" & hhi_per_capita <= 1407 | region_type == "RURAL" & hhi_per_capita <= 972,
                       1, 0))


# Multiply 'dummy_POOR' with 'size_group' to get the number of poor people
 small_trial %>%
  group_by(year, district) %>%
   summarize(tot_pop = sum(size_group),
             poor_pop = poor * size_group,
             headcount_ratio = poor_pop/tot_pop) %>%
   view()
 





