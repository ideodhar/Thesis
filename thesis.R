library(tidyverse)
library(data.table)

install.packages("priceR")
library(priceR)

setwd("~/COLLEGE/THESIS")

cphs_exp <- fread("cphs_con_m.txt", sep = "|")

cphs_exp <- cphs_exp %>%
  filter(TOT_EXP != -99)
cphs_exp$YEAR <- str_sub(cphs_exp$MONTH_SLOT, 4)
cphs_exp$YEAR <- as.numeric(cphs_exp$YEAR)
colnames(cphs_exp) <- tolower(colnames(cphs_exp))

cphs_exp <- cphs_exp %>%
  mutate(size_group = case_when(
    size_group == "8-10 Members" ~ 9,
    size_group == "11-15 Members" ~ 13,
    size_group == "> 15 Members" ~ 15,
    TRUE ~ as.numeric(substr(size_group, 1, 1))
  ))

inflation_dataframe <- retrieve_inflation_data("IN")
countries_dataframe <- show_countries()

#adjusting tot_exp
start.time <- Sys.time()
cphs_exp <- cphs_exp %>%
  mutate(
    real_tot_exp = adjust_for_inflation(tot_exp, 
                                        year,  
                                        "IN", 
                                        2014, 
                                        inflation_dataframe = inflation_dataframe, 
                                        countries_dataframe = countries_dataframe)
  )

cphs_exp <- cphs_exp %>%
  group_by(year, hh_id) %>%
  mutate(hh_me = mean(real_tot_exp)) 


cphs_exp <- cphs_exp %>%
  group_by(district, year, hh_id) %>%
  mutate(
    hhi_mpce = hh_me/size_group 
  ) 


cphs_exp <- cphs_exp %>%
  mutate(poor = ifelse(region_type == "URBAN" & hhi_mpce <= 1407 | region_type == "RURAL" & hhi_mpce <= 972,
                       1, 0))

poverty_headcount <- cphs_exp %>%
  group_by(year, district) %>%
  mutate(poor_size = poor * size_group) %>%
  summarise(poor_pop = sum(poor_size),
            tot_pop = sum(size_group),
            headcount_ratio = poor_pop/tot_pop) 

fwrite(poverty_headcount, "poverty_headcount.csv")