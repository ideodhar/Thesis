library(tidyverse)
library(data.table)
library(zoo)
library(priceR)
library(ggthemes)
library(ineq)
library(povcalnetR)
        

setwd("D:/Thesis")

cphs_exp_1 <- fread("cphs_con_m.txt", sep = "|")


# filtering for wave 1
cphs_exp <- cphs_exp_1 %>%
  filter(grepl("May", MONTH_SLOT) | 
           grepl("Jun", MONTH_SLOT) | 
           grepl("Jul", MONTH_SLOT) | 
           grepl("Aug", MONTH_SLOT))

#cleanining the data
cphs_exp <- cphs_exp %>%
  filter(TOT_EXP != -99) %>%
  filter(RESPONSE_STATUS == "Accepted")
cphs_exp$YEAR <- str_sub(cphs_exp$MONTH_SLOT, 4)
cphs_exp$YEAR <- as.numeric(cphs_exp$YEAR)
colnames(cphs_exp) <- tolower(colnames(cphs_exp))
cphs_exp$state <- gsub(" ", "_", cphs_exp$state)
cphs_exp$state <- tolower(cphs_exp$state)

# check 
cphs_exp|>
  filter(year == 2021) |>
mutate(
adj_w = hh_nr_for_country_ms *  r_hh_wgt_for_country_ms
)|>
dplyr::pull(adj_w) %>%
  sum()


# Making class of hh_size from continues to discreet 

cphs_exp <- cphs_exp %>%
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

adjusted_values <- numeric(nrow(cphs_exp))

for (i in unique(cphs_exp$year)) {
  rows_to_update <- cphs_exp$year == i
  adjusted_values[rows_to_update] <- adjust_for_inflation(
    cphs_exp$tot_exp[rows_to_update],
    i,
    "IN",
    to_date = 2014,
    inflation_dataframe = inflation_dataframe,
    countries_dataframe = countries_dataframe
  )
}
cphs_exp$real_tot_exp <- adjusted_values

##adj weights 

cphs_exp <- cphs_exp %>%
  group_by(month_slot) %>%
  mutate(
    adj_s = hh_nr_for_state_ms *  r_hh_wgt_for_state_ms,
    adj_c = hh_nr_for_country_ms * r_hh_wgt_for_country_ms,
    s = r_hh_wgt_for_state_ms * hh_nr_for_state_ms, 
    c = r_hh_wgt_for_country_ms * hh_nr_for_country_ms
  )



#calculating HH per capita expenditure

cphs_exp <- cphs_exp %>%
  group_by(year, hh_id) %>%
  mutate(
    hhi_mpce = real_tot_exp/size_group 
  ) 


## calculating mpce 

mpce_dist<- cphs_exp%>%
  group_by(year, district) %>%
  summarise(district_mpce = DescTools::Mean(hhi_mpce))


##ayush
ayush <- cphs_exp|>
  filter(response_status == "Accepted")|>
  filter(month_slot %in%  c( "Sep 2021" ,"Oct 2021" ,
                             "Nov 2021" ,"Dec 2021"))|>
  mutate(
    adj_w = hh_nr_for_country_ms *  r_hh_wgt_for_country_ms
  )|> 
  group_by(state,year)|>
  summarise(
    
    average_exp = DescTools::Mean(
      real_tot_exp,
      weights = adj_w
    )
  )

mpce_state<- cphs_exp%>%
  group_by(year, state) %>%
  summarise(state_mpce = DescTools::Mean(hhi_mpce, weights = adj_s),
            state_mpce_cphs = DescTools::Mean(hhi_mpce, weights = s))

write.csv(mpce_state, "state_mpce.csv")

###headcount

#Create a new column poor based on the poverty thresholds
cphs_exp <- cphs_exp %>%
  mutate(poor = ifelse(region_type == "URBAN" & hhi_mpce <= 1407 | region_type == "RURAL" & hhi_mpce <= 972,
                       1, 0))


#state poverty

poverty_headcount <- cphs_exp %>%
  group_by(year, state) %>%
  summarise(headcount_ratio = DescTools::Mean(poor, weights = adj_s))
           # rural_headcount = DescTools::Mean(poor[region_type == "RURAL"], weights = adj_s[region_type == "RURAL"]),
          #  urban_headcount = DescTools::Mean(poor[region_type == "URBAN"], weights = adj_s[region_type == "URBAN"])) 

fwrite(poverty_headcount, "poverty_state_nw.csv")

poverty_headcount_year <- cphs_exp %>%
  group_by(year) %>%
  summarise(headcount_ratio = DescTools::Mean(poor, weights = adj_c), 
            rural_headcount = DescTools::Mean(poor[region_type == "RURAL"], weights = adj_s[region_type == "RURAL"]),
            urban_headcount = DescTools::Mean(poor[region_type == "URBAN"], weights = adj_s[region_type == "URBAN"]))





