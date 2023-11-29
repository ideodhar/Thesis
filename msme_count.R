library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(broom)


setwd("D:/Thesis")

# reading the files from prowess 
prowess_key <- read_delim("Data/prowess/key_sheet.txt") 

prowess_fin <- read_delim("Data/prowess/finance_sheet.txt")

#making the columns lower case
colnames(prowess_key) <- tolower(colnames(prowess_key))
colnames(prowess_fin) <- tolower(colnames(prowess_fin))

#converting the year column as date 
prowess_fin$year <- as.Date(prowess_fin$year, "%d-%m-%Y")
prowess_fin$year <- as.numeric(format(prowess_fin$year, "%Y"))

# classifying firms into MSME
msme <- prowess_fin%>%
  filter(sales <= 2500) %>%
  mutate(cat = case_when(sales <= 50 ~ "micro", 
                         sales <= 500 & sales >= 50  ~ "small",
                         sales <= 2500 & sales >=500 ~ "medium")) 


#join it with the address data
msme <- msme %>%
  left_join(prowess_key, by =join_by(`company name`)) 

colnames(msme)[16] <- "Pincode"

## Mapping the addresses with pincodes 
# reading the pincode

pin_data <- read.csv("D:/Thesis/Data/Locality_village_pincode_final_mar-2017.csv") %>%
  select(Pincode, Districtname, StateName)

msme_dis_state<- left_join(msme, pin_data, relationship = "many-to-many")

#distict

msme_dis_state %>%
  group_by(year, Districtname) %>%
  #group_by(year)%>%
  summarise(
    micro = sum(case_when(cat == "micro" ~ 1, TRUE ~ 0)),
    small = sum(case_when(cat == "small" ~ 1, TRUE ~ 0)),
    medium = sum(case_when(cat == "medium" ~ 1, TRUE ~ 0)),
    msme_count = sum(micro, medium, small)
  ) %>%
 filter(year >= 2014)-> district_msme_count

colnames(district_msme_count)[2] <- "district"


fwrite(district_msme_count, "district_msme.csv")

msme_dis_state %>%
  group_by(year, StateName) %>%
  summarise(
    micro = sum(case_when(cat == "micro" ~ 1, TRUE ~ 0)),
    small = sum(case_when(cat == "small" ~ 1, TRUE ~ 0)),
    medium = sum(case_when(cat == "medium" ~ 1, TRUE ~ 0)),
    msme_count = sum(micro, medium, small)
  ) %>%
  filter(year >= 2014) -> state_msme_count 

colnames(state_msme_count)[2] <- "state"

fwrite(state_msme_count, "state_msme.csv")



