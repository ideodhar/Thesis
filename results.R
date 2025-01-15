library(tidyverse)
library(plm)
library(car)
library(lmtest)
library(MASS)
library(broom)
library(lmtest)
library(sandwich)
library(googlesheets4)
library(modelsummary)

setwd("~/UNDERGRAD/Thesis/")



state_mpce <- read_delim("state_mpce.csv")
state_mpce$state <- tolower(state_mpce$state)
state_mpce$state <- gsub (" ", "_", state_mpce$state)
poverty_data <- read.csv("poverty_state.csv")
poverty_data$state <- gsub(" ", "_", poverty_data$state)
poverty_data$state <- tolower(poverty_data$state)
state_poor <- left_join(state_mpce, poverty_data) %>%
  filter(year < 2020)


control <- read_delim("~/UNDERGRAD/Thesis/Data/soi/controls.txt", delim = "|")
colnames(control) <- tolower(colnames(control))
colnames(control) <- gsub(" ", "_", colnames(control))
colnames(control)[4] <- "state"
control$state <- tolower(control$state)
control$state <- gsub(" ", "_", control$state)
control$date <- as.Date(control$date, format = "%d-%m-%Y")
control$date <- format(control$date, "%Y")
control$date <- as.numeric(control$date)
control$value <- as.numeric(control$value)
colnames(control)[9] <- "year"
control$indicator_description <- tolower(control$indicator_description)
control$indicator_description <- gsub(" ", "_", control$indicator_description )
control_w <- control %>%
  # select(state, year, indicator_description, value) %>%
  group_by(state, year, indicator_description) %>%
  summarise(value = mean(value), .groups = 'drop') %>%
  filter(indicator_description != "Population") %>%
  pivot_wider(names_from = indicator_description,
              values_from = value)

control_w <- control_w %>%
  mutate(state = case_when(state == "nct_of_delhi" ~ "delhi", TRUE ~ state))

population <- read.csv("demography_controls.csv")

population <- population %>%
  mutate(state = case_when(state == "nct_of_delhi" ~ "delhi", TRUE ~ state))


control.1 <- left_join(control_w, population)
colnames(control.1) <- gsub("[:\\-]", "", colnames(control.1))
colnames(control.1) <- gsub(" ", "_", colnames(control.1))
colnames(control.1) <- tolower(colnames(control.1))


asi_msme <- read_delim("asi_msme_data_f.csv")


#regression_asi <- left_join(state_mpce, asi_msme, by = join_by(year, state))
regression_asi <- right_join(state_poor, asi_msme, by = join_by("year", "state"))
regression_asi$year <- as.numeric(regression_asi$year)
regression_asi <- left_join(regression_asi, control.1) [-156:-nrow(regression_asi),] %>%
  as.data.frame(
    
  )

regression_asi <- regression_asi %>%
  group_by(state, year) %>%
  mutate(mse = sum(micro, small),
         sme = sum(medium, small),
         headcount_ratio = headcount_ratio * 100,
         headcount_ratio = case_when(headcount_ratio == 0 ~ 1,  
                                      TRUE ~ headcount_ratio),
         yeild = mean(`yield_pulses` + `yield_cereals` + `yield_foodgrain`)) %>%
 # filter(!state %in% c("sikkim", "tripura", "meghalaya", "chandigarh", "puducherry")) %>%
 #filter(!state %in% c("sikkim", "tripura", "meghalaya", "chhattisgarh", "chandigarh", "puducherry")) %>%
  drop_na()

BIMARU_S <- c("madhya_pradesh", "uttar_pradesh", "bihar", "rajasthan")
SOUTH_S <- c("andhra_pradesh", "karnataka", "kerala","tamil_nadu" )

regression_asi <- regression_asi %>%
mutate(BIMARU = case_when(state %in% BIMARU_S ~ 1,
                          TRUE ~ 0),
       SOUTH = case_when(state %in% SOUTH_S ~ 1, 
                         TRUE ~ 0), 
       WEST = case_when(state %in% c("maharashtra", "gujarat") ~ 1, 
                        TRUE ~ 0),
       pp_msme = msme_count/population)


colnames(regression_asi)[17] <- "banking"
colnames(regression_asi)[15] <- "pc_gdp"

summary(m7 <- plm(state_mpce ~ small +  pc_gdp  +
                    + urban + yield_foodgrain + population ,data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))

format(coeftest(m7, vcov. = vcovHC(m7, type = "HC3"))[, 1], scientific = F)
format(coeftest(m7, vcov. = vcovHC(m7, type = "HC3"))[, 2], scientific = F)


summary(m6 <- plm(headcount_ratio ~ log(sme) +log(micro) + urban  + log(population),
                  data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))
pdwtest(m6, index = c("state", "year"))
robust_se <- vcovHC(m6, type = "HC3", cluster = "group")
coeftest(m6, vcov. = robust_se)


summary(m5 <- plm(headcount_ratio ~ log(mse) +log(medium) + log(pc_gdp) + urban  + log(yield_foodgrain) + log(population),
                  data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))

pdwtest(m5, index = c("state", "year"))
#autocorrelation exists
robust_se <- vcovHC(m5, type = "HC3", cluster = "group")
coeftest(m5, vcov. = robust_se)


                  
summary(m4 <- plm(state_mpce ~ micro + small + medium +  pc_gdp  +
                    + urban + yield_foodgrain + population ,data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))
pdwtest(m4, index = c("state", "year"))

robust_se <- vcovHC(m4, type = "HC3", cluster = "group")
coeftest(m4, vcov. = robust_se)

summary(m3 <- plm(headcount_ratio ~ log(micro) + log(small) + log(medium)
                  + urban
                  + log(population),
                  data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))

pdwtest(m3, index = c("state", "year"))
#autocorrelation exists
robust_se <- vcovHC(m3, type = "HC3", cluster = "group")
coeftest(m3, vcov. = robust_se)



summary(m2 <- plm(state_mpce ~ msme_count  +  pc_gdp + urban + yield_foodgrain + population,
                  data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))
pdwtest(m2, index = c("state", "year"))
robust_se <- vcovHC(m2, type = "HC3", cluster = "group")
coeftest(m2, vcov. = robust_se)

summary(m1 <- plm(headcount_ratio ~ log(micro) + log(small) + log(medium)
                  + urban
                  + log(population),
                  data = regression_asi, model = "within", index = c("state", "year"), effect = "twoways"))
 #serial auto correlation is present. Using robust standard errors.
pdwtest(m1, index = c("state", "year"))
pwtest(m1, type = c("bp"))
robust_se <- vcovHC(m1, type = "HC3", cluster = "group")
coeftest(m1, vcov. = robust_se)
