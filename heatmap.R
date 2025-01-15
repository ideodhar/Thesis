library(ggplot2)
library(sf)
library(raster)
library(httr)
library(dplyr)
library(tidyverse)

##getting the shapefile
#old_shp <- st_read("C:/Users/DELL/Downloads/India Shapefile With Kashmir/India Shape/india_st.shp")

setwd("D:/Thesis")


india_map <- st_read("D:/Thesis/Heatmap/India_State_Boundary.shp")
india_map$State_Name <- toupper(india_map$State_Name)
#india_map$geometry[india_map$State_Name=="JAMMU AND KASHMIR"] <- old_shp$geometry[old_shp$STATE == "JAMMU AND KASHMIR"]

msme_data <- read.csv("asi_msme_data_f.csv")

msme_data$state <- gsub("_", " ", msme_data$state) # Replace underscores with spaces
msme_data$state <- toupper(msme_data$state) 
msme_data <- msme_data %>% 
  mutate(state = case_when(state == "TAMIL NADU" ~ "TAMILNADU",
                           state == "JAMMU & KASHMIR" ~ "JAMMU AND KASHMIR",
                           state == "TELANGANA" ~ "TELENGANA", 
                           state == "CHHATTISGARH" ~ "CHHATTISHGARH",
                           TRUE ~ state))


# Now merge the datasets
# Ensure the state column in msme_data matches the STATE column in india_map
map_data <- merge(india_map, msme_data, by.x = "State_Name", by.y = "state", all.x = TRUE) %>% 
  filter(year == 2019)
  
lad <- india_map %>% filter(State_Name %in% c("LADAKH", "MIZORAM"))

lad$medium <- NA
lad$msme_count <- NA
lad$small <- NA
lad$micro <- NA
lad$year <- 2019
lad$X <- 2
 
map_data <- rbind(map_data, lad)

pdf("msme_plot.pdf")
  ggplot(data = map_data) +
  geom_sf(aes(fill = msme_count), color = NA) +
  geom_sf(color = "darkgray", fill = NA) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent") +
  labs(fill = "MSME Count")    +
  theme(text = element_text(family = "Times New Roman")) +
  theme_void() 
  dev.off()
  
  

  pdf("micro_plot.pdf")
  ggplot(data = map_data) +
    geom_sf(aes(fill = micro), color = NA) +
    geom_sf(color = "darkgray", fill = NA) +
    scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent") +
    labs(fill = "Micro Count") +
    theme(axis.text.x = element_blank(),    # Remove x axis labels
          axis.text.y = element_blank(),    # Remove y axis labels
          axis.ticks = element_blank(),     # Remove all axis ticks
          axis.title.x = element_blank(),   # Remove x axis title
          axis.title.y = element_blank(),   # Remove y axis title
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line = element_blank())   +
    theme_void()
  dev.off()
  
  pdf("small_plot.pdf")
  ggplot(data = map_data) +
    geom_sf(aes(fill = small), color = NA) +
    geom_sf(color = "darkgray", fill = NA) +
    scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent") +
    labs(fill = "Small Count") +
    theme(axis.text.x = element_blank(),    # Remove x axis labels
          axis.text.y = element_blank(),    # Remove y axis labels
          axis.ticks = element_blank(),     # Remove all axis ticks
          axis.title.x = element_blank(),   # Remove x axis title
          axis.title.y = element_blank(),   # Remove y axis title
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line = element_blank())   +
    theme_void()
  dev.off()
  
pdf("medium_plot.pdf")
  ggplot(data = map_data) +
    geom_sf(aes(fill = medium), color = NA) +
    geom_sf(color = "darkgray", fill = NA) +
    scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent") +
    labs(fill = "Medium Count") +
    theme(axis.text.x = element_blank(),    # Remove x axis labels
          axis.text.y = element_blank(),    # Remove y axis labels
          axis.ticks = element_blank(),     # Remove all axis ticks
          axis.title.x = element_blank(),   # Remove x axis title
          axis.title.y = element_blank(),   # Remove y axis title
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line = element_blank())   +
    theme_void()  
  dev.off()
  
  list.files(getwd())
  
  ## 
  state_poor <- read.csv("poverty_state.csv")
  state_poor$state <- gsub("_", " ", state_poor$state) # Replace underscores with spaces
  state_poor$state <- toupper(state_poor$state) 
  state_poor <- state_poor %>% 
    mutate(state = case_when(state == "TAMIL NADU" ~ "TAMILNADU",
                             state == "JAMMU & KASHMIR" ~ "JAMMU AND KASHMIR",
                             state == "TELANGANA" ~ "TELENGANA", 
                             state == "CHHATTISGARH" ~ "CHHATTISHGARH",
                             TRUE ~ state))
  
  map_data_poor <- merge(india_map, state_poor, by.x = "State_Name", by.y = "state", all.x = TRUE) %>% 
    filter(year == 2019) %>%
    dplyr::select(-c("urban_headcount", "rural_headcount"))
  
  lad_p <- india_map %>% filter(State_Name %in% c("LADAKH", "ARUNACHAL PRADESH", "NAGALAND", "MANIPUR", "MIZORAM"))
  
  lad_p$headcount_ratio <- NA
  lad_p$year <- 2019

  
 map_data_poor <-  rbind(map_data_poor, lad_p) 
 
 pdf("poverty_plot.pdf")
 ggplot(data = map_data_poor) +
   geom_sf(aes(fill = headcount_ratio), color = NA) +
   geom_sf(color = "darkgray", fill = NA) +
   scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent") +
   labs(fill = "Poverty") +
   theme(axis.text.x = element_blank(),    # Remove x axis labels
         axis.text.y = element_blank(),    # Remove y axis labels
         axis.ticks = element_blank(),     # Remove all axis ticks
         axis.title.x = element_blank(),   # Remove x axis title
         axis.title.y = element_blank(),   # Remove y axis title
         panel.grid.major = element_blank(), # Remove major grid lines
         panel.grid.minor = element_blank(), # Remove minor grid lines
         axis.line = element_blank())   +
   theme_void()
 dev.off()
 