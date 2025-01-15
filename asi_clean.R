library(readr)
library(data.table)
library(dplyr)

blockA_14<- read_delim("D:/Thesis/Data/ASI_raw/blka201314.dat", delim = "\t", col_names = F)[-1, ]
colnames(blockA_14) <- tolower(colnames(blockA_14))
blockA_15 <- read_delim("D:/Thesis/Data/ASI_raw/blka201415.dat", delim = "\t", col_names = F)[-1, ]
colnames(blockA_15) <- tolower(colnames(blockA_15))

blockA14_15 <- rbind(blockA_14, blockA_15)
colnames(blockA14_15) <- c("Year", "Block_code", "DSL", "PSL_No", "Scheme_code_Census_1_Sample_2",
                           "Ind_Code_as_per_Frame_4_digit_Class_of_NIC_2008",
                           "Ind_Code_as_per_Return_5_digit_Sub_class_of_NIC_2008", 
                           "State_Name", "District_code","Sector_Rural_1_Urban_2", "RO_SRO_code",
                           "No_of_units", "Status_of_Unit", "Bonus_in_Rs", "Contribution_to_provident_other_funds_in_Rs",
                           "Workmen_staff_welfare_expenses_in_Rs", "Number_of_working_days_Manufacturing_days", "Number_of_working_days_Non_Manufacturing_days", 
                           "Number_of_working_days_Total", "Total_Cost_of_Production", "Share_of_products_by_products_directly_export", "Multiplier")
blockA14_15$State_Name <- tolower(blockA14_15$State_Name)
blockA14_15$State_Name <- gsub("[[:punct:] ]", "", blockA14_15$State_Name)

State_Name <- c("a_and_n_islands", "andhra_pradesh", "arunachal_pradesh", "assam", "bihar", 
                 "chandigarhut", "chattisgarh", "dadra_and_nagar_haveli", "daman_and_diu", 
                 "delhi", "goa", "gujarat", "haryana", "himachal_pradesh", "jammukashmir", 
                 "jharkhand", "karnataka", "kerala", "madhya_pradesh", "maharashtra", 
                 "manipur", "meghalaya", "mizoram", "nagaland", "orissa", "Pondicherry", "punjab", 
                 "rajasthan", "sikkim", "tamil_nadu", "telangana", "tripura", "uttar_pradesh", 
                 "Uttaranchal", "west_bengal")

state_code <- c("35", "28", "12", "18", "10", "04", "22", "26", "25", "07", "30", "24", "06", "02", "01", "20", "29", "32", "23", "27", 
                 "14", "17", "15", "13", "21", "34", "03","08", "11", "33", "36", "16", "09", "05", "19")

# Creating a dataframe
df_states <- data.frame(State_Name=State_Name, state_code=state_code)

#df_states$State_Name <- gsub(".", "", df_states$State_Name)
df_states$State_Name <- gsub("[_.]", "", df_states$State_Name)
df_states$State_Name <- tolower(df_states$State_Name)

block_mew <- left_join(blockA14_15, df_states, relationship = "many-to-many", by = join_by("State_Name"))

#for status of unit codes 
block_mew$Status_of_Unit <- tolower(block_mew$Status_of_Unit)
block_mew$Status_of_Unit <- gsub(" ", "", block_mew$Status_of_Unit)

block_mew <- block_mew %>%
  filter(Status_of_Unit == "open") %>%
  mutate(Status_of_Unit = case_when(Status_of_Unit=="open" ~ 1))

block_mew$Status_of_Unit <- as.numeric(block_mew$Status_of_Unit)

block_mew <- block_mew %>%
  mutate(State_Name = state_code)

colnames(block_mew)[8] <- "state_code"

block_mew <- block_mew[ ,-23]

setwd("D:/Thesis/Data/ASI_blockA")

fwrite(block_mew, "blka2014_and_2015.txt")

blockC_15 <- read_delim("D:/Thesis/Data/ASI_raw/blkc201415.dat", delim = "\t", col_names = F)[-1,]
blockC_14<- read_delim("D:/Thesis/Data/ASI_raw/blkc201314.dat", delim = "\t", col_names = F)[-1, ]

blockC14_15 <- rbind(blockC_14, blockC_15)
blockC14_15$X4 <- gsub(" ", "_", blockC14_15$X4)
                      
blockC_mew <- blockC14_15 %>%
  filter(X4 == "Plant_and_Machinery") %>%
  mutate(X4 = case_when(X4 == "Plant_and_Machinery" ~ 3))

setwd("D:/Thesis/Data/ASI_blockC")

fwrite(blockC_mew, "blkC2014_and_2015.txt")

