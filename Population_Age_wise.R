library(dplyr)
library(data.table)
library(tidyr)
library(writexl)

L2S3 <- fread("D:/*****/*****/LEVEL - 02 (Section 3).txt")
L4T2 <- fread("D:/Sch_edu/HCES/Demographic and other particulars of household members - Block 4  - Level 4 - Type 2 - 68(2011-2012).txt")
View(L2S3)

free_meal <- L2S3 %>%
  mutate(
    Sector = case_when(
      V4 == 1 ~ "Rural",
      V4 == 2 ~ "Urban"
    ),
    final_weight = V36/100,
    Age_Group = case_when(
      V22 >= 0 & V22 <= 5 ~ "Age 0-5",
      V22 >=6 & V22<=15 ~ "Age 6-15",
      TRUE ~ NA_character_
    ),
    State = V5,
  ) %>%
  filter(!is.na(Age_Group))


state_level <- free_meal %>%
  group_by(State, Age_Group, Sector) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop")%>%
  mutate(State = as.character(State))


india_level <- free_meal %>%
  group_by(Age_Group, Sector) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(State = "India")  


free_meal_bind <- bind_rows(state_level, india_level)


combined_wide <- free_meal_bind %>%
  pivot_wider(
    names_from = Age_Group,
    values_from = Population,
    values_fill = 0
  ) %>%
  mutate(State = as.character(State)) %>%  
  arrange(State)


combined_wide <- combined_wide %>%
  mutate(State_Name = case_when(
    State == "India" ~ "India",
    State == "1" ~ "Jammu & Kashmir",
    State == "2" ~ "Himachal Pradesh",
    State == "3" ~ "Punjab",
    State == "4" ~ "Chandigarh",
    State == "5" ~ "Uttarakhand",
    State == "6" ~ "Haryana",
    State == "7" ~ "Delhi",
    State == "8" ~ "Rajasthan",
    State == "9" ~ "Uttar Pradesh",
    State == "10" ~ "Bihar",
    State == "11" ~ "Sikkim",
    State == "12" ~ "Arunachal Pradesh",
    State == "13" ~ "Nagaland",
    State == "14" ~ "Manipur",
    State == "15" ~ "Mizoram",
    State == "16" ~ "Tripura",
    State == "17" ~ "Meghalaya",
    State == "18" ~ "Assam",
    State == "19" ~ "West Bengal",
    State == "20" ~ "Jharkhand",
    State == "21" ~ "Odisha",
    State == "22" ~ "Chhattisgarh",
    State == "23" ~ "Madhya Pradesh",
    State == "24" ~ "Gujarat",
    State == "25" ~ "Daman & Diu, Dadra & Nagar Haveli",
    State == "27" ~ "Maharashtra",
    State == "28" ~ "Andhra Pradesh",
    State == "29" ~ "Karnataka",
    State == "30" ~ "Goa",
    State == "31" ~ "Lakshadweep",
    State == "32" ~ "Kerala",
    State == "33" ~ "Tamil Nadu",
    State == "34" ~ "Puducherry",
    State == "35" ~ "Andaman & Nicobar Islands",
    State == "36" ~ "Telangana",
    State == "37" ~ "Ladakh",
    TRUE ~ "Unknown"
  ))


free_meal_all <- combined_wide %>%
  select(State, State_Name, Sector, everything())


# 2011-2012 =====================================================================

free_meal1 <- L4T2 %>%
  mutate(
    Sector = case_when(
      V6 == 1 ~ "Rural",
      V6 == 2 ~ "Urban"
    ),
    final_weight = (V34/100)*(V32/V33),
    Age_Group = case_when(
      V22 >= 0 & V22 <= 5 ~ "Age 0-5",
      V22 >= 6 & V22 <= 15 ~ "Age 6-15",
      #Column20 >= 14 & Column20 <= 15 ~ "14-15",
      #Column20 >= 16 & Column20 <= 17 ~ "16-17",
      TRUE ~ NA_character_
    ),
    State = V36,
  ) %>%
  filter(!is.na(Age_Group))
View(free_meal1)

state_level1 <- free_meal1 %>%
  group_by(State, Age_Group, Sector) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop")%>%
  mutate(State = as.character(State))

india_level1 <- free_meal1 %>%
  group_by(Age_Group, Sector) %>%
  summarise(Population = sum(final_weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(State = "India")

free_meal1_bind <- bind_rows(state_level1,india_level1)

combined_wide1 <- free_meal1_bind %>%
  pivot_wider(
    names_from = Age_Group,
    values_from = Population,
    values_fill = 0
  ) %>%
  mutate(State = as.character(State)) %>%  
  arrange(State)

combined_wide1 <- combined_wide1 %>%
  mutate(State_Name = case_when(
    State == "India" ~ "India",
    State == "1" ~ "Jammu & Kashmir",
    State == "2" ~ "Himachal Pradesh",
    State == "3" ~ "Punjab",
    State == "4" ~ "Chandigarh",
    State == "5" ~ "Uttarakhand",
    State == "6" ~ "Haryana",
    State == "7" ~ "Delhi",
    State == "8" ~ "Rajasthan",
    State == "9" ~ "Uttar Pradesh",
    State == "10" ~ "Bihar",
    State == "11" ~ "Sikkim",
    State == "12" ~ "Arunachal Pradesh",
    State == "13" ~ "Nagaland",
    State == "14" ~ "Manipur",
    State == "15" ~ "Mizoram",
    State == "16" ~ "Tripura",
    State == "17" ~ "Meghalaya",
    State == "18" ~ "Assam",
    State == "19" ~ "West Bengal",
    State == "20" ~ "Jharkhand",
    State == "21" ~ "Odisha",
    State == "22" ~ "Chhattisgarh",
    State == "23" ~ "Madhya Pradesh",
    State == "24" ~ "Gujarat",
    State == "25" ~ "Daman & Diu, Dadra & Nagar Haveli",
    State == "27" ~ "Maharashtra",
    State == "28" ~ "Andhra Pradesh",
    State == "29" ~ "Karnataka",
    State == "30" ~ "Goa",
    State == "31" ~ "Lakshadweep",
    State == "32" ~ "Kerala",
    State == "33" ~ "Tamil Nadu",
    State == "34" ~ "Puducherry",
    State == "35" ~ "Andaman & Nicobar Islands",
    State == "36" ~ "Telangana",
    State == "37" ~ "Ladakh",
    TRUE ~ "Unknown"
  ))

free_meal1_all <- combined_wide1 %>%
  select(State, State_Name, Sector, everything())




