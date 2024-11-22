#Written by: Danielle Ethier
#November 2024

#This code is to access, filter, and output LPBO banding data that are on the I drive
#You must have access to the I drive in order for this code to work

library(readxl)
library(purrr)
library(dplyr)

# Set the path to your directory
path <- "I:/LPBO/Banding Data"

files <- list.files(path, pattern = "^dbo_cmmn_lpbo_band.*\\.xlsx$", full.names = TRUE)

# Read all files into a list
data_list <- map(files, read_excel)

l
# Convert the Colour column to character in all data frames
data_list <- lapply(data_list, function(df) {
  df %>%
    mutate(Colour = as.character(Colour), prob_age = as.character(prob_age))
})

# Combine all data frames into one (if they have the same structure)
combined_data <- bind_rows(data_list)

LPBO2014<-read.csv("Data/lpbo2014.csv")
LPBO2014<-LPBO2014 %>% filter(Yr == 2014) %>% dplyr::select(record_id, Spcd, Yr, Mo, day, Sex, Alphasex, Age, Alphaage, Wingcrd, fat=User2, Wt)

LPBO2015<-read.csv("Data/lpbo2015.csv")
LPBO2015<-LPBO2015 %>% filter(Yr == 2015) %>% dplyr::select(record_id, Spcd, Yr, Mo, day, Sex, Alphasex, Age, Alphaage, Wingcrd, fat=User2, Wt)

combined_data<-combined_data %>% filter(Yr != 2014, Yr !=2015)


#Now you can select the columns of interest and filter for the species of interest
#if the data need to be zero filled, you will want to create the zero-fill matrix first before filtering 
dat<-combined_data %>% select(record_id, Spcd, Yr, Mo, day, Sex, Alphasex, Age, Alphaage, Wingcrd, fat, Wt)
dat<-rbind(dat, LPBO2014)
dat<-rbind(dat, LPBO2015)

dat<-dat %>% filter(Yr !=0)
dat<-dat %>% mutate(season = ifelse(Mo %in% c(4, 5, 6, 7), "spring", ifelse(Mo %in% c(8, 9, 10, 11), "fall", "delete"))) %>% filter(season != "delete")

#example of how to filter by a species list
sp.list<-c("WTSP", "SWTH", "OVEN", "REVI", "HETH", "MYWA", "RCKI", "NAWA", "MAWA", "BLPW")
dat<-dat %>% filter(Spcd %in% sp.list)

#fix errors and omissions in age and sex scores
dat<-dat %>% mutate(Age = case_when(
  Alphaage == "U" ~ 0, 
  Alphaage == "HY" ~ 2, 
  Alphaage == "AHY" ~ 1, 
  Alphaage == "SY" ~ 5, 
  Alphaage == "ASY" ~ 6, 
  Alphaage == "L" ~ 4,
  Alphaage == "TY" ~ 7, 
  Alphaage == "ATY" ~ 8
))

dat<-dat %>% mutate(Alphaage = case_when(
  Age == 0 ~"UNK",
  Age == 2 ~ "HY", 
  Age == 1 ~ "AHY", 
  Age == 4 ~ "L", 
  Age == 5 ~ "SY", 
  Age == 6 ~ "ASY", 
  Age == 7 ~ "TY", 
  Age == 8 ~ "ATY"
))


dat <-dat %>% mutate(Sex = case_when(
  Alphasex == "U" ~ 0, 
  Alphasex == "F" ~ 5, 
  Alphasex == "M"~ 4
))

dat<-dat %>% mutate(Alphasex = case_when(
  Sex == 0 ~ "U", 
  Sex == 4 ~ "M", 
  Sex == 5 ~ "F", 
  Sex == 6 ~ "U", 
  Sex == 7 ~ "U"
))

dat<-dat %>% filter(!(fat %in% c("I", "H", "23", "30.9", "9"))| is.na(fat))


#Now you can write your file to the I drive using a meaningful name 
#write.csv(dat, "I:/LPBO/Banding Data/LPBO_DATE_DESCRIPTION.csv")

write.csv(dat, "LBPO_Yun_21Nov2024.csv")
#read.csv("LBPO_Combined_1963-2022.csv")