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

#Now you can select the columns of interest and filter for the species of interest
#if the data need to be zero filled, you will want to create the zero-fill matrix first before filtering 

dat<-combined_data %>% select(record_id, Spcd, Yr, Mo, day, Sex, Alphasex, Age, Alphaage, Wingcrd, fat, Wt)
dat<-dat %>% filter(Yr !=0)
dat<-dat %>% mutate(season = ifelse(Mo %in% c(4, 5, 6, 7), "spring", ifelse(Mo %in% c(8, 9, 10, 11), "fall", "delete"))) %>% filter(season != "delete")

#Now you can write your file to the I drive using a meaningful name 
write.csv(dat, "I:/LPBO/Banding Data/LPBO_DATE_DESCRIPTION.csv")
