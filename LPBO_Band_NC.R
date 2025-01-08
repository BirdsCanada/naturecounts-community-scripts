##January 2025
##Danielle Ethier

#This code is for accessing and summarizing LBPO-BAND data from the NatureCounts database. 
#To access these data, you will need to request permission for cjardine@birdscanada.org 

#Install packages
library(naturecounts)
library(tidyverse)

#You need to use the nc_query_table function to access the dataset

#To preview which data tables are available in the nc_query_tables package, use the following code:
nc_query_table(uername = "YOURUSERNAME") #you will be prompted for a password. 

#You are looking to download the CmmnLpboBand table.
LPBO<-nc_query_table("CmmnLpboBand", username = "dethier")

#Create a srping and fall column
LPBO<-LPBO %>% mutate(season = ifelse(Mo %in% c(4, 5, 6, 7), "spring", ifelse(Mo %in% c(8, 9, 10, 11), "fall", "delete"))) %>% filter(season != "delete")

#Identify recapture birds by identifying Bandnum appearing more than once in the records
recap<-LPBO %>% 
  group_by(Bandnum) %>% 
  filter(n()>1) %>% 
  select(Spcd, Bandnum, day, Mo, Yr)

#make a list of recapture birds that do not share the same species code
recap2<-recap %>% 
  group_by(Bandnum) %>% 
  filter(n_distinct(Spcd)>1) %>% 
  select(Spcd, Bandnum, day, Mo, Yr)

#Make a summary of the number banded each Yr and season, including the number of recaptures
capsum<-LPBO %>% 
  group_by(Yr, season) %>% 
  summarise(n_banded=n(), n_recap=sum(duplicated(Bandnum))) %>% 
  arrange(Yr)

#fix errors and omissions in age and sex scores
LPBO<-LPBO %>% mutate(Age = case_when(
  Alphaage == "U" ~ 0, 
  Alphaage == "HY" ~ 2, 
  Alphaage == "AHY" ~ 1, 
  Alphaage == "SY" ~ 5, 
  Alphaage == "ASY" ~ 6, 
  Alphaage == "L" ~ 4,
  Alphaage == "TY" ~ 7, 
  Alphaage == "ATY" ~ 8
))

LPBO<-LPBO %>% mutate(Alphaage = case_when(
  Age == 0 ~"UNK",
  Age == 2 ~ "HY", 
  Age == 1 ~ "AHY", 
  Age == 4 ~ "L", 
  Age == 5 ~ "SY", 
  Age == 6 ~ "ASY", 
  Age == 7 ~ "TY", 
  Age == 8 ~ "ATY"
))


LPBO <-LPBO %>% mutate(Sex = case_when(
  Alphasex == "U" ~ 0, 
  Alphasex == "F" ~ 5, 
  Alphasex == "M"~ 4
))

LPBO<-LPBO %>% mutate(Alphasex = case_when(
  Sex == 0 ~ "U", 
  Sex == 4 ~ "M", 
  Sex == 5 ~ "F", 
  Sex == 6 ~ "U", 
  Sex == 7 ~ "U"
))

LPBO<-LPBO %>% filter(!(fat %in% c("I", "H", "23", "30.9", "9"))| is.na(fat))


#example of how to filter by a species list
sp.list<-c("WTSP", "SWTH", "OVEN", "REVI", "HETH", "MYWA", "RCKI", "NAWA", "MAWA", "BLPW")
sp.dat<-LPBO %>% filter(Spcd %in% sp.list)

#Make a summary of the number banded each Yr and season, including the number of recaptures
capsum2<-sp.dat %>% 
  group_by(Yr, season) %>% 
  summarise(n_banded=n(), n_recap=sum(duplicated(Bandnum))) %>% 
  arrange(Yr, season)

write.csv(capsum2, "LPBO_Band_NC.csv")

#filter data for Yr >= 1974
plot<-LPBO %>% filter(Yr >= 1974)

plot<-plot %>% 
  group_by(Yr, season) %>% 
  summarise(n_banded=n(), n_recap=sum(duplicated(Bandnum))) %>% 
  arrange(Yr, season)

#Create a ggplot point and line graph of the number of birds banded each year by season
ggplot(plot, aes(x=Yr, y=n_banded, color=season))+
  geom_point()+
  geom_line()+
  labs(title="Number of birds banded each year by season", x="Year", y="Number of birds banded")+
  theme_minimal()

