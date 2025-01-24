#Open Door Science - SoCB
#January 24, 2025

#Many of the State of Canada's Birds (SoCB) datasets are available in the NatureCounts database
#These are accessible using the nc_query() function 
#You must have permission from Catherine to access these tables (and a NatureCount username)
#Once you have permission, the following code will give you access to the SoCB tables

library(tidyverse)
library(naturecounts)
tables<-nc_query_table(username="dethier") #replace with your username, you will be promted for a password
View(tables) #to see what tables you have access to

#The groups table was used for the composite analysis that Adam Smith discussed in his talk. 
#You can access it using the following code

SocbTrendGroups<-nc_query_table(username="dethier", "SocbTrendGroups") #replace with your username
#the include column is whether or not the species was included in SoCB
#add the species names to the table
sp.list<-meta_species_codes()
sp.list<-sp.list %>% filter(authority == "BBS") %>% select(species_id2, species_code)

SocbTrendGroups<-SocbTrendGroups %>% left_join(sp.list, by=c("speciesID"="species_id2")) #There are some missing species codes??
