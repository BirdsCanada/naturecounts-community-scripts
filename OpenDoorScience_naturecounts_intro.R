# Open Door Science Code
# October 4 2024

# How to access your data using the NatureCounts R Package

# Step 1: Create a NatureCounts account, ask Catherine to give you access to your programs data

# Step 2: Review the Intro Tutorial
#https://birdscanada.github.io/NatureCounts_IntroTutorial/
#If you have never used NatureCounts R, please install the package using the following script

#install.packages("remotes")
#remotes::install_github("BirdsCanada/naturecounts")

# Step 3: Explore your data download option
library(naturecounts)
library(tidyverse)

#view Access Levels for NatureCounts collections
dat<-nc_count() #what libraries do you have access to? Level 5 = Open Access
view(dat)

#explore the meta files
collections<-meta_collections()
view(collections)

#Region and Species filtering 
dat<-nc_count(region=list(statprov="ON"))
view(dat)

dat<-nc_count(species=2510)

search_species("chickadee")
search_species_code("BCCH")
# 14280
search_species("Least bittern")
# 2510

#BMDE Data Download
?nc_data_dl()
#filter options (set up the three maximum)
#field_set = "extended" #Gives you all the BMDE columns (including species common name)

#Other Data Download
nc_query_table()
nc_query_table(username="dethier")

BCCWS_ex<-nc_query_table(table="BmdeExtendBCCWS" , username="dethier")

#Zero-filling requires you download all the data for the time period you are interested in analyzing. 
