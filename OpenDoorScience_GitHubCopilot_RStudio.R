##Jan 2025
##Danielle Ethier

##Getting started with Github Copilot in RStudio

#To use GitHub Copilot, you must have a GitHub account and an active subscription to Copilot for Individuals or Copilot for Business. 
#For more information, see billing for GitHub Copilot.
#To use GitHub Copilot in RStudio, you must have a compatible version of RStudio installed. 
#GitHub Copilot is available for RStudio Desktop 2023.09.0 and later. 
#GitHub Copilot is disabled by default in RStudio Server and Posit Workbench, but can be enabled by an administrator.
#To use GitHub Copilot, you must have access to the internet in order to send requests to the Copilot APIs 
#and receive suggestions from GitHub Copilot.

#To enable GitHub Copilot in RStudio instructions: https://docs.posit.co/ide/user/ide/guide/tools/copilot.html
#Useful video to get you started: https://www.youtube.com/watch?v=h1tfqDfmE2Q 

#To specifically ask a question of Copilot, use a comment with a # q: at the beginning and a question mark at the end. 

#When using Copilot to solve a broad problem, it is a good idea to provide a high-level goal at the beginning of the file. 
#This will help Copilot provide suggestions that are relevant to the specific problem you are trying to solve.

# This script will do x
# using the packages x,y,z
# other constraints or details
# Specify the dataframe field explicitly


# At times, normal autocomplete and Copilot may seem to conflict with each other. 
# If it is, you can accept the suggestion by pressing Tab. 
# If it is not, you can ignore the suggestion and continue typing
# force the normal autocomplete to show by pressing Ctrl+Space. 
# You can also toggle Copilot on and off for a particular document Ctrl+Shift+P.

#Working through an Open Door Science Example

library(naturecounts)
library(tidyverse)


data<-nc_data_dl(collections= "BCCWS", years=c(2010,2020), fields_set = "extended", username = "dethier", info ="Pulling data for Open Door Science")

#select columns of interest for the example
dat<-data %>% select(species_id, SpeciesCode, survey_year, survey_month, survey_day, SiteCode, ObservationCount, DecimalLatitude, DecimalLongitude)
                    
#Remove NA from the dat dataframe
dat<-na.omit(dat)

#create day of year column using Julian dates 
dat$doy<-as.numeric(format(as.Date(paste(dat$survey_year, dat$survey_month, dat$survey_day, sep="-")), "%j"))

# create a winter year variable starting in September and ending in April of the next years 
dat$winter_year<-ifelse(dat$survey_month %in% c(9,10,11,12), dat$survey_year, ifelse(dat$survey_month %in% c(1,2,3,4), dat$survey_year-1, NA))

#filter data from September to April
dat<-dat %>% filter(survey_month %in% c(9,10,11,12,1,2,3,4))

#assign the month to there names (e.g., 1=January, 2=February, etc.). Make this a factor. 
dat$month<-factor(dat$survey_month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

##filter the dat dataframe so that there is a single record per sitecode, month and year, using the slice min
dat<-dat %>% group_by(SiteCode, winter_year, month) %>% slice_min(doy)

#create a data martix for zero filling, using SiteCode, winter_year, and doy, DecemalLatitude, DecimalLongitude
events.matrix<-dat %>% select(SiteCode, winter_year, doy, DecimalLatitude, DecimalLongitude) %>% distinct()

#filter the data for the seaduck of interest from the SpeciesCode column
sp.list<-c("WWSC", "BLSC", "LTDU", "SUSC")

#filter the dat for the sp.list
dat<-dat %>% filter(SpeciesCode %in% sp.list)

#plot the data, starting in September and ending in April of the next years by re-ordering the doy column


# q: what is the difference between using an offset versus using covariate in an analysis when trying to control for effort?



#open LLM in the RStudio interface
library(chattr)
chattr_app()


