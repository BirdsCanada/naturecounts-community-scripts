
# Into the tidyverse ------------------------------------------------------


# Preamble ----------------------------------------------------------------

# This script provides a brief tutorial on various data wrangling and 
# visualization tools available in the tidyverse. 


# Load packages -----------------------------------------------------------

library(tidyverse)
library(naturecounts)


# Import data -------------------------------------------------------------

nc_permissions(username = "sample")

dat = nc_data_dl(username = "sample", 
           collections = "BBS50-CAN", 
           info = "science hour",
           region = list(statprov = "SK"),
           years = c(2011, 2021))

# species list
sp_code = search_species() 
# modify the code list so that the crows are aggregated
sp_code = sp_code %>%
  select(species_id, english_name, scientific_name)

# join species names to detections
dat_j = dat %>%
  left_join(sp_code) %>%
  # remove columns with all NA
  select(where(~!all(is.na(.x))))



# Let's zero-fill the data  -----------------------------------------------

dat_f = dat_j %>%
  naturecounts::format_zero_fill(extra_species = c("english_name", 
                                                   "scientific_name"),
                                 extra_event = c("SiteCode", 
                                                 "RouteIdentifier",
                                                 "latitude", "longitude", 
                                                 "survey_year", 
                                                 "survey_month", 
                                                 "survey_day",
                                                 "TimeCollected", 
                                                 "CollectorNumber"))


# Basic data summaries ----------------------------------------------------

# what columns exist in the data?
dat_f %>%
  str()

# how many records?
dat_f %>%
  nrow()

# how many species?
dat_f %>%
  distinct(english_name) %>%
  nrow()

# which species?
sp_list = dat_f %>%
  distinct(english_name)


# Filter the data ---------------------------------------------------------

# very common is to filter by species
ex = dat_f %>% 
  filter(english_name == "Chestnut-collared Longspur")

# or by year
ex = dat_f %>% 
  filter(survey_year == 2015)

# easy to combine filters
ex = dat_f %>% 
  filter(survey_year == 2015,
         english_name == "Chestnut-collared Longspur")


# Selecting columns -------------------------------------------------------

# perhaps we only need a few of the available columns
ex = dat_f %>%
  select(SamplingEventIdentifier, english_name, ObservationCount)


# Summarizing data --------------------------------------------------------

# maybe we don't care about the stop-level data and want to summarize at
# route level
ex = dat_f %>% 
  group_by(RouteIdentifier, survey_year, english_name) %>%
  summarise(TotalCount = sum(ObservationCount),
            MaxCount = max(ObservationCount))


# Creating/modifying a column ----------------------------------------------

# perhaps we want to put the date information in a single column
ex = dat_f %>%
  mutate(survey_date = paste(survey_year, survey_month, survey_day, sep = "-"))

# recall there were two different species codes for American Crow
# we probably want to combine them for analysis
ex = dat_f %>%
  mutate(english_name = if_else(species_id %in% c(13900, 38407), 
                                "American Crow", 
                                english_name),
         scientific_name = if_else(species_id %in% c(13900, 38407), 
                                   "Corvus brachyrhynchos", 
                                   scientific_name)) 

# check that it worked
ex %>%
  distinct(english_name) %>%
  arrange(english_name)
# good

dat_f %>%
  mutate(ObservationCount = as.numeric(ObservationCount))


# Strings! ----------------------------------------------------------------

# many fancy things can be done with character strings, and it tends to get
# very specific to your application, so google/stackexchange is your friend
# let's try a basic application though

# what if we want to filter all the warblers?
# we could supply all the warbler names or codes to the filter function
# but that would be a lot of species to type out
# e.g., dat_f %>% 
#         filter(english_name %in% c("Black-and-white Warbler,
#                                    "Chestnut-sided Warbler, ....))
# or we could just tell it to search for the string "Warbler"
ex = dat_f %>% 
  filter(str_detect(english_name, "Warbler"))

# did it work?
ex %>% 
  distinct(english_name)
# yep

# we also might want to extract a string from a column
# for example, SiteCode contains the stop number at the end of the string
# let's make a new column for stop number
# one way to do this is to take the last two digits of the string
ex = dat_f %>%
  mutate(stop = str_sub(SiteCode, start = -2))
# but the problem is stops can have <2 digits
ex %>% 
  distinct(stop)

# we can do this by taking only the numeric characters in the last two digits
ex = dat_f %>%
  mutate(stop = str_sub(SiteCode, start = -2),
         stop = str_extract(stop, "[[:digit:]]+"))
# let's make sure there are no letters attached to the stop number
ex %>% 
  distinct(stop)
# good


# Long vs wide format -----------------------------------------------------

# the data we are working with are in the long format, which is usually how 
# you want it for analysis, but let's say you wanted to summarize the data
# in wide format for a table

# let's make a table where each row is a year, each column is a species,
# and each cell contains the count per route

ex = dat_f %>%
  # first we summarize the data at the route level
  group_by(RouteIdentifier, survey_year, english_name) %>%
  summarise(TotalCount = sum(ObservationCount, na.rm = T)) %>%
  # now summarize by year
  group_by(survey_year, english_name) %>%
  summarise(mu_count = sum(TotalCount) / length(unique(RouteIdentifier))) %>%
  # now we spread the data
  pivot_wider(names_from = "english_name", 
              values_from = "mu_count")

# what if we want to go back to long format?
ex2 = ex %>%
  pivot_longer(cols = c(2:230),
               names_to = "english_name",
               values_to = "mu_count")



# Let's try a more realistic workflow --------------------------------------

# we want to make a plot showing the trend in average count per route before
# we start our hypothetical analysis


# how many routes have data in each year?
dat_f %>% 
  group_by(survey_year) %>%
  summarise(n_route = length(unique(RouteIdentifier)))

ex = dat_f %>%
  # let's filter to a few key grassland SAR
  filter(english_name %in% c("Baird's Sparrow", "Chestnut-collared Longspur",
                             "Sprague's Pipit")) %>%
  # summarize counts by route
  group_by(RouteIdentifier, survey_year, english_name) %>%
  summarise(TotalCount = sum(ObservationCount)) %>%
  # summarize for each year
  group_by(survey_year, english_name) %>%
  summarise(mu_count = sum(TotalCount) / n())


# Plotting ----------------------------------------------------------------

# we're just scratching the surface here
f_trend = ggplot(ex, 
                 aes(x = survey_year,
                     y = mu_count,
                     col = english_name)) +
  geom_line()
f_trend  

# let's make it prettier
f_trend = ggplot(ex, 
                 aes(x = survey_year,
                     y = mu_count,
                     col = english_name)) +
  # the ggthemes package has some nifty formatting for plots
  ggthemes::theme_tufte() +
  geom_line() +
  # new y axis title 
  ylab("Average count per route") +
  # remove legend and x axis title
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())
f_trend 
ggsave("figures/bbs_trend.pdf",
       f_trend,
       height = 4,
       width = 6)  
  