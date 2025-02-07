
# Preamble ----------------------------------------------------------------

# Loops are are used when we want a command or set of commands repeated 
# many times. The most basic kinds of loops in R are "for" and "while" 
# loops, and we will focus on their use here. Note that in general these 
# loops are not usually the most efficient way of completing a task,
# especially with large datasets. We will also look at a couple useful 
# alternatives to loops. 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(sf)
library(lubridate)
library(naturecounts)
library(mgcv)
library(DHARMa)


# Import data -------------------------------------------------------------

dat = nc_data_dl(username = "insert_your_username", 
                 collections = "BBS50-CAN", 
                 info = "science hour",
                 region = list(statprov = "SK"),
                 years = c(2011, 2021))


# Data prep ---------------------------------------------------------------

# species list
sp_code = search_species() 
# modify the code list so that the crows are aggregated
sp_code = sp_code %>% select(species_id, english_name, scientific_name)
# join species names to detections
dat_j = dat %>%
  left_join(sp_code) %>%
  # remove columns with all NA
  select(where(~!all(is.na(.x))))
# zero fill
dat_f = dat_j %>%
  naturecounts::format_zero_fill(extra_species = c("english_name", "scientific_name"),
                                 extra_event = c("SiteCode", "RouteIdentifier",
                                                 "latitude", "longitude", 
                                                 "survey_year", "survey_month", 
                                                 "survey_day", "TimeCollected", 
                                                 "CollectorNumber"))


# For loops ---------------------------------------------------------------

# For loops execute a command a set number of times as defined by the user. 
# Here's a super basic example:
for (i in 1:5) {
  # i is an index variable that will be assigned the values after the "in"
  # operator. In the first iteration i = 1, in the second i = 2, etc.
  print(i)
  # in this case the only command we are executing is to print i
}

# i does not have to start at 1
for (i in 5:10) {
  print(i)
}
for (i in -1:4) {
  print(i)
}

# it does not even have to be a list of numbers
sp = unique(dat_f$english)
for (i in sp) {
  print(i)
}
# the same principle can apply to rows of a dataframe, or elements of a list 
# or vector

# when NOT to use a loop:
# on rows of a very large dataframe
# for example, let's concatenate a date column from survey year, month and day
for (i in 1:nrow(dat_f)) {
  dat_f$date[i] = make_date(dat_f$survey_year[i], dat_f$survey_month[i], dat_f$survey_day[i])
}
# runs forever
# there are much better vectorized ways of doing this that we already know: dplyr
dat_f = dat_f %>%
  mutate(date = make_date(survey_year, survey_month, survey_day))

# more realistic example of where loops might be useful
# What if we wanted to produce an exploratory trend plots for each of a 
# select list of species?
# code from my original tidyverse intro
ex = dat_f %>%
  filter(english_name %in% c("Baird's Sparrow", "Chestnut-collared Longspur", "Sprague's Pipit")) %>%
  group_by(RouteIdentifier, survey_year, english_name) %>%
  summarise(TotalCount = sum(ObservationCount)) %>%
  group_by(survey_year, english_name) %>%
  summarise(mu_count = sum(TotalCount) / n())
# plot
f_trend = ggplot(ex, aes(x = survey_year, y = mu_count, col = english_name)) +
  ggthemes::theme_tufte() +
  geom_line() +
  ylab("Average count per route") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())
f_trend


# but what if we wanted a separate plot for each species without going 
# through the trouble of coding each one?
sp = c("Baird's Sparrow", "Chestnut-collared Longspur", "Sprague's Pipit")
for (i in sp) {
  f_trend = ggplot(ex %>% filter(english_name == i), 
                   aes(x = survey_year, y = mu_count)) +
    ggthemes::theme_tufte() +
    geom_line() +
    ylab("Average count per route") +
    ggtitle(i) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) 
  print(f_trend)
}
# and of course we could save all of those plots too
for (i in sp) {
  f_trend = ggplot(ex %>% filter(english_name == i), 
                   aes(x = survey_year, y = mu_count)) +
    ggthemes::theme_tufte() +
    geom_line() +
    ylab("Average count per route") +
    ggtitle(i) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) 
  ggsave(paste0("figures/bbs_trend_", i, ".pdf"),
         f_trend,
         height = 4,
         width = 6) 
}

# another useful application of a for loop is when you have to import/export many 
# files at once. 
# For example, what if we wanted to write individual dataframes for our three 
# grassland SAR as csv 
# first, split our dataframe
dat_ls = dat_f %>%
  filter(english_name %in% sp) %>%
  group_by(english_name) %>%
  group_split()
# loop over each element of the list of dataframes and write to csv
for (i in 1:length(dat_ls)) {
  write_csv(dat_ls[[i]], paste0("data/bbs/bbs_", sp[i], ".csv"))
  # when we refer to an element of a list, we use double square brackets
  # when we refer to a row of a dataframe or an element of a vector, we use 
  # single square brackets
}

# and of course we can do the opposite and import many files from the same 
# directory
fnames = list.files(path = "data/bbs", 
                    pattern = ".csv$", recursive = F, full.names = T) 
# empty list for the new files
dat_ls = list()
for (i in 1:length(fnames)) {
  dat_ls[[i]] = read_csv(fnames[i])
}
dat_ex = bind_rows(dat_ls)

# you could use the same technique to import shapefiles, rasters, or any 
# other data type


# what about running models for multiple species from one dataset?
# summarise the dataset at the route level
dat_r = dat_f %>%
  filter(english_name %in% c("Baird's Sparrow", "Chestnut-collared Longspur", "Sprague's Pipit")) %>%
  group_by(RouteIdentifier, survey_year, english_name) %>%
  summarise(TotalCount = sum(ObservationCount)) %>%
  group_by(english_name) %>%
  group_split()

# empty list for the models
mod_ls = list()
for (i in 1:length(dat_r)) {
  mod_ls[[i]] = gam(TotalCount ~ s(survey_year, k = 5), data = dat_r[[i]], family = poisson())
  print(summary(mod_ls[[i]]))
  plot(mod_ls[[i]], shade = T)
}


# While loops -------------------------------------------------------------

# While loops are slightly different in that they will execute a command 
# until a condition is met. They are used in cases where the number of 
# iterations is unknown. While loops usually have fewer alternatives to 
# their use than for loops. I don't use them very often, but they are good 
# to be familiar with.

# this is a silly example that does the same thing as the basic for loops above
i = 0
while (i <= 5) {
  # as long as i is less than or equal to 5, the loop will continue to run
  print(i)
  i = i + 1
  # add one to i, or the loop will run forever
}

# randomly draw detections until there's 10 of each grassland SAR
i = 0
# i is the number of detections of each species
while (any(i != 10)) {
  dat_s = dat_f %>%
    filter(english_name %in% sp, 
           ObservationCount > 0) %>%
    slice_sample(n = 30)
  i = dat_s %>%
    group_by(english_name) %>%
    summarise(n = n()) %>%
    pull(n)
}
# works, but more complicated than it has to be
dat_s = dat_f %>%
  filter(english_name %in% sp, 
         ObservationCount > 0) %>%
  group_by(english_name) %>%
  slice_sample(n = 10)
# group_by followed by slice does the same thing 

# but there are other cases where while is difficult to replace. One 
# example that was very useful to me was when designing a new monitoring
# program for the Canadian Prairies. I wanted to draw spatially-balanced
# random grids over a large area, but also wanted them to be balanced by
# land cover. I used while to repeat the draw until the grids were a good 
# representation of the landscape. If you're interested in that code, shoot
# me an email.


# For loop alternatives ---------------------------------------------------

# The apply family of functions are basically wrappers around loops. Many 
# of the above examples of iterating over lists could be accomplished with
# lapply, which is usually how I do it. Let's redo a couple examples.

# species trend plots
ex_ls = ex %>%
  group_by(english_name) %>%
  group_split()
lapply(ex_ls, function(x) {
  ggplot(x, aes(x = survey_year, y = mu_count)) +
    ggthemes::theme_tufte() +
    geom_line() +
    ylab("Average count per route") +
    ggtitle(first(x$english_name)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) 
}
)

# bulk read files
fnames = list.files(path = "data/bbs", pattern = ".csv$", recursive = F, full.names = T) 
dat_ex = lapply(fnames, read_csv)
dat_ex = bind_rows(dat_ls)


# the purrr library has some neat tools that can also replace loops
# Here's an example of a workflow for modelling a list of species
mod_res = dat_f %>%
  # using our list of three SAR
  filter(english_name %in% sp) %>%
  group_by(RouteIdentifier, survey_year, english_name) %>%
  # calcalate total count for each route
  summarise(TotalCount = sum(ObservationCount)) %>%
  group_by(english_name) %>%
  # nest the data frame (dataframe for each species within a larger dataframe)
  nest() %>%
  # apply model to each nested dataframe using map()
  mutate(mod = map(.x = data, .f = function(x) {
    gam(TotalCount ~ s(survey_year, k = 5), data = x, family = poisson())
  }))
# scaled model residuals from the DHARMa package
mod_res = mod_res %>%
  mutate(scaled_resids = map(mod, simulateResiduals, plot = F, re.form = NULL))
# let's look at one type of diagnostic plot - for the dispersion test
lapply(mod_res$scaled_resids, testDispersion)
# all three species show overdispersion - much more digging to be done here

# model predictions
mod_res = mod_res %>%
  mutate(fit = map(mod, predict, 
                   newdata = data.frame(survey_year = seq(2011, 2021, 1)),
                   type = "link", se.fit = T),
         fit = map(fit, function(x) {
           data.frame(t(bind_rows(x))) %>%
             rename(fit = 1, se = 2)
         }),
         pred = map(.x = data, .f = function(x) {
           data.frame(survey_year = seq(min(x$survey_year), max(x$survey_year), 1))
           }))
# unnest with just the species name, prediction data, and predictions
mp = mod_res %>%
  select(english_name, pred, fit) %>%
  unnest() %>%
  mutate(ci2.5 = exp(fit - 1.96 * se),
         ci97.5 = exp(fit + 1.96 * se),
         fit = exp(fit))
# make a nice plot
f_density_mp = ggplot(mp, aes(x = survey_year, y = fit, col = english_name, fill = english_name)) +
  theme_tufte() +
  geom_line() +
  geom_ribbon(aes(ymin = ci2.5, ymax = ci97.5), alpha = 0.25, col = NA) +
  ylim(0, 6) +
  ylab("Predicted count / route") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())
f_density_mp         
