library(tidyverse)

#read and inspect
surveys= read_csv("data/portal_data_joined.csv")
head(surveys)
summary(surveys)

#Q1: What's the type of column species_id? of hindfoot_length?
#Character, dbl

#Q2: How many rows and columns are in surveys?
#13 you can just run it, or use nrows etc

select(surveys, plot_id, species_id, weight)
select(surveys, plot_id, weight_g=weight)
#Max suggests using assign operator when you mean assign, bec you might run into problems that involve something to do with your environment, which is a bummer

select(surveys, -record_id, -species_id)
filter(surveys, year==1995)
filter(surveys, year==1995, plot_id==7) #this ANDs them together
filter(surveys, month==2 | day==20) #this is OR (go make sure my data work)

#Q3: filter surveys to records collected in November where hindfoot_length is greater than 36.0

filter(surveys, month==11, hindfoot_length>36.0)

#Q4: Fix these errors

filter(surveys,year==1995)
filter(surveys, plot_id==2)

surveys_psw<- surveys %>% 
  filter(year==1995) %>% 
  select(plot_id, weight)

#Q5: Use pipes to subset survbeys to animals collected before 1995 retaqining kust the columns year, sex, and weight

surveys_early<-surveys %>% 
  filter(year<1995) %>% 
  select(year, sex, weight)

surveys %>% 
  mutate(weight_kg=weight/1000)

surveys %>% 
  mutate(weight_kg=weight/1000,
         weight_lb=weight_kg*2.2)

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg=weight/1000,
         weight_lb=weight_kg*2.2) %>% 
  view()

#Q6: Create a new data frame from the surveys data that meets the following criteria: contains only the species_id column and a new column called hindfoot_cm containing the hindfoot_length values (currently in mm) converted to centimeters. In this hindfoot_cm column, there are no NAs and all values are less than 3.

#Hint: think about how the commands should be ordered to produce this data frame!

surveys_6<-surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm=hindfoot_length/10) %>%
  filter(hindfoot_cm<3) %>% 
  select(species_id, hindfoot_cm)
#filter in dplyr will remove NAs when you do the comparison line e.g. (<3)

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight=mean(weight, na.rm=TRUE))

surveys %>% 
  drop_na(weight) %>% #just removes NAs, not NaNs  - we introduced NaNs earlier by attempting a mean on NAs
  group_by(species_id, sex) %>% 
  summarize(mean_weight=mean(weight), 
            min_weight=min(weight),
            .groups="drop") %>% #this last line removed the groups we made earlier! cool. if you didnt do the ungroup the tibble would remain grouped by the first group  
  arrange(desc(mean_weight))

#Q7: How many animals were caught in each plot_type surveyed?
surveys %>% 
  group_by(plot_type) %>% 
  summarize(observations=n())

 # Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). Also add the number of observations (hint: see ?n).
surveys %>% 
  group_by(species_id) %>% 
  filter(!is.na(hindfoot_length))
  summarize(mean_foot=mean(hindfoot_length), 
                           min_foot=min(hindfoot_length), 
                           max_foot=max(hindfoot_length, 
                                        observations=n()))

#Q9: What was the heaviest animal measured in each year? Return the columns year, genus, species_id, and weight.
  


