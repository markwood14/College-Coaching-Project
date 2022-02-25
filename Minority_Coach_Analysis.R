# Who gets opportunities? Exploring minority coaching performance and outcomes vs. white coaches

library(remotes)
library(Rcpp)
library(dplyr)
library(readr)
library(tidyverse)
library(devtools)
library(cfbfastR)
library(gt)
library(ggimage)
library(ggeasy)
library(reshape2)
library(purrr)
library(parallel)
library(future)
library(data.table)
library(stringr)

coordinators <- coach_df %>% filter(str_detect(Role, "Coordinator"))
offensive_coordinators <- coordinators %>% filter(str_detect(Role, "Offensive"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Run Game Coordinator"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Pass Game Coordinator"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Associate offensive"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Special Teams"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Assistant offensive"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Offensive Recruiting Coordinator"))
offensive_coordinators <- offensive_coordinators %>% filter(!str_detect(Role, "Head Coach"))

defensive_coordinators <- coordinators %>% filter(str_detect(Role, "Defensive"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Associate Defensive"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Special Teams"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Assistant Defensive"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Recruiting Coordinator"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Pass Game Coordinator"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Run Game Coordinator"))
defensive_coordinators <- defensive_coordinators %>% filter(!str_detect(Role, "Head Coach"))

head_coaches <- coach_df %>% filter(str_detect(Role, "Head"))
head_coaches <- head_coaches %>% filter(!str_detect(Role, "Associate Head"))
head_coaches <- head_coaches %>% filter(!str_detect(Role, "Assistant Head"))
head_coaches <- head_coaches %>% filter(!str_detect(Role, "Interim Head"))

# High Level Analysis

head_coaches %>% group_by(Race) %>% summarise(num_race = n())
#   Race  num_race
#2 Black       69
#3 Other        8
#4 White      557
defensive_coordinators %>% group_by(Race) %>% summarise(num_race = n())
#  Race  num_race
#2 Black      230
#3 Other       23
#4 White     1094
offensive_coordinators %>% group_by(Race) %>% summarise(num_race = n())
#Race  num_race
#2 Black      121
#3 Other       14
#4 White     1108

# Coordinators who became head coaches

offensive_to_head <- offensive_coordinators %>% inner_join(head_coaches, by = "Coach") 
offensive_to_head <- offensive_to_head%>% distinct(Coach, .keep_all = TRUE)
defensive_to_head <- defensive_coordinators %>% inner_join(head_coaches, by = "Coach")
defensive_to_head <- defensive_to_head%>% distinct(Coach, .keep_all = TRUE)

offensive_to_head %>% group_by(Race.x) %>% summarise(num_race = n())
# Race.x num_race
#2 Black        12
#3 Other         2
#4 White       127

defensive_to_head %>% group_by(Race.x) %>% summarise(num_race = n())
# Race.x num_race
#2 Black        11
#3 Other         3
#4 White       74

head_coach_impact <- data.frame(coaches$Coach)
# Need to edit head_coach to combine where the same guy is still the head coach but added/dropped coordinator title, etc

for(i in 1:nrow(head_coaches)){
  # create a vector of years from start to end
  years <- c(head_coaches[i, "year_start"]:head_coaches[i, "year_end"])
  # provide a check that the start year is 2005 or later so we have data?
  if (years[1] < 2005) {
    years[1] <-2005}
  # this isn't right, years vector won't turn out correctly
  # pull the team name
  team <- head_coaches[i, "College"]
  
  # get the advanced stats history
  
  team_advanced <- data.frame()
 
  for(year in years){
    team_advanced <- team_advanced %>% dplyr::bind_rows(
      cfbd_stats_season_advanced(year = year, team = team))
  }
  
  # join the advanced stats to the head_coach_impact df, join by Coach name?
  
  # get the FPI ratings
  team_FPI <- data.frame()
  for(year in years){
    team_FPI <- team_FPI %>% dplyr::bind_rows(
      espn_ratings_fpi(year = year)%>% filter(name == team) %>% select(fpi))
  }
  # join to the head_coach_impact df, join by coach name?
  # add a column for a before/after tag
  # create a vector of previous years for comparison, mark those as before
  years <- years - 3
  
  if (years[1] < 2005){
    years[1] <-2005}
  # repeat to get advanced stats and FPI and then join to head_coach_impact
  
}

# repeat for offensive coordinators - pull offensive advanced stats and offensive FPI
# repeat for defensive coordinators - pull defensive advanced stats and offensive FPI

# use code from Coaching Analysis to summarise before/after, net change, etc
