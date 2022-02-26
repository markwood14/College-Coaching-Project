# Who gets opportunities? Exploring minority coaching performance and outcomes vs. white coaches - how long does it take each to get promoted / fired? How many chances do they get after being let go?
# Give attention to minority coordinators who seem to deserve a shot at HC (be careful using subjective "Race" data for individuals)
# Are minorities equally under-represented among assistants, coordinators, and HCs?
# Are there certain HCs who seem to more readily give minorities promotions / coordinator opportunities?

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

# Read and clean the coaches csv
coach_df <- read.csv("coaches_by_race.csv")
coach_df <- coach_df %>%
  filter(Race != "#N/A") %>%
  group_by(Coach, College, Role) %>%
  mutate(year_start = min(Season),
         year_end = max(Season)) %>%
  select(c(College, Coach, Role, Race, year_start, year_end)) %>%
  distinct()
save(coach_df,file="coach_df.Rda")


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
coach_df %>% group_by(Race) %>% summarise(num_race = n())

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



# filtering to only coaches where we will have before/after data - done
head_coaches_recent <- head_coaches %>% filter(year_start >= 2006)

# Edit head_coach to combine where the same guy is still the head coach but added/dropped coordinator title, etc
head_coaches_recent <- head_coaches_recent %>% 
  group_by(College, Coach) %>%
  mutate(year_start = min(year_start), 
         year_end = min(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)

# creating an empty df that we will use to add rows to throughout - done
head_coach_impact <- data.frame()
# for loop that will create a huge before/after stat dataframe - done
for(i in 1:nrow(head_coaches_recent)){
  # create a vector of years from start to end - done
  
  start_year <- as.integer(head_coaches_recent[i, "year_start"])
  if (start_year<2005){start_year<-2005}
  end_year <- as.integer(head_coaches_recent[i,6])
  years <-start_year:end_year
  
  # pull the team name - done
  team <- toString(head_coaches_recent[i, "College"])
  
  # pull the coach's name - done
  coach <- toString(head_coaches_recent[i, "Coach"])
  # pull the coach's race - done
  race <- toString(head_coaches_recent[i, "Race"])
  
  # get the advanced stats history - done
  
  team_advanced <- data.frame()
  num_years <- length(years)
  
  for(year in years){
    team_advanced <- team_advanced %>% dplyr::bind_rows(
      cfbd_stats_season_advanced(year = year, team = team))
  }
  # get the FPI ratings - done
  
  team_FPI <- data.frame()
  for(year in years){
    fpi_row <- cfbfastR:::espn_ratings_fpi(year=year)%>% filter(name == team) %>% select(fpi) %>%as.double() %>% set_names(c("fpi")) 
    team_FPI <- team_FPI %>% bind_rows(fpi_row)
  }
  
  # join the advanced stats and FPI to the head_coach_impact df, binding new rows - done
  
  for(j in 1:nrow(team_advanced)){
    row_to_add <- bind_cols(c(coach), team_advanced[j,], team_FPI[j,], c(race), c("after"))
    colnames(row_to_add)[1] <- toString("Coach")
    colnames(row_to_add)[83] <- toString("FPI_Rating")
    colnames(row_to_add)[84] <- toString("Race")
    colnames(row_to_add)[85] <- toString("BeforeAfter")
    
    head_coach_impact <- head_coach_impact %>% bind_rows(row_to_add)
  }
  
  
  # create a vector of previous years for comparison, will mark data for these years as "before" - done
  
  previous_years <- (years[1] - 3):(years[1]-1)
  
  # checking to make sure that we have data for the years and adjusting the years vector - done
  if(previous_years[1] < 2005){
    previous_years <-2005:tail(previous_years, 1)
  }
  
  # repeat to get advanced stats and FPI and then join to head_coach_impact- done
  
  # get the before advanced stats history - done
  
  num_years <- length(previous_years)
  team_advanced <- data.frame()
  
  for(year in previous_years){
    team_advanced <- team_advanced %>% dplyr::bind_rows(
      cfbd_stats_season_advanced(year = year, team = team))
  }
  # get the FPI ratings - done
  
  team_FPI <- data.frame()
  for(year in previous_years){
    fpi_row <- cfbfastR:::espn_ratings_fpi(year=year)%>% filter(name == team) %>% select(fpi) %>%as.double() %>% set_names(c("fpi")) 
    team_FPI <- team_FPI %>% bind_rows(fpi_row)
  }
  
  # join the advanced stats and FPI to the head_coach_impact df, binding new rows - done
  
  for(j in 1:nrow(team_advanced)){
    row_to_add <- bind_cols(c(coach), team_advanced[j,], team_FPI[j,], c(race), c("before"))
    colnames(row_to_add)[1] <- toString("Coach")
    colnames(row_to_add)[83] <- toString("FPI_Rating")
    colnames(row_to_add)[84] <- toString("Race")
    colnames(row_to_add)[85] <- toString("BeforeAfter")
    head_coach_impact <- head_coach_impact %>% bind_rows(row_to_add)
  }
  
}

# repeat for offensive coordinators - pull offensive advanced stats and offensive FPI
# repeat for defensive coordinators - pull defensive advanced stats and offensive FPI

# use code from Coaching Analysis to summarise before/after, net change, etc
