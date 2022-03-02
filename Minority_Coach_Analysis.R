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
library(ggpubr)

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

# fixing issue with mismatched school names
head_coaches_recent["College"][head_coaches_recent["College"] == "FAU"] <- "Florida Atlantic"
head_coaches_recent["College"][head_coaches_recent["College"] == "FIU"] <- "Florida International"
head_coaches_recent["College"][head_coaches_recent["College"] == "Hawaii"] <- "Hawai'i"
head_coaches_recent["College"][head_coaches_recent["College"] == "Massachusetts"] <- "UMass"
head_coaches_recent["College"][head_coaches_recent["College"] == "Miami (FL)"] <- "Miami"
head_coaches_recent["College"][head_coaches_recent["College"] == "San Jose State"] <- "San José State"
head_coaches_recent["College"][head_coaches_recent["College"] == "Southern Miss"] <- "Southern Mississippi"
head_coaches_recent["College"][head_coaches_recent["College"] == "UConn"] <- "Connecticut"
head_coaches_recent["College"][head_coaches_recent["College"] == "UL Monroe"] <- "Louisiana Monroe"
head_coaches_recent["College"][head_coaches_recent["College"] == "USF"] <- "South Florida"
head_coaches_recent["College"][head_coaches_recent["College"] == "UTSA"] <- "UT San Antonio"

# Edit head_coach to combine where the same guy is still the head coach but added/dropped coordinator title, etc
head_coaches_recent <- head_coaches_recent %>% 
  group_by(College, Coach) %>%
  mutate(year_start = min(year_start), 
         year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)

head_coaches_recent %>% group_by(Race) %>% summarise(num_race = n())

# creating an empty df that we will use to add rows to throughout - done
head_coach_impact <- data.frame()


# for loop that will create a huge before/after stat dataframe - done

# charlotte did not exist before 2015, so it errored out. starting the loop again with 
# row 66, will healy taking over charlotte
# same error for coastal, starting at row 73 with chadwell
# same error for utsa, starting at row 342 with wilson

for(i in 342:nrow(head_coaches_recent)){
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
    progressr::with_progress({
      future::plan("multisession")
      team_advanced <- team_advanced %>% dplyr::bind_rows(
        cfbd_stats_season_advanced(year = year, team = team))
      
    })
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

save(head_coach_impact, file="head_coach_impact.Rda")

# load("head_coach_impact.Rda")
# use code from Coaching Analysis to summarise before/after, net change, etc
head_coach_impact_summary <- head_coach_impact %>% select(c("Coach", "team", "off_ppa", "off_success_rate", "off_stuff_rate", "off_passing_plays_success_rate",
                                                            "def_ppa", "def_success_rate", "def_stuff_rate", "def_passing_plays_success_rate", "FPI_Rating", "Race", "BeforeAfter"))

head_coach_impact_summary <- head_coach_impact_summary %>% group_by(Coach, team, Race, BeforeAfter) %>% summarise(
  mean_ppa = mean(off_ppa), 
  mean_sr = mean(off_success_rate),
  mean_stuff = mean(off_stuff_rate),
  mean_pass_sr = mean(off_passing_plays_success_rate),
  mean_defppa = mean(def_ppa),
  mean_defsr = mean(def_success_rate),
  mean_defstuff = mean(def_stuff_rate),
  mean_defpasssr = mean(def_passing_plays_success_rate),
  mean_fpi = mean(FPI_Rating)
)

# fixing to account for the three instances with no "before"
head_coach_impact_summary <- head_coach_impact_summary[-c(51,340,403),]

# Calculating the net (offense-defense after-before) impact on PPA, SR, Stuff, Pass SR, FPI

head_coach_impact_results <- data.frame()
i <- 1
while (i < nrow(head_coach_impact_summary)){
  head_coach_results <- data.frame()
  row_to_add <- data.frame()
  head_coach_results <- head_coach_impact_summary[i,] %>% group_by(Coach, team, Race) %>% 
    summarise(net_ppa = head_coach_impact_summary[i,"mean_ppa"]-head_coach_impact_summary[i+1, "mean_ppa"] - head_coach_impact_summary[i,"mean_defppa"]+head_coach_impact_summary[i+1, "mean_defppa"],
              net_sr = head_coach_impact_summary[i,"mean_sr"]-head_coach_impact_summary[i+1, "mean_sr"] - head_coach_impact_summary[i,"mean_defsr"]+head_coach_impact_summary[i+1, "mean_defsr"],
              net_stuff = head_coach_impact_summary[i,"mean_stuff"]-head_coach_impact_summary[i+1, "mean_stuff"] - head_coach_impact_summary[i,"mean_defstuff"]+head_coach_impact_summary[i+1, "mean_defstuff"],
              net_pass_sr = head_coach_impact_summary[i,"mean_pass_sr"]-head_coach_impact_summary[i+1, "mean_pass_sr"] - head_coach_impact_summary[i,"mean_defpasssr"]+head_coach_impact_summary[i+1, "mean_defpasssr"],
              net_fpi = head_coach_impact_summary[i,"mean_fpi"]-head_coach_impact_summary[i+1, "mean_fpi"])
  head_coach_impact_results <- head_coach_impact_results %>% bind_rows(head_coach_results)
  i=i+2
}
colnames(head_coach_impact_results) <- c(toString("Coach"), "Team", "Race", "Net_PPA", "Net_SR", "Net_Stuff_Rate", "Net_Pass_SR", "Net_FPI")

save(head_coach_impact_results, file="head_coach_impact_results.Rda")

# load("head_coach_impact_results.Rda")

# Fixing issue where individual columns are lists
head_coach_impact_results_test <- lapply(head_coach_impact_results, unlist)
head_coach_impact_results_test <- data.frame(lapply(head_coach_impact_results_test, `length<-`, max(lengths(head_coach_impact_results_test))))
head_coach_impact_results <- head_coach_impact_results_test

# doing some preliminary analysis
head_coach_impact_results %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
#   Race  mean_net_ppa_race
#  1 ?              -0.0222 
#2 Black          -0.0292 
#3 Other           0.00167
#4 White           0.0106 

####################################################################################
# check for normality and outliers
hist(head_coach_impact_results$Net_PPA,
     xlab = "Net PPA",
     main = "Histogram of Net PPA")
# data seems to be normality distributed
boxplot(head_coach_impact_results$Net_PPA,
        ylab = "Net PPA",
        main = "Boxplot of Net PPA")
# there are 7 outliers. Let's remove them.
out_vals <- boxplot.stats(head_coach_impact_results$Net_PPA)$out
out_inds <- which(head_coach_impact_results$Net_PPA %in% c(out_vals))
out_inds
# (I didn't want to overwrite your 'head_coach_impact_results' df so I just added the '1' suffix. - MW)
head_coach_impact_results1 <- head_coach_impact_results[-c(out_inds),]
ggqqplot(head_coach_impact_results1$Net_PPA)
head_coach_impact_results1 %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))

# analyze via simple linear regression
lm1 <- lm(Net_PPA ~ Race, head_coach_impact_results1)
summary(lm1)
# no significant variables

# Potential errors/ommissions in the analysis?
# 1. Small sample sizes (only 52 Black and 8 Other)
# 2. How many years were the coaches given to turn the program around? I think the "after" period varies. Do we need to adjust for this? For example, Hugh Freeze 1-year tenure at Arkansas State counts the same as his 5-year tenure at Ole Miss.
# 3. Could a handfull of coaches be skewing the data set? For example, Hugh Freeze is counted twice here as +Net_PPA for "The White Team," whereas Willie Taggart & Darrell Hazell are counted twice as -Net_PPA for "The Black Team."
# 4. "The Vandy Rule": First I was surprised to see James Franklin's Vandy tenure as only the 46th best Net_PPA. What he did there was really impressive. But then Derek Mason was penalized for following him. Derek Mason had the next highest win percentage of a Vandy HC since Gerry DiNardo in '91-'94 (7 total coaches since then). But he has the 25th worst coaching tenure on this list in terms of Net_PPA.
########################################################################################


# repeat for offensive coordinators - pull offensive advanced stats

# data cleaning for OC dataframe

# filtering to only coaches where we will have before/after data - done
oc_recent <- offensive_coordinators %>% filter(year_start >= 2006)

# fixing issue with mismatched school names
oc_recent["College"][oc_recent["College"] == "FAU"] <- "Florida Atlantic"
oc_recent["College"][oc_recent["College"] == "FIU"] <- "Florida International"
oc_recent["College"][oc_recent["College"] == "Hawaii"] <- "Hawai'i"
oc_recent["College"][oc_recent["College"] == "Massachusetts"] <- "UMass"
oc_recent["College"][oc_recent["College"] == "Miami (FL)"] <- "Miami"
oc_recent["College"][oc_recent["College"] == "San Jose State"] <- "San José State"
oc_recent["College"][oc_recent["College"] == "Southern Miss"] <- "Southern Mississippi"
oc_recent["College"][oc_recent["College"] == "UConn"] <- "Connecticut"
oc_recent["College"][oc_recent["College"] == "UL Monroe"] <- "Louisiana Monroe"
oc_recent["College"][oc_recent["College"] == "USF"] <- "South Florida"
oc_recent["College"][oc_recent["College"] == "UTSA"] <- "UT San Antonio"

# Edit oc_recent to combine where the same guy is still the head coach but added/dropped coordinator title, etc
oc_recent <- oc_recent %>% 
  group_by(College, Coach) %>%
  mutate(year_start = min(year_start), 
         year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)

# creating an empty df that we will use to add rows to throughout - done
oc_impact <- data.frame()


# for loop that will create a huge before/after stat dataframe - done

# charlotte did not exist before 2015, so it errored out. starting the loop again with 
# row 135, shane montgomery taking over charlotte
# same error for UTSA, starting again on row 771

for(i in 772:nrow(oc_recent)){
  # create a vector of years from start to end - done
  
  start_year <- as.integer(oc_recent[i, "year_start"])
  if (start_year<2005){start_year<-2005}
  end_year <- as.integer(oc_recent[i,6])
  years <-start_year:end_year
  
  # pull the team name - done
  team <- toString(oc_recent[i, "College"])
  
  # pull the coach's name - done
  coach <- toString(oc_recent[i, "Coach"])
  # pull the coach's race - done
  race <- toString(oc_recent[i, "Race"])
  
  # get the advanced stats history - done
  
  team_advanced <- data.frame()
  num_years <- length(years)
  
  for(year in years){
    progressr::with_progress({
      future::plan("multisession")
      team_advanced <- team_advanced %>% dplyr::bind_rows(
        cfbd_stats_season_advanced(year = year, team = team))
      
    })
  }
  
  # join the advanced stats and FPI to the oc_impact df, binding new rows - done
  
  for(j in 1:nrow(team_advanced)){
    row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("after"))
    colnames(row_to_add)[1] <- toString("Coach")
    colnames(row_to_add)[83] <- toString("Race")
    colnames(row_to_add)[84] <- toString("BeforeAfter")
    
    oc_impact <- oc_impact %>% bind_rows(row_to_add)
  }
  
  
  # create a vector of previous years for comparison, will mark data for these years as "before" - done
  
  previous_years <- (years[1] - 3):(years[1]-1)
  
  # checking to make sure that we have data for the years and adjusting the years vector - done
  if(previous_years[1] < 2005){
    previous_years <-2005:tail(previous_years, 1)
  }
  
  # repeat to get advanced stats and then join to oc_impact- done
  
  # get the before advanced stats history - done
  
  num_years <- length(previous_years)
  team_advanced <- data.frame()
  
  for(year in previous_years){
    team_advanced <- team_advanced %>% dplyr::bind_rows(
      cfbd_stats_season_advanced(year = year, team = team))
  }
  
  # join the advanced stats to the oc_impact df, binding new rows - done
  
  for(j in 1:nrow(team_advanced)){
    row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("before"))
    colnames(row_to_add)[1] <- toString("Coach")
    colnames(row_to_add)[83] <- toString("Race")
    colnames(row_to_add)[84] <- toString("BeforeAfter")
    oc_impact <- oc_impact %>% bind_rows(row_to_add)
  }
  
}

save(oc_impact, file="oc_impact.Rda")

# load("oc_impact.Rda")

# use code from Coaching Analysis to summarise before/after, net change, etc
oc_impact_summary <-oc_impact %>% select(c("Coach", "team", "off_ppa", "off_success_rate", "off_stuff_rate", "off_passing_plays_success_rate",
                                           "Race", "BeforeAfter"))

oc_impact_summary <- oc_impact_summary %>% group_by(Coach, team, Race, BeforeAfter) %>% summarise(
  mean_ppa = mean(off_ppa), 
  mean_sr = mean(off_success_rate),
  mean_stuff = mean(off_stuff_rate),
  mean_pass_sr = mean(off_passing_plays_success_rate),
)

# fixing to account for the three instances with no "before"
oc_impact_summary <- oc_impact_summary[-c(737, 920),]

# Calculating the net (offense-defense after-before) impact on PPA, SR, Stuff, Pass SR, FPI

oc_impact_results <- data.frame()
i <- 1
while (i < nrow(oc_impact_summary)){
  oc_results <- data.frame()
  row_to_add <- data.frame()
  oc_results <- oc_impact_summary[i,] %>% group_by(Coach, team, Race) %>% 
    summarise(net_ppa = oc_impact_summary[i,"mean_ppa"]-oc_impact_summary[i+1, "mean_ppa"],
              net_sr = oc_impact_summary[i,"mean_sr"]-oc_impact_summary[i+1, "mean_sr"],
              net_stuff = oc_impact_summary[i,"mean_stuff"]-oc_impact_summary[i+1, "mean_stuff"],
              net_pass_sr = oc_impact_summary[i,"mean_pass_sr"]-oc_impact_summary[i+1, "mean_pass_sr"])
  oc_impact_results <- oc_impact_results %>% bind_rows(oc_results)
  i=i+2
}

save(oc_impact_results, file="oc_impact_results.Rda")

# load("head_coach_impact_results.Rda")

# Fixing issue where individual columns are lists
oc_impact_results_test <- lapply(oc_impact_results, unlist)
oc_impact_results_test <- data.frame(lapply(oc_impact_results_test, `length<-`, max(lengths(oc_impact_results_test))))
oc_impact_results <- oc_impact_results_test

# updating a few of the Race entries
oc_impact_results["Race"][oc_impact_results["Coach"] == "Billy Gonzales"] <- "Other"
oc_impact_results["Race"][oc_impact_results["Coach"] == "Ron Prince"] <- "Black"
# doing some preliminary analysis
oc_impact_results %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(net_ppa))
#Race  mean_net_ppa_race
#  1 ?              0.0380
#2 Black            0.0114
#3 Other           -0.0145
#4 White            0.0259 

####################################################################################
# check for normality and outliers
hist(oc_impact_results$net_ppa,
     xlab = "Net PPA",
     main = "Histogram of Net PPA")
# data seems to be normality distributed
boxplot(oc_impact_results$net_ppa,
        ylab = "Net PPA",
        main = "Boxplot of Net PPA")
# Remove the outliers
out_vals <- boxplot.stats(oc_impact_results$net_ppa)$out
out_inds <- which(oc_impact_results$net_ppa %in% c(out_vals))
out_inds

oc_impact_results1 <- oc_impact_results[-c(out_inds),]
ggqqplot(oc_impact_results1$net_ppa)
oc_impact_results1 %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(net_ppa))

# analyze via simple linear regression
lm1 <- lm(net_ppa ~ Race, oc_impact_results)
summary(lm1)
# no significant variables

# I wonder if it would be worth scraping the FPI offensive and defensive efficiencies instead of using PPA. Only if there seemed to be some signal here maybe...
########################################################################################

oc_impact_results %>% group_by(Race) %>% summarise(mean_net_sr_race = mean(net_sr))
oc_impact_results %>% group_by(Race) %>% summarise(mean_net_passsr_race = mean(net_pass_sr))

# repeat for defensive coordinators - pull defensive advanced stats

# data cleaning for DC dataframe

# filtering to only coaches where we will have before/after data - done
dc_recent <- defensive_coordinators %>% filter(year_start >= 2006)

# fixing issue with mismatched school names
dc_recent["College"][dc_recent["College"] == "FAU"] <- "Florida Atlantic"
dc_recent["College"][dc_recent["College"] == "FIU"] <- "Florida International"
dc_recent["College"][dc_recent["College"] == "Hawaii"] <- "Hawai'i"
dc_recent["College"][dc_recent["College"] == "Massachusetts"] <- "UMass"
dc_recent["College"][dc_recent["College"] == "Miami (FL)"] <- "Miami"
dc_recent["College"][dc_recent["College"] == "San Jose State"] <- "San José State"
dc_recent["College"][dc_recent["College"] == "Southern Miss"] <- "Southern Mississippi"
dc_recent["College"][dc_recent["College"] == "UConn"] <- "Connecticut"
dc_recent["College"][dc_recent["College"] == "UL Monroe"] <- "Louisiana Monroe"
dc_recent["College"][dc_recent["College"] == "USF"] <- "South Florida"
dc_recent["College"][dc_recent["College"] == "UTSA"] <- "UT San Antonio"

# Edit dc_recent to combine where the same guy is still the head coach but added/dropped coordinator title, etc
dc_recent <- dc_recent %>% 
  group_by(College, Coach) %>%
  mutate(year_start = min(year_start), 
         year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)

# creating an empty df that we will use to add rows to throughout - done
dc_impact <- data.frame()


# for loop that will create a huge before/after stat dataframe - done

# charlotte did not exist before 2015, so it errored out. starting the loop again with 
# row 115,  charlotte
# row 134, coastal
# same error for UTSA, starting again on row 764

for(i in 764:nrow(dc_recent)){
  # create a vector of years from start to end - done
  
  start_year <- as.integer(dc_recent[i, "year_start"])
  if (start_year<2005){start_year<-2005}
  end_year <- as.integer(dc_recent[i,6])
  years <-start_year:end_year
  
  # pull the team name - done
  team <- toString(dc_recent[i, "College"])
  
  # pull the coach's name - done
  coach <- toString(dc_recent[i, "Coach"])
  # pull the coach's race - done
  race <- toString(dc_recent[i, "Race"])
  
  # get the advanced stats history - done
  
  team_advanced <- data.frame()
  num_years <- length(years)
  
  for(year in years){
    progressr::with_progress({
      future::plan("multisession")
      team_advanced <- team_advanced %>% dplyr::bind_rows(
        cfbd_stats_season_advanced(year = year, team = team))
      
    })
  }
  
  # join the advanced stats and FPI to the oc_impact df, binding new rows - done
  
  for(j in 1:nrow(team_advanced)){
    row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("after"))
    colnames(row_to_add)[1] <- toString("Coach")
    colnames(row_to_add)[83] <- toString("Race")
    colnames(row_to_add)[84] <- toString("BeforeAfter")
    
    dc_impact <- dc_impact %>% bind_rows(row_to_add)
  }
  
  
  # create a vector of previous years for comparison, will mark data for these years as "before" - done
  
  previous_years <- (years[1] - 3):(years[1]-1)
  
  # checking to make sure that we have data for the years and adjusting the years vector - done
  if(previous_years[1] < 2005){
    previous_years <-2005:tail(previous_years, 1)
  }
  
  # repeat to get advanced stats and then join to oc_impact- done
  
  # get the before advanced stats history - done
  
  num_years <- length(previous_years)
  team_advanced <- data.frame()
  
  for(year in previous_years){
    team_advanced <- team_advanced %>% dplyr::bind_rows(
      cfbd_stats_season_advanced(year = year, team = team))
  }
  
  # join the advanced stats to the oc_impact df, binding new rows - done
  
  for(j in 1:nrow(team_advanced)){
    row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("before"))
    colnames(row_to_add)[1] <- toString("Coach")
    colnames(row_to_add)[83] <- toString("Race")
    colnames(row_to_add)[84] <- toString("BeforeAfter")
    dc_impact <- dc_impact %>% bind_rows(row_to_add)
  }
  
}

save(dc_impact, file="dc_impact.Rda")

# load("dc_impact.Rda")

# use code from Coaching Analysis to summarise before/after, net change, etc
dc_impact_summary <-dc_impact %>% select(c("Coach", "team", "def_ppa", "def_success_rate", "def_stuff_rate", "def_passing_plays_success_rate",
                                           "Race", "BeforeAfter"))

dc_impact_summary <- dc_impact_summary %>% group_by(Coach, team, Race, BeforeAfter) %>% summarise(
  mean_ppa = mean(def_ppa), 
  mean_sr = mean(def_success_rate),
  mean_stuff = mean(def_stuff_rate),
  mean_pass_sr = mean(def_passing_plays_success_rate),
)

# fixing to account for the three instances with no "before"
dc_impact_summary <- dc_impact_summary[-c(1105, 1114, 1189),]

# Calculating the net (offense-defense after-before) impact on PPA, SR, Stuff, Pass SR, FPI

dc_impact_results <- data.frame()
i <- 1
# flipping signs so that positive change is good for a DC
while (i < nrow(dc_impact_summary)){
  dc_results <- data.frame()
  row_to_add <- data.frame()
  dc_results <- dc_impact_summary[i,] %>% group_by(Coach, team, Race) %>% 
    summarise(net_ppa = -dc_impact_summary[i,"mean_ppa"]+dc_impact_summary[i+1, "mean_ppa"],
              net_sr = -dc_impact_summary[i,"mean_sr"]+dc_impact_summary[i+1, "mean_sr"],
              net_stuff = -dc_impact_summary[i,"mean_stuff"]+dc_impact_summary[i+1, "mean_stuff"],
              net_pass_sr = -dc_impact_summary[i,"mean_pass_sr"]+dc_impact_summary[i+1, "mean_pass_sr"])
  dc_impact_results <- dc_impact_results %>% bind_rows(dc_results)
  i=i+2
}

# Fixing issue where individual columns are lists
dc_impact_results_test <- lapply(dc_impact_results, unlist)
dc_impact_results_test <- data.frame(lapply(dc_impact_results_test, `length<-`, max(lengths(dc_impact_results_test))))
dc_impact_results <- dc_impact_results_test

# updating a few of the Race entries
dc_impact_results["Race"][dc_impact_results["Coach"] == "Phil Elmassian"] <- "White"
dc_impact_results["Race"][dc_impact_results["Coach"] == "Chris Simpson"] <- "Black"
dc_impact_results["Race"][dc_impact_results["Coach"] == "John Chavis"] <- "White"
dc_impact_results["Race"][dc_impact_results["Coach"] == "John Papuchis"] <- "White"
dc_impact_results["Race"][dc_impact_results["Coach"] == "Justin Ena"] <- "White"
dc_impact_results["Race"][dc_impact_results["Coach"] == "Justin Hamilton"] <- "White"

save(dc_impact_results, file="dc_impact_results.Rda")

# load("dc_impact_results.Rda")

# doing some preliminary analysis
dc_impact_results %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(net_ppa))
#Race  mean_net_ppa_race
# 1 Black           -0.0298
# 2 Other           -0.0135
# 3 White           -0.0243

dc_impact_results %>% group_by(Race) %>% summarise(mean_net_sr_race = mean(net_sr))
dc_impact_results %>% group_by(Race) %>% summarise(mean_net_passsr_race = mean(net_pass_sr))

####################################################################################
# Try grouping minorities together to see how they compare:
# dc_impact_results$Race <- ifelse(dc_impact_results$Race == "?", "Non-white",
#                                  ifelse(dc_impact_results$Race == "Other", "Non-white",
#                                         ifelse(dc_impact_results$Race == "Black", "Non-white", "White")))

# check for normality and outliers
hist(dc_impact_results$net_ppa,
     xlab = "Net PPA",
     main = "Histogram of Net PPA")
# data seems to be normality distributed
boxplot(dc_impact_results$net_ppa,
        ylab = "Net PPA",
        main = "Boxplot of Net PPA")
# Remove the outliers
out_vals <- boxplot.stats(dc_impact_results$net_ppa)$out
out_inds <- which(dc_impact_results$net_ppa %in% c(out_vals))
out_inds

dc_impact_results1 <- dc_impact_results[-c(out_inds),]
ggqqplot(dc_impact_results1$net_ppa)
dc_impact_results1 %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(net_ppa))

# analyze via simple linear regression
lm1 <- lm(net_ppa ~ Race, dc_impact_results)
summary(lm1)

########################################################################################

# We want to compare the impact of white and black coordinators who got hired as HC



defensive_to_head <- defensive_to_head %>% select(College.x, Coach, Race.x, College.y)
colnames(defensive_to_head) <- c("Coordinator_School", "Coach", "Race", "Head_Coach_School")
# ** - some more data cleaning to do - sometimes name of school doesn't match, so the same
# coach is considered o have gotten hired but it's just a different school name
# at least Diaz at Miami and Hopson at Southern Miss
# fixing issue with mismatched school names

defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "FAU"] <- "Florida Atlantic"
defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "Miami (FL)"] <- "Miami"
dc_recent["College"][dc_recent["College"] == "Miami (FL)"] <- "Miami"
dc_recent["College"][dc_recent["College"] == "San Jose State"] <- "San José State"
defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "Southern Miss"] <- "Southern Mississippi"
dc_recent["College"][dc_recent["College"] == "Southern Miss"] <- "Southern Mississippi"
dc_recent["College"][dc_recent["College"] == "UConn"] <- "Connecticut"
defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "UConn"] <- "Connecticut"
dc_recent["College"][dc_recent["College"] == "UL Monroe"] <- "Louisiana Monroe"
dc_recent["College"][dc_recent["College"] == "USF"] <- "South Florida"
defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "USF"] <- "South Florida"
dc_recent["College"][dc_recent["College"] == "UTSA"] <- "UT San Antonio"

dc_to_head_impact <- data.frame()
dc_to_head_impact <- defensive_to_head %>% inner_join(dc_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, team, net_ppa, net_sr, net_stuff, net_pass_sr
)
colnames(dc_to_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                 "Net_SR", "Net_Stuff", "Net_Pass_SR")

dc_to_head_impact %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
#Race  mean_net_ppa_race
#  1 Black           0.0176 
#2 Other          -0.0133 
#3 White          -0.00471
## - We have something!
sd(dc_to_head_impact$Net_PPA)
# SD is 0.067, so black DCs about 0.5 sd's better

dc_to_head_impact %>% group_by(Race) %>% summarise(mean_net_sr_race = mean(Net_SR))
#Race  mean_net_sr_race
#  1 Black          0.0125 
#2 Other          0.00652
#3 White         -0.00154

# doing the same for offense
offensive_to_head <- offensive_to_head %>% select(College.x, Coach, Race.x, College.y)
colnames(offensive_to_head) <- c("Coordinator_School", "Coach", "Race", "Head_Coach_School")

offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "FAU"] <- "Florida Atlantic"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "FIU"] <- "Florida International"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "Miami (FL)"] <- "Miami"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "Hawaii"] <- "Hawai'i"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "Miami (FL)"] <- "Miami"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "San Jose State"] <- "San José State"
defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "Southern Miss"] <- "Southern Mississippi"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "Southern Miss"] <- "Southern Mississippi"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "UConn"] <- "Connecticut"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "Massachusetts"] <- "UMass"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "UL Monroe"] <- "Louisiana Monroe"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "USF"] <- "South Florida"
defensive_to_head["Head_Coach_School"][defensive_to_head["Head_Coach_School"] == "USF"] <- "South Florida"
offensive_to_head["Head_Coach_School"][offensive_to_head["Head_Coach_School"] == "UTSA"] <- "UT San Antonio"

oc_to_head_impact <- data.frame()
oc_to_head_impact <- offensive_to_head %>% inner_join(oc_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, team, net_ppa, net_sr, net_stuff, net_pass_sr
)
colnames(oc_to_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                 "Net_SR", "Net_Stuff", "Net_Pass_SR")

oc_to_head_impact %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_net_ppa_race
#  1 ?                0.0633
#2 Black            0.0116
#3 Other           -0.0743
#4 White            0.0358

##########################################################################################
# dc_to_head_impact and oc_to_head_impact had a total of 19 unique Black coaches included. 
# I don't think there's a great reason to keep these 2 separate, so combine datasets and minority races to increase sample size
coord_to_head_impact <- dc_to_head_impact %>%
  rbind(oc_to_head_impact)
coord_to_head_impact$Race <- ifelse(coord_to_head_impact$Race == "?", "Non-white",
                                 ifelse(coord_to_head_impact$Race == "Other", "Non-white",
                                        ifelse(coord_to_head_impact$Race == "Black", "Non-white", "White")))
coord_to_head_impact %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
# Race      mean_net_ppa_race
# 1 Non-white           0.00546
# 2 White               0.0191 

# (no signal)
##########################################################################################

oc_to_head_impact %>% group_by(Race) %>% summarise(mean_net_sr_race = mean(Net_SR))
oc_to_head_impact %>% group_by(Race) %>% summarise(mean_net_stuff_race = mean(Net_Stuff))

## need to run again with the numbers for their impact as head coaches
# Show that the head coach results have been better for black defensive coordinators? they have not

former_dc_head_impact <- data.frame()
former_dc_head_impact <- defensive_to_head %>% inner_join(head_coach_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, Team, Net_PPA, Net_SR, Net_Stuff_Rate, Net_Pass_SR, Net_FPI
)
colnames(former_dc_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                     "Net_SR", "Net_Stuff", "Net_Pass_SR", "Net_FPI")

former_dc_head_impact %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_net_ppa_race
# <chr>             <dbl>
#   1 Black         -0.0437  
# 2 Other         -0.0344  
# 3 White         -0.000818

former_dc_head_impact %>% group_by(Race) %>% filter(!is.na(Net_FPI)) %>% summarise(mean_net_fpi_race = mean(Net_FPI))
# Race  mean_net_fpi_race
# <chr>             <dbl>
#   1 Black           -6.80  
# 2 Other           -1.96  
# 3 White            0.0311
#This definitely does not support hypothesis
# Then propose some black defensive coordinators a as good head coach options?
# Brian Norwood

# running oc's with their head coach numbers
former_oc_head_impact <- data.frame()
former_oc_head_impact <- offensive_to_head %>% inner_join(head_coach_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, Team, Net_PPA, Net_SR, Net_Stuff_Rate, Net_Pass_SR, Net_FPI
)
colnames(former_oc_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                     "Net_SR", "Net_Stuff", "Net_Pass_SR", "Net_FPI")

former_oc_head_impact %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_net_ppa_race
# <chr>             <dbl>
#   1 ?              -0.0222 
# 2 Black          -0.00891
# 3 Other           0.0377 
# 4 White           0.0170 

former_oc_head_impact %>% group_by(Race) %>% filter(!is.na(Net_FPI)) %>% summarise(mean_net_fpi_race = mean(Net_FPI))
# Race  mean_net_fpi_race
# <chr>             <dbl>
#   1 ?                4     
# 2 Black           -0.998 
# 3 Other            4.44  
# 4 White           -0.0952
#This definitely does not support hypothesis
# Then propose some black offensive coordinators a as good head coach options?
# Maurice Harris
# Alex Atkins 
# Josh Gattis
# Newland Isaac


############################################################################################

# Are there certain HCs who seem to more readily give minorities promotions / coordinator opportunities?
test <- head_coach_impact %>% distinct(Coach)

coaching_tree <- head_coach_impact %>%
  select(Coach, season, team, Race) %>%
  left_join((dc_impact %>% 
               filter(BeforeAfter == "after") %>%
               select(Coach, season, team, Race) %>% 
               rename(Coordinator = Coach)), 
            by = c("season", "team"))
coaching_tree1 <- head_coach_impact %>%
  select(Coach, season, team, Race) %>%
  left_join((oc_impact %>%
               filter(BeforeAfter == "after") %>%
               select(Coach, season, team, Race) %>%
               rename(Coordinator = Coach)),
            by = c("season", "team"))
coaching_tree <- coaching_tree %>%
  rbind(coaching_tree1) %>%
  filter(!is.na(Coordinator))


# Remove this next line if you want to see white vs. black instead of white vs minority
coaching_tree$Race.y <- ifelse(coaching_tree$Race.y == "?", "Non-white",
                                 ifelse(coaching_tree$Race.y == "Other", "Non-white",
                                        ifelse(coaching_tree$Race.y == "Black", "Non-white", "White")))
# The following counts each year separately, so for example Andrew Thacker would count a 3 years of a White DC.
hires_by_years <- coaching_tree %>% 
  group_by(Coach, Race.x) %>%
  count(Race.y, name = "years") %>%
  mutate(total_years = sum(years)) %>%
  ungroup() %>%
  mutate(percent_of_years_POC = years/total_years) %>%
  filter(Race.y == "Non-white")
# Now count each coordinator's tenure as 1 (not weighted for how long they held the position)
hires_by_coord <- coaching_tree %>%
  select(Coach, Race.x, team, Coordinator, Race.y) %>%
  distinct() %>%
  group_by(Coach, Race.x) %>%
  count(Race.y, name = "coordinators") %>%
  mutate(total_coordinators = sum(coordinators)) %>%
  ungroup() %>%
  mutate(percent_of_coords_POC = coordinators/total_coordinators) %>%
  filter(Race.y == "Non-white")
minority_hires <- hires_by_years %>%
  left_join(hires_by_coord) %>%
  mutate(years_rank = rank(desc(percent_of_years_POC)),
         coords_rank = rank(desc(percent_of_coords_POC)),
         rank = rank(years_rank + coords_rank))
# This may be a good opportunity for a 538-style table

# Do minority HCs hire more minority coordinators than white HCs?
minority_hires$Race.x <- ifelse(minority_hires$Race.x == "?", "Non-white",
                               ifelse(minority_hires$Race.x == "Other", "Non-white",
                                      ifelse(minority_hires$Race.x == "Black", "Non-white", "White")))
minority_hires %>%
  group_by(Race.x) %>%
  summarise(percent_of_years_POC = mean(percent_of_years_POC),
            percent_of_coords_POC = mean(percent_of_coords_POC))
# minority HCs are 2-3% more likely than white HCs to hire a minority coordinator.
sum(minority_hires$total_coordinators) # <- sample size
############################################################################################
