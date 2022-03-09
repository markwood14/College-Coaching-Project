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
library(colorBlindness)

# Read and clean the coaches csv
coach_df1 <- read.csv("coaches_by_race.csv")
coach_df <- coach_df1 %>%
  filter(Race != "#N/A") %>%
  group_by(Coach, College, Role) %>%
  mutate(year_start = min(Season),
         year_end = max(Season)) %>%
  select(c(College, Coach, Role, Race, year_start, year_end)) %>%
  distinct()
# save(coach_df,file="coach_df.Rda")

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

# the following numbers are not 'per year' and haven't been fixed for when a coach drops a coordinator/assistant tag.
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


# How long do coaches stay in their roles?
coach_df %>% group_by(Race) %>% summarise(duration = mean(year_end - year_start)+1)
# Race  duration
# 1 ?         1.96
# 2 Black     2.12
# 3 Other     2.37
# 4 White     2.45
head_coaches <- head_coaches %>% 
  group_by(College, Coach) %>%
  mutate(year_start = min(year_start), 
         year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)
head_coaches %>% group_by(Race) %>% summarise(duration = mean(year_end - year_start)+1)
# Race  duration
# 1 ?         2.5 
# 2 Black     3.20
# 3 Other     5.5 
# 4 White     4.50
defensive_coordinators %>% group_by(Race) %>% summarise(duration = mean(year_end - year_start)+1)
# Race  duration
# 1 ?         2.31
# 2 Black     1.81
# 3 Other     2.09
# 4 White     2.28
offensive_coordinators %>% group_by(Race) %>% summarise(duration = mean(year_end - year_start)+1)
# Race  duration
# 1 ?         1.89
# 2 Black     2   
# 3 Other     1.71
# 4 White     2.31
# Black coaches have shorter tenures than their white counterparts at every level. Why?
tenure <- head_coaches %>%
  mutate(Role = "Head Coach") %>%
  rbind(offensive_coordinators %>%
          mutate(Role = "Offensive Coordinator")) %>%
  rbind(defensive_coordinators %>%
          mutate(Role = "Defensive Coordinator")) %>%
  mutate(Race = ifelse(Race == "?", "Other", Race)) %>%
  mutate(duration = mean(year_end - year_start)+1)
tenure$Race <- factor(as.factor(tenure$Race), levels = c('White', 'Black', 'Other'))
tenure$Role <- factor(as.factor(tenure$Role), levels = c('Head Coach', 'Offensive Coordinator', 'Defensive Coordinator'))
tenure_jitter <- ggplot(data=tenure, aes(x = Race, y = duration, colour = Role)) +
  # geom_hline(yintercept=0) +
  geom_jitter(width = 0.48, height = 0.48, size = 1.5, alpha=0.7) +
  geom_vline(xintercept = c(1.5, 2.5)) +
  scale_colour_brewer(palette = "Set2") +
  # ylim(0.5,22.5) +
  coord_cartesian(ylim = c(1.4, 22)) +
  theme_light() +
  labs(title = "Length of Tenure ", 
       # subtitle = "",
       fill = "Race: ",
       caption = "Plot: @markwood14 and @robert_binion") +
  ylab("Years") +
  theme(# panel.grid.minor.x = element_line(linetype = 1, color = "red"),
    # legend.position = ("bottom"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.subtitle = element_text(size=9),
    axis.title.x = element_blank())
tenure_jitter
ggsave('tenure_jitter.png', height = 5.625, width = 10, dpi = 300)
#########################################################################################


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

load("head_coach_impact.Rda")
head_coach_impact_weighted <- head_coach_impact %>% filter(BeforeAfter == "after") %>% group_by(Coach, team) %>%
  summarise(tenure_length = n())
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

load("head_coach_impact_results.Rda")
# need to remove the three coaches we removed in the other df - Lambert, Moglia, Coker UTSA
head_coach_impact_weighted <- head_coach_impact_weighted[-c(26, 171, 203),]

# add tenure length to this Df so we can weight
head_coach_tenures <- data.frame(head_coach_impact_weighted$tenure_length)
head_coach_impact_results <- head_coach_impact_results %>% bind_cols(head_coach_tenures)

# Fixing issue where individual columns are lists
head_coach_impact_results_test <- lapply(head_coach_impact_results, unlist)
head_coach_impact_results_test <- data.frame(lapply(head_coach_impact_results_test, `length<-`, max(lengths(head_coach_impact_results_test))))
head_coach_impact_results <- head_coach_impact_results_test
colnames(head_coach_impact_results) <- c(toString("Coach"), "Team", "Race", "Net_PPA", "Net_SR", "Net_Stuff_Rate", "Net_Pass_SR", "Net_FPI", "Tenure")
# adding in cols that multiple tenure by pp metrics
head_coach_impact_results <- head_coach_impact_results %>% mutate(total_sr = Net_SR*Tenure, total_stuffs = Net_Stuff_Rate*Tenure, total_pass_sr = Net_Pass_SR*Tenure)

# doing some preliminary analysis
head_coach_impact_results %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
#   Race  mean_net_ppa_race
#  1 ?              -0.0222 
#2 Black          -0.0292 
#3 Other           0.00167
#4 White           0.0106 



head_coach_impact_results_test <- lapply(head_coach_impact_results, unlist)
head_coach_impact_results_test <- data.frame(lapply(head_coach_impact_results_test, `length<-`, max(lengths(head_coach_impact_results_test))))
head_coach_impact_results <- head_coach_impact_results_test
head_coach_impact_results["Race"][head_coach_impact_results["Race"] == "?"] <- "Other" 

head_coach_impact_plot <- head_coach_impact_results %>% ggplot(aes(x = Net_SR, y = Net_PPA, colour = Race)) +
  geom_jitter(width = 0.48, height = 0.48, size = 1.5, alpha=0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Net Success Rate Change", y= "Net PPA Change",
       title = "Head Coach Impact by Race", 
       subtitle = "FBS, 2005-present\nR-Squared for Net_PPA~Race = 0.0093",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: @cfb_data with #cfbfastR") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("top"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9))
head_coach_impact_plot
ggsave("head_coach_impact_plot.png", height = 7, width = 10, dpi = 300)

head_coach_impact_results %>% group_by(Race) %>% summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_total_ppa_race
#   1 ?                 -0.0203
# 2 Black             -0.0713
# 3 Other              0.0202
# 4 White              0.121 
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
head_coach_impact_results1 %>% group_by(Race) %>% summarise(count = n())
# analyze via simple linear regression
lm1 <- lm(Net_PPA ~ Race, head_coach_impact_results1)
summary(lm1)
# no significant variables

# check the total metric for normality and outliers
hist(head_coach_impact_results$total_ppa,
     xlab = "Total PPA",
     main = "Histogram of Total PPA")
# data has a decent right tail
boxplot(head_coach_impact_results$total_ppa,
        ylab = "Total PPA",
        main = "Boxplot of Total PPA")
# there are lots of outliers. Let's remove them.
out_vals <- boxplot.stats(head_coach_impact_results$total_ppa)$out
out_inds <- which(head_coach_impact_results$total_ppa %in% c(out_vals))
out_inds
# (I didn't want to overwrite your 'head_coach_impact_results' df so I just added the 'weighted' suffix. - MW)
head_coach_impact_results_weighted <- head_coach_impact_results[-c(out_inds),]
library(ggpubr)
ggqqplot(head_coach_impact_results_weighted$total_ppa)
head_coach_impact_results_weighted %>% group_by(Race) %>% summarise(total_net_ppa_race = mean(total_ppa))
# Race  total_net_ppa_race
# <chr>              <dbl>
#   1 ?                -0.0203
# 2 Black            -0.0573
# 3 Other            -0.120 
# 4 White             0.0571
# analyze via simple linear regression
lm1 <- lm(total_ppa ~ Race, head_coach_impact_results_weighted)
summary(lm1)
# Still no significant variables

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

for(i in 1:nrow(oc_recent)){
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

load("oc_impact.Rda")

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

load("oc_impact_results.Rda")

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

for(i in 1:nrow(dc_recent)){
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

load("dc_impact.Rda")

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

load("dc_impact_results.Rda")
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

load("dc_impact_results.Rda")

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

# checking the significance level here:
dc_to_head_model <- lm(Net_PPA~Race, data = dc_to_head_impact)
summary(dc_to_head_model)
summary(aov(Net_PPA~Race, data = dc_to_head_impact))
# p value of .3612 for this as a whole. So no significance here

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
# Then propose some black defensive coordinators as good head coach options?
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

former_oc_head_impact$Race <- ifelse(former_oc_head_impact$Race == "?", "Non-white",
                                     ifelse(former_oc_head_impact$Race == "Other", "Non-white",
                                            ifelse(former_oc_head_impact$Race == "Black", "Non-white", "White")))
former_oc_head_impact %>% group_by(Race) %>% filter(!is.na(Net_FPI)) %>% summarise(mean_net_fpi_race = mean(Net_FPI))
# Race  mean_net_fpi_race
# <chr>             <dbl>
#   1 ?                4     
# 2 Black           -0.998 
# 3 Other            4.44  
# 4 White           -0.0952

# Race      mean_net_fpi_race
# <chr>                 <dbl>
#   1 Non-white           -0.384 
# 2 White               -0.0952
#This  does not support hypothesis
# Then propose some black offensive coordinators as good head coach options?
# Maurice Harris
# Alex Atkins 
# Josh Gattis
# Newland Isaac


############################################################################################

# Are there certain HCs who seem to more readily give minorities promotions / coordinator opportunities?

coordinators <- coach_df1 %>% filter(str_detect(Role, "Coordinator"))
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

head_coaches <- coach_df1 %>% filter(str_detect(Role, "Head"))
head_coaches <- head_coaches %>% filter(!str_detect(Role, "Associate Head"))
head_coaches <- head_coaches %>% filter(!str_detect(Role, "Assistant Head"))
head_coaches <- head_coaches %>% filter(!str_detect(Role, "Interim Head"))

head_coaches["College"][head_coaches["College"] == "FAU"] <- "Florida Atlantic"
head_coaches["College"][head_coaches["College"] == "FIU"] <- "Florida International"
head_coaches["College"][head_coaches["College"] == "Hawaii"] <- "Hawai'i"
head_coaches["College"][head_coaches["College"] == "Massachusetts"] <- "UMass"
head_coaches["College"][head_coaches["College"] == "Miami (FL)"] <- "Miami"
head_coaches["College"][head_coaches["College"] == "San Jose State"] <- "San José State"
head_coaches["College"][head_coaches["College"] == "Southern Miss"] <- "Southern Mississippi"
head_coaches["College"][head_coaches["College"] == "UConn"] <- "Connecticut"
head_coaches["College"][head_coaches["College"] == "UL Monroe"] <- "Louisiana Monroe"
head_coaches["College"][head_coaches["College"] == "USF"] <- "South Florida"
head_coaches["College"][head_coaches["College"] == "UTSA"] <- "UT San Antonio"

offensive_coordinators["College"][offensive_coordinators["College"] == "FAU"] <- "Florida Atlantic"
offensive_coordinators["College"][offensive_coordinators["College"] == "FIU"] <- "Florida International"
offensive_coordinators["College"][offensive_coordinators["College"] == "Hawaii"] <- "Hawai'i"
offensive_coordinators["College"][offensive_coordinators["College"] == "Massachusetts"] <- "UMass"
offensive_coordinators["College"][offensive_coordinators["College"] == "Miami (FL)"] <- "Miami"
offensive_coordinators["College"][offensive_coordinators["College"] == "San Jose State"] <- "San José State"
offensive_coordinators["College"][offensive_coordinators["College"] == "Southern Miss"] <- "Southern Mississippi"
offensive_coordinators["College"][offensive_coordinators["College"] == "UConn"] <- "Connecticut"
offensive_coordinators["College"][offensive_coordinators["College"] == "UL Monroe"] <- "Louisiana Monroe"
offensive_coordinators["College"][offensive_coordinators["College"] == "USF"] <- "South Florida"
offensive_coordinators["College"][offensive_coordinators["College"] == "UTSA"] <- "UT San Antonio"

defensive_coordinators["College"][defensive_coordinators["College"] == "FAU"] <- "Florida Atlantic"
defensive_coordinators["College"][defensive_coordinators["College"] == "FIU"] <- "Florida International"
defensive_coordinators["College"][defensive_coordinators["College"] == "Hawaii"] <- "Hawai'i"
defensive_coordinators["College"][defensive_coordinators["College"] == "Massachusetts"] <- "UMass"
defensive_coordinators["College"][defensive_coordinators["College"] == "Miami (FL)"] <- "Miami"
defensive_coordinators["College"][defensive_coordinators["College"] == "San Jose State"] <- "San José State"
defensive_coordinators["College"][defensive_coordinators["College"] == "Southern Miss"] <- "Southern Mississippi"
defensive_coordinators["College"][defensive_coordinators["College"] == "UConn"] <- "Connecticut"
defensive_coordinators["College"][defensive_coordinators["College"] == "UL Monroe"] <- "Louisiana Monroe"
defensive_coordinators["College"][defensive_coordinators["College"] == "USF"] <- "South Florida"
defensive_coordinators["College"][defensive_coordinators["College"] == "UTSA"] <- "UT San Antonio"

head_coaches %>% group_by(Race) %>% summarise(num_race = n())

coaching_tree <- head_coaches %>%
  select(Coach, Season, College, Race) %>%
  left_join((defensive_coordinators %>% 
               select(Coach, Season, College, Race) %>% 
               rename(Coordinator = Coach)), 
            by = c("Season", "College"))
coaching_tree1 <- head_coaches %>%
  select(Coach, Season, College, Race) %>%
  left_join((offensive_coordinators %>%
               select(Coach, Season, College, Race) %>%
               rename(Coordinator = Coach)),
            by = c("Season", "College"))
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
  select(Coach, Race.x, College, Coordinator, Race.y) %>%
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
colnames(minority_hires) <- c("Coach", "Coach_Race", "Alternate_Race", "Non-White_Coordinator_Seasons", 
                              "Available_Coordinator_Seasons", "Available_Seasons_with_Non-White_Coordinator_Percentage", 
                              "Non-White_Coordinators", "Possible_Coordinators", "Non-White_Coordinators_Percentage", 
                              "Non-White_Coordinator_Seasons_Rank", "Non-White_Coordinators_Rank", "Combined_Rank")

minority_hires_for_table <- minority_hires %>% select(Coach, `Available_Seasons_with_Non-White_Coordinator_Percentage`, `Non-White_Coordinators_Percentage`, Combined_Rank)
minority_hires_for_table <- minority_hires_for_table %>% mutate_if(is.numeric, round, digits = 2)
minority_hires_for_table <- minority_hires_for_table %>% arrange(Combined_Rank)

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}
minority_hires_for_table$Combined_Rank <- floor(minority_hires_for_table$Combined_Rank)
minority_hires_top20 <- minority_hires_for_table[1:20,]
minority_hires_bottom20 <-minority_hires_for_table[188:207,]
colnames(minority_hires_top20) <- c('Coach', 'Non-White Coordinator Seasons', 'Non-White Coordinators', "Combined Rank")
colnames(minority_hires_bottom20) <- c('Coach', 'Non-White Coordinator Seasons', 'Non-White Coordinators', "Combined Rank")
minority_hiring_table_top20 <- minority_hires_top20 %>% gt() %>%  tab_spanner(
  label = "Non-White Coordinator Percentages\nTop 20",
  columns = c("Non-White Coordinator Seasons", 
              "Non-White Coordinators")) %>% 
  data_color(
    columns = c("Non-White Coordinator Seasons", "Non-White Coordinators"),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  #cols_label(
  #  success_rate = "SUCCESS RATE (%)"
  #) %>% 
  tab_source_note(
    source_note = md("SOURCE: CFB_Data & Team Resource Pages <br>TABLE: @Robert_Binion & @MarkWood14")
  ) %>% 
  gt_theme_538(table.width = px(550))

minority_hiring_table_top20
gtsave(minority_hiring_table_top20, "Minority_Hiring_Table_Top20.png")

minority_hiring_table_bottom20 <- minority_hires_bottom20 %>% gt() %>%  tab_spanner(
  label = "Non-White Coordinator %\nBottom 20",
  columns = c("Non-White Coordinator Seasons", 
              "Non-White Coordinators")) %>% 
  data_color(
    columns = c("Non-White Coordinator Seasons", "Non-White Coordinators"),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  #cols_label(
  #  success_rate = "SUCCESS RATE (%)"
  #) %>% 
  tab_source_note(
    source_note = md("SOURCE: CFB_Data & Team Resource Pages <br>TABLE: @Robert_Binion & @MarkWood14")
  ) %>% 
  gt_theme_538(table.width = px(550))

minority_hiring_table_bottom20
gtsave(minority_hiring_table_bottom20, "Minority_Hiring_Table_bottom20.png")

# Do minority HCs hire more minority coordinators than white HCs?
minority_hires$Coach_Race <- ifelse(minority_hires$Coach_Race == "?", "Non-white",
                                ifelse(minority_hires$Coach_Race == "Other", "Non-white",
                                       ifelse(minority_hires$Coach_Race == "Black", "Non-white", "White")))
minority_hires %>%
  group_by(Coach_Race) %>%
  summarise(percent_of_years_POC = mean(`Available_Seasons_with_Non-White_Coordinator_Percentage`),
            percent_of_coords_POC = mean(`Non-White_Coordinators_Percentage`))

# Coach_Race percent_of_years_POC percent_of_coords_POC
# <chr>                     <dbl>                 <dbl>
#   1 Non-white                 0.345                 0.343
# 2 White                     0.265                 0.281
# Coach_Race percent_of_years_POC percent_of_coords_POC
# <chr>                     <dbl>                 <dbl>
#   1 ?                         0.5                   0.417
# 2 Black                     0.334                 0.349
# 3 Other                     0.353                 0.258
# 4 White                     0.265                 0.281
# minority HCs are 6-8 percentage points more likely than white HCs to hire a minority coordinator (20-30% more likely).
# sum(minority_hires$total_coordinators) # <- sample size

# linking the above data with connected and influence data from below:
centrality_df_modified <- centrality_df %>% select(variable, closeness, degree, Race)
colnames(centrality_df_modified) <- c("Coach", "Influence", "Connections",  "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at("Influence", ~(scale(., center = FALSE) %>% as.vector))
centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

minority_hires_influence <- minority_hires %>% left_join(centrality_df_scaled, by = "Coach")
minority_hires_influence <- minority_hires_influence %>% select(Coach, Coach_Race, `Available_Seasons_with_Non-White_Coordinator_Percentage`, `Non-White_Coordinators_Percentage`, Combined_Rank, Connections, Influence)
minority_hires_influence <- minority_hires_influence %>% mutate_if(is.numeric, round, digits = 2)
minority_hires_influence$Combined_Rank <- floor(minority_hires_influence$Combined_Rank)

# sort based on the Connections
minority_hires_connection_rank <- minority_hires_influence[order(-minority_hires_influence$Connections),]

# add a rank column
minority_hires_connection_rank <- rowid_to_column(minority_hires_connection_rank, "Connections Rank")
minority_hires_connection_rank <- minority_hires_connection_rank %>% relocate("Connections Rank", .after = "Influence")

# sort based on the Influence
minority_hires_connection_rank <- minority_hires_connection_rank[order(-minority_hires_connection_rank$Influence),]

# add a rank column
minority_hires_connection_rank <- rowid_to_column(minority_hires_connection_rank, "Influence Rank")
minority_hires_connection_rank <- minority_hires_connection_rank %>% relocate("Influence Rank", .after = "Connections Rank")


colnames(minority_hires_connection_rank) <- c("Coach", "Coach_Race",'Non-White Coordinator Seasons', 'Non-White Coordinators', "Non-White Coordinator Hiring Rank", "Connections", "Influence", "Connections Rank", "Influence Rank" )

minority_hires_connection_for_table <- minority_hires_connection_rank %>% select(Coach, Coach_Race, 'Non-White Coordinator Seasons', 'Non-White Coordinators', "Non-White Coordinator Hiring Rank", "Connections Rank", "Influence Rank" )

minority_hires_connection_for_table <- minority_hires_connection_for_table %>% arrange(`Connections Rank`)
minority_hires_top20_connections <- minority_hires_connection_for_table[1:20,]

minority_hires_connection_for_table <- minority_hires_connection_for_table %>% arrange(`Influence Rank`)
minority_hires_top20_influential <- minority_hires_connection_for_table[1:20,]

minority_hiring_table_connected20 <- minority_hires_top20_connections %>% gt() %>%  tab_spanner(
  label = "Most Connected Coaches Coordinator Hiring",
  columns = c("Non-White Coordinator Seasons", 
              "Non-White Coordinators", "Non-White Coordinator Hiring Rank")) %>% 
  data_color(
    columns = c("Non-White Coordinator Hiring Rank"),
    colors = scales::col_numeric(
      palette = "RdBu",
      domain = c(1:208),
      reverse = TRUE
    )
  ) %>% 
  #cols_label(
  #  success_rate = "SUCCESS RATE (%)"
  #) %>% 
  tab_source_note(
    source_note = md("SOURCE: CFB_Data & Team Resource Pages <br>TABLE: @Robert_Binion & @MarkWood14")
  ) %>% 
  gt_theme_538(table.width = px(550))
minority_hiring_table_connected20
gtsave(minority_hiring_table_connected20, "minority_hiring_table_connected20.png")

minority_hiring_table_influential20 <- minority_hires_top20_influential %>% gt() %>%  tab_spanner(
  label = "Most Influential Coaches Coordinator Hiring",
  columns = c("Non-White Coordinator Seasons", 
              "Non-White Coordinators", "Non-White Coordinator Hiring Rank")) %>% 
  data_color(
    columns = c("Non-White Coordinator Hiring Rank"),
    colors = scales::col_numeric(
      palette = "RdBu",
      domain = c(1:208),
      reverse = TRUE
    )
  ) %>% 
  #cols_label(
  #  success_rate = "SUCCESS RATE (%)"
  #) %>% 
  tab_source_note(
    source_note = md("SOURCE: CFB_Data & Team Resource Pages <br>TABLE: @Robert_Binion & @MarkWood14")
  ) %>% 
  gt_theme_538(table.width = px(550))
minority_hiring_table_influential20
gtsave(minority_hiring_table_influential20, "minority_hiring_table_influential20.png")
############################################################################################

# https://ona-book.org/community.html

# Use the Louvain algorithm to further analyze the social impact of coaching hires?
# install.packages("igraph")
library(igraph)
install.packages("qgraph")
library(qgraph)
library(corrplot)
library(Hmisc)
coaching_tree1 <- coaching_tree %>% 
  group_by(Coach, Coordinator) %>%
  summarise(years_together = n())

###
# create a  graph from an edgelist
edgelist <- coaching_tree %>%
  select(Coach, Coordinator) %>%
  rename(c("from" = "Coach", "to" = "Coordinator"))
edgelist_matrix <- as.matrix(edgelist)
graph1 <- igraph::graph_from_edgelist(el = edgelist_matrix, directed = TRUE)
graph1
# IGRAPH 5da95ff DN-- 1431 5643 -- 
# ^ denotes 'D'irected graph with 'N'amed vertices containing 1,431 vertices and 5,643 edges
# create a weighted graph from a dataframe
coaching_tree1 <- coaching_tree1 %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))
vertex_df <- head_coaches %>%
  rbind(defensive_coordinators) %>%
  rbind(offensive_coordinators) %>%
  select(Coach, Race) %>%
  distinct()
# create the graph object
graph <- igraph::graph_from_data_frame(d = coaching_tree1, directed = TRUE, vertices = vertex_df)
graph
# IGRAPH d0216f2 DNW- 1431 2214 -- 
#   + attr: name (v/c), weight (e/n)
# ^ denotes a Directed graph with Named vertices and numerically (e/n) Weighted edges 
# V(graph) # gives vertices of the graph
# E(graph) # gives edge sets of a graph
# E(graph)$weight # gives weights of edges
# V(graph)$Race # gives Race of each vertex/node
# assign communities to graph

# LABELS
set.seed(123)
# only store a label if "Nick Saban" or "Geoff Collins"
V(graph)$label <- ifelse(V(graph)$name %in% c("Nick Saban", "Geoff Collins"),
                         V(graph)$name,
                         "")
# change label font color, size, and font family
V(graph)$label.color <- "black"
V(graph)$label.cex <- 0.8
# V(graph)$label.family <- "arial"

# VERTICES
V(graph)$color <- graph$community
V(graph)$size <- 3 
V(graph)$frame.color = "black"

# EDGES
#E(graph)$width <- graph$weight
E(graph)$arrow.size <- 0.5
# how to get alpha or width of edges to change based on weights?
#create plot layout
layout1 = layout_randomly(graph)
# shape oriented layouts
layout2 = layout_as_tree(graph)
layout3 = layout_in_circle(graph)
layout4 = layout_on_sphere(graph)
# force-directed layouts - use algorithms to attract connected vertices together and repel non-connected vertices
layout5 = layout_with_fr(graph)
layout6 = layout_with_kk(graph)
# Sugiyama is suitable for directed graphs and minimizes edge crossings by introducing bends on edges
layout7 = layout_with_sugiyama(graph) # says layout must be a matrix?
# for large graphs
layout8 = layout_with_lgl(graph)
layout9 = layout_with_drl(graph)
layout10 = layout_with_graphopt(graph)
plot(graph, layout = layout1)
plot(graph, layout = layout2)
plot(graph, layout = layout3)
plot(graph, layout = layout4)
plot(graph, layout = layout5)
plot(graph, layout = layout6)
plot(graph, layout = layout7)
plot(graph, layout = layout8)
plot(graph, layout = layout9)
plot(graph, layout = layout10)
# or use ggraph similar to ggplot2
# install.packages("ggraph")
library(ggraph)
ggraph(graph, layout = "kk") +
  geom_edge_link(aes(edge_width = weight), color = "grey", alpha = 0.7, show.legend = FALSE) +
  geom_node_point(color = "blue", size = 1) +
  # geom_node_label(aes(label = name), color = "blue") +
  # or geom_node_point(aes(color = community))
  theme_void() +
  labs(title = "Coaching Tree")
# or use library(networkD3) for interactive graphs (I don't know how to embed this in an article while keeping the interactive features though)
install.packages("networkD3")
library(networkD3)
networkD3::simpleNetwork(coaching_tree1)
V(graph)$group <- ifelse(V(graph)$name %in% c("Nick Saban", "Geoff Collins"), 1, 2)
netd3_list <- networkD3::igraph_to_networkD3(graph, group = V(graph)$group)
networkD3::forceNetwork(
  Links = netd3_list$links,
  Nodes = netd3_list$nodes,
  NodeID = "name",
  Source = "source",
  Target = "target",
  Group = "group"
)

# PATHS, DISTANCE, & CENTRALITY
edge_density(graph)
# 0.001081937 (it's a sparse graph)
get_diameter(graph)
get_diameter(graph, weights = NA)
diameter_list <- as.list(get_diameter(graph, weights = NA))
# Bret was a coordinator for Bill, Paul was a coordinator for Bret, Dave was a coordinator for Paul, etc. This is the longest "shortest path".
# how to create a subgraph with only certain coaches included - this subgraph only includes coaches (nodes) from the diameter above:
subgraph <- induced_subgraph(graph, vids = c(diameter_list))
plot(subgraph, layout = layout.auto)
# are there any disconnected nodes in this graph?
is.connected(graph)
# FALSE so it is not connected, ie there are disconnected nodes in the graph.
# now who has the most connections?
# create empty vectors:
v_name <- c()
n_neighbors <- c()
# capture name and number of neighbors for every vertex. A vertex is a neighbor of another one (in other words, the two vertices are adjacent), if they are incident to the same edge.
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
# find the max
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
# Nick Saban has the most neighbors in this data set at 20.
# average distance, not considering weights. Distance is the length of shortest paths
mean_distance(graph) # 3.578102
dt <- distance_table(graph, directed = TRUE)
barplot(dt$res)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree(graph)
degree_centrality <- melt(data.frame(as.list(degree(graph))))
# Todd Graham at 21, Nick Saban and Tommy Tubberville at 20. I wonder why it says Saban has the most neighbors at 20, but Todd Graham has the highest Degree Centrality at 21.
# Ego Size: the n-th order ego network of a given vertex v is a set including v and all vertices of distance at most n from v. (Saban has a 1st-order ego size of 20)
ego(graph, order=2)
ego_size(graph, order=2, nodes = V(graph))
# Saban has 111 2nd-degree connections (aka 2nd-order edges)
# Closeness Centrality
closeness_centrality <- melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"
# 2nd & 3rd order (ego) are not included here
melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))
centrality_df["Race"][centrality_df["variable"] == "Aazaar Abdul Rahim"] <- "Black"
centrality_df["Race"][centrality_df["variable"] == "Joe Salave a"] <- "Other"
centrality_df["Race"][centrality_df["variable"] == "O Neill Gilbert"] <- "Black"
centrality_df["Race"][centrality_df["variable"] == "Brian Jean Mary"] <- "Black"
centrality_df["Race"][centrality_df["variable"] == "Re quan Boyette"] <- "Black"
centrality_df["Race"][centrality_df["variable"] == "J D  Williams"] <- "Black"
centrality_df["Race"][centrality_df["variable"] == "Maurice Crum Jr "] <- "Black"
centrality_df["Race"][centrality_df["variable"] == "Tim Harris Jr "] <- "Black"
centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))
# *****So white coaches are 7 times more influential than Black coaches (eigen) and 1.43 times more connected than Black coaches (degree) on average.*****
centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  summarise(eigen = median(eigen),
            betweenness = median(betweenness),
            closeness = median(closeness),
            degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector
# so now you can adjust the size of the node (for example) according to the degree centrality (aes(size = degree))
# Another 538-style table showing the most and least connected and influential people?
# Let me know what you want in the table here and I can do that

########
# COMPONENTS, COMMUNITIES, & CLIQUES
# Community = a densely connected subset of vertices
# A connected component of a graph is a connected subset of vertices, none of which are connected to any other vertex in the graph. A disconnected graph is comprised of multiple connected components.
# Vertex partitioning occurs to cut/split connected graphs into disconnected subgroups. In a partition, all vertices must be in one and only 1 subgroup.
# Community Detection or Community Discovery = process of determining optimal vertex clusters or communities. (Unsupervised Learning)
# Louvain Algorithm is one type of Community Detection (most common and fastest) - maximizes modularity of the graph. Modularity measures how dense the connections are within subsets of vertices by comparing the density to that which would be expected from a random graph. The higher the modularity, the more connected the vertices are inside the subgroups compared to between the subgroups. 
# ^ basically like clustering but for graphs (with relationships).
# Clique = subset of vertices in an undirected subgraph whose induced subgraph is complete. (Our graph is directed)
# get weakly connected components:
graph_components <- components(graph, mode= "weak")
# how many components? 9
graph_components$no
# size of components? 1 huge one & 8 small ones
graph_components$csize
# assign component property
V(graph)$component <- graph_components$membership
# visualize
ggraph(graph) +
  geom_edge_link(color="grey",
                 arrow = arrow(length=unit(0.2, "cm"))) +
  geom_node_point(size = 2, aes(color = as.factor(component))) + 
  labs(color = "Component") +
  theme_void()
# Partitioning
# Detect communities using Louvain - only works for Undirected communities
set.seed(123)
graph1 <- igraph::graph_from_data_frame(d = coaching_tree1, directed = FALSE, vertices = vertex_df)
communities <- cluster_louvain(graph1)
V(graph1)$community <- membership(communities)
sizes(communities)
# 41 communities of various sizes
# pick 1 community to analyze (34 is just random):
communities[[34]] # selects community #34
community34 <- induced_subgraph(graph1, vids = communities[[34]])
plot(community34, layout = layout_on_sphere(community34))
# test modularity of louvain's partitioning:
modularity(graph1, V(graph1)$community)
# after adding the race attribute to vertices, you can test the modularity of the race attribute and compare (the higher the better)
modularity(graph1, as.integer(as.factor(V(graph1)$Race)))
# so Louvain's community structure is a better indicator of connected coaching subgroups than Race.
# visualize louvain communities
set.seed(123)
louvain_plot <- ggraph(graph1, layout = "kk") +
  geom_edge_link(color = "black") +
  geom_node_point(size = 2, aes(color = as.factor(community)),
                  show.legend = FALSE) +
  theme_void()
louvain_plot
# then visualize divided by race
set.seed(123)
race_plot <- ggraph(graph1, layout = "kk") +
  geom_edge_link(color = "black") +
  geom_node_point(size = 2, aes(color = Race),
                  show.legend = FALSE) +
  theme_void()
race_plot

# plot individual communities grouped by Race. (probably don't want to label these w/ coach's names)
# Bill Snyder's
community <- induced_subgraph(graph1, vids = communities[[23]])
race_plot23 <- ggraph(community, layout = "kk") +
  geom_edge_link(color = "grey") +
  geom_node_point(size = 4, aes(color = Race)) +
  geom_node_text(aes(label = name), repel=T, force=100) +
  theme_void() +
  labs(title = "The Bill Snyder Community")
race_plot23
communities[[23]]
# Nick Saban's
community <- induced_subgraph(graph1, vids = communities[[9]])
race_plot9 <- ggraph(community, layout = "kk") +
  geom_edge_link(color = "grey") +
  geom_node_point(size = 4, aes(color = Race)) +
  geom_node_text(aes(label = name), repel=T, force=100) +
  theme_void() +
  labs(title = "The Nick Saban Community")
race_plot9
communities[[9]]

# Least diverse communities (by %): 2, 11, 24, 28 
# most diverse communities: 3, 18, 35, 41
############################################################################################

# Racial demographics of CFB Football Players (2011 - 2020) (self-reported  data from NCAA member schools' and published at NCAA.org: https://www.ncaa.org/about/resources/research/ncaa-demographics-database)

athletes <- read.csv("racial_demographics_of_cfb_athletes.csv", check.names=FALSE) %>% filter(Race.Ethnicity != "TOTAL")
athletes$Race.Ethnicity <- ifelse(athletes$Race.Ethnicity == "White", "White",
                                  ifelse(athletes$Race.Ethnicity == "Black", "Black", "Other"))
athletes <- athletes %>%
  group_by(Division, Race.Ethnicity) %>%
  summarise(Percent = sum(Percent))
athletes$Race.Ethnicity <- relevel(as.factor(athletes$Race.Ethnicity), 'White')
athletes_plot <- athletes %>%
  ggplot(aes(fill = Race.Ethnicity, y = Percent, x = forcats::fct_rev(Division), label = Percent)) +
  geom_col(position = position_fill(reverse = TRUE), alpha = 0.8, width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(size = 3, position = position_stack(vjust = 0.5, reverse = TRUE)) +
  coord_flip() +
  theme_light() +
  labs(title = "Racial Demographics of CFB Football Players", 
       subtitle = "From 2011 to 2020",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: https://www.ncaa.org/about/resources/research/ncaa-demographics-database") +
  xlab("Division") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("bottom"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9),
        axis.title.x = element_blank())
athletes_plot
ggsave('athletes_plot.png', height = 5.625, width = 10, dpi = 300)

## Racial demographics of CFB COACHES (2011 - 2020) (self-reported  data from NCAA member schools' and published at NCAA.org: https://www.ncaa.org/about/resources/research/ncaa-demographics-database)

coaches <- read.csv("racial_demographics_of_cfb_coaches.csv", check.names=FALSE) %>% filter(Race != "TOTAL")
coaches$Race <- ifelse(coaches$Race == "White", "White",
                       ifelse(coaches$Race == "Black", "Black", "Other"))
coaches <- coaches %>%
  group_by(Position, Race) %>%
  summarise(Percent = sum(Percent))
coaches$Race <- relevel(as.factor(coaches$Race), 'White')
coaches$Position <- factor(as.factor(coaches$Position), levels = c('Head', 'OC', 'DC', 'Assistant', "GA"))
coaches_plot <- coaches %>%
  ggplot(aes(fill = Race, y = Percent, x = forcats::fct_rev(Position), label = Percent)) +
  geom_col(position = position_fill(reverse = TRUE), alpha = 0.8, width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(size = 3, position = position_stack(vjust = 0.5, reverse = TRUE)) +
  coord_flip() +
  theme_light() +
  labs(title = "Racial Demographics of D-I CFB Coaches", 
       subtitle = "From 2011 to 2020",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: https://www.ncaa.org/about/resources/research/ncaa-demographics-database") +
  xlab("Role") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("bottom"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9),
        axis.title.x = element_blank())
coaches_plot
ggsave('coaches_plot.png', height = 5.625, width = 10, dpi = 300)

# what should the coaching demographics actually look like?
ideal_demog <- data.frame(Race = c("White", "Black", "Other"),
                          united_states = c(0.601, 0.122, 0.277),
                          cfb = c(0.5065, 0.3900, 0.1035))
ideal_demog <- ideal_demog %>%
  mutate(ideal_percent = 0.927*cfb + 0.073*united_states) %>%
  select(Race, ideal_percent)
hc_2021 = head_coaches %>%
  filter(Season == 2021) %>% 
  mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>% 
  summarise(num_race = n()) %>%
  mutate(actual_2021 = num_race / sum(num_race)) %>%
  left_join(ideal_demog) %>%
  mutate(num_ideal = round(sum(num_race)*ideal_percent, 0),
         difference = num_ideal - num_race)
oc_2021 = offensive_coordinators %>%
  filter(Season == 2021) %>% 
  mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>% 
  summarise(num_race = n()) %>%
  mutate(actual_2021 = num_race / sum(num_race)) %>%
  left_join(ideal_demog) %>%
  mutate(num_ideal = round(sum(num_race)*ideal_percent, 0),
         difference = num_ideal - num_race)
dc_2021 = defensive_coordinators %>%
  filter(Season == 2021) %>% 
  mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>% 
  summarise(num_race = n()) %>%
  mutate(actual_2021 = num_race / sum(num_race)) %>%
  left_join(ideal_demog) %>%
  mutate(num_ideal = round(sum(num_race)*ideal_percent, 0),
         difference = num_ideal - num_race)
ideal_change <- data.frame(role = c("Head Coach", 
                                    "Offensive Coordinator", 
                                    "Defensive Coordinator"),
                           white = c(hc_2021[hc_2021$Race == "White", "difference"][[1]], 
                                     oc_2021[oc_2021$Race == "White", "difference"][[1]], 
                                     dc_2021[dc_2021$Race == "White", "difference"][[1]]),
                           black = c(hc_2021[hc_2021$Race == "Black", "difference"][[1]], 
                                     oc_2021[oc_2021$Race == "Black", "difference"][[1]], 
                                     dc_2021[dc_2021$Race == "Black", "difference"][[1]]),
                           other = c(hc_2021[hc_2021$Race == "Other", "difference"][[1]], 
                                     oc_2021[oc_2021$Race == "Other", "difference"][[1]], 
                                     dc_2021[dc_2021$Race == "Other", "difference"][[1]])) %>%
  melt(id="role")
ideal_change$role <- factor(as.factor(ideal_change$role), levels = c('Head Coach', 'Offensive Coordinator', 'Defensive Coordinator'))
ideal_plot <- ggplot(data=ideal_change, aes(x = role, y = value, fill = variable, label = value)) +
  geom_col(width = 0.6, alpha=0.8) +
  geom_hline(yintercept=0) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_light() +
  labs(title = "Additional Coaches Needed by Race to Reflect the Demographics of the Candidate Pool", 
       subtitle = "based on 2021 Coaching Staffs",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: U.S. Census Bureau & NCAA.org") +
  ylab("Additional Coaches") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("bottom"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9),
        axis.title.x = element_blank())
ideal_plot
ggsave('ideal_plot.png', height = 5.625, width = 10, dpi = 300)
#############  Time Series Data

head_coach_time_series <- head_coaches %>% group_by(Season) %>% mutate(num_coaches = n()) %>%
  ungroup() %>% group_by(Season, Race) %>% filter(Race == "Black") %>% mutate(percent_black = n()/num_coaches) %>% distinct(Season, .keep_all = TRUE) %>% select(Season, percent_black)
head_coach_time_series <- head_coach_time_series %>% mutate_if(is.numeric, round, digits = 2)
head_coach_race_time_plot <- head_coach_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Head Coaches in the FBS",
       title = "Percentage of Black Head Coaches since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
head_coach_race_time_plot
ggsave('head_coach_race_time_plot.png', height = 7, width = 10, dpi = 300)

oc_time_series <- offensive_coordinators %>% group_by(Season) %>% mutate(num_coaches = n()) %>%
  ungroup() %>% group_by(Season, Race) %>% filter(Race == "Black") %>% mutate(percent_black = n()/num_coaches) %>% distinct(Season, .keep_all = TRUE) %>% select(Season, percent_black)
oc_time_series <- oc_time_series %>% mutate_if(is.numeric, round, digits = 2)
oc_race_time_plot <- oc_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Offensive Coordinators in the FBS",
       title = "Percentage of Black Offensive Coordinators since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
oc_race_time_plot
ggsave('oc_race_time_plot.png', height = 7, width = 10, dpi = 300)

dc_time_series <- defensive_coordinators %>% group_by(Season) %>% mutate(num_coaches = n()) %>%
  ungroup() %>% group_by(Season, Race) %>% filter(Race == "Black") %>% mutate(percent_black = n()/num_coaches) %>% distinct(Season, .keep_all = TRUE) %>% select(Season, percent_black)
dc_time_series <- dc_time_series %>% mutate_if(is.numeric, round, digits = 2)
dc_race_time_plot <- dc_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Defensive Coordinators in the FBS",
       title = "Percentage of Black Defensive Coordinators since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
dc_race_time_plot
ggsave('hdc_race_time_plot.png', height = 7, width = 10, dpi = 300)
#########################################################################################

###############################################################################################

# Time Series
# Exponential Smoothing
# dataset includes player demographics from NCAA Student-Athlete Ethnicity Reports from all Divisions (I, II, III)
player_ts1 <- read.csv("player_race_time_series.csv", check.names=FALSE)
# just look at Black % first
player_ts1 <- player_ts1 %>% filter(Race == "Black")
# create a vector of this data
player_ts_vec <- as.vector(unlist(player_ts1[,2:22]))
# turn the vector into a time series object
player_ts <- ts(player_ts_vec,start=2000,frequency=1)
# Double exponential smoothing: Holt-Winters exponential smoothing with trend and without seasonal component.
es1 <- HoltWinters(player_ts,gamma=FALSE)
es1
# If beta and b are very close to 0 that means there's not a significant trend. Beta is not very close to 0.
plot(es1)
plot(fitted(es1))

player_ts1 <- player_ts1 %>%
  melt(variable.name = "Season",
       value.name = "percent_black") %>%
  mutate(Person = "Player")
player_ts1$Season <- as.numeric(as.character(player_ts1$Season))

coach_time_series <- head_coaches %>% 
  mutate(Role = "Head Coach") %>%
  rbind(offensive_coordinators %>%
          mutate(Role = "Offensive Coordinator")) %>%
  rbind(defensive_coordinators %>%
          mutate(Role = "Defensive Coordinator")) %>%
  group_by(Season) %>% 
  mutate(num_coaches = n()) %>%
  ungroup() %>% 
  group_by(Season, Race) %>% 
  filter(Race == "Black") %>% 
  mutate(percent_black = n()/num_coaches) %>% 
  distinct(Season, .keep_all = TRUE) %>% 
  select(Season, percent_black) %>%
  mutate(Person = "Coach") %>%
  rbind(player_ts1)
df1 <- coach_time_series %>%
  filter(Season == 2000 | Season == 2020)
df<- data.frame(x1 = 2000, x2 = 2000, x3 = 2020, x4 = 2020,
                y1 = df1[df1$Person == "Player" & df1$Season == 2000, "percent_black"][[1]],
                y2 = df1[df1$Person == "Coach" & df1$Season == 2000, "percent_black"][[1]],
                y3 = df1[df1$Person == "Player" & df1$Season == 2020, "percent_black"][[1]],
                y4 = df1[df1$Person == "Coach" & df1$Season == 2020, "percent_black"][[1]])
hc_2021[hc_2021$Race == "White", "difference"][[1]]
# coach_time_series <- coach_time_series %>% mutate_if(is.numeric, round, digits = 2)
coach_race_time_plot <- coach_time_series %>% 
  ggplot(aes(x=Season, y=percent_black, color = Person)) +
  geom_line(stat="identity", size=1.5)+
  geom_point(size=2.5)+
  scale_colour_brewer(palette = "Set2") +
  geom_segment(aes(x=df$x1, y=df$y1, xend=df$x2, yend=df$y2), linetype = "dashed", color="black") +
  geom_segment(aes(x=df$x3, y=df$y3, xend=df$x4, yend=df$y4), linetype = "dashed", color="black") +
  geom_text(aes(x=df$x1-0.2, y=(df$y1-df$y2)/2+df$y2), label = round(df$y1-df$y2,3), angle=90, color="black", size=3) +
  geom_text(aes(x=df$x3-0.2, y=(df$y3-df$y4)/2+df$y4), label = round(df$y3-df$y4,3), angle=90, color="black", size=3) +
  labs(x = "Season", y= "Percentage",
       title = "% of Black Coaches in FBS vs. Black Players in CFB",
       caption = "Plot: @markwood14 and @robert_binion\nData: Team Info Pages & NCAA.org") +
  theme_light() +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"))
coach_race_time_plot
ggsave('coach_race_time_plot.png', height = 5.625, width = 10, dpi = 300)

oc_time_series <- offensive_coordinators %>% group_by(Season) %>% mutate(num_coaches = n()) %>%
  ungroup() %>% group_by(Season, Race) %>% filter(Race == "Black") %>% mutate(percent_black = n()/num_coaches) %>% distinct(Season, .keep_all = TRUE) %>% select(Season, percent_black)
oc_time_series <- oc_time_series %>% mutate_if(is.numeric, round, digits = 2)
oc_race_time_plot <- oc_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Offensive Coordinators in the FBS",
       title = "Percentage of Black Offensive Coordinators since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
oc_race_time_plot
ggsave('oc_race_time_plot.png', height = 7, width = 10, dpi = 300)

dc_time_series <- defensive_coordinators %>% group_by(Season) %>% mutate(num_coaches = n()) %>%
  ungroup() %>% group_by(Season, Race) %>% filter(Race == "Black") %>% mutate(percent_black = n()/num_coaches) %>% distinct(Season, .keep_all = TRUE) %>% select(Season, percent_black)
dc_time_series <- dc_time_series %>% mutate_if(is.numeric, round, digits = 2)
dc_race_time_plot <- dc_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Defensive Coordinators in the FBS",
       title = "Percentage of Black Defensive Coordinators since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
dc_race_time_plot
ggsave('hdc_race_time_plot.png', height = 7, width = 10, dpi = 300)
