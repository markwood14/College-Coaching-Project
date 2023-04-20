# Install & load packages
packages <- c("remotes", "Rcpp", "dplyr", "readr", "tidyverse", "devtools", "cfbfastR", "gt", "ggimage", "ggeasy", "reshape2", "purrr", "parallel", "future", "data.table", "stringr", "ggpubr", "RColorBrewer", "igraph", "qgraph", "corrplot", "Hmisc", "ggraph", "networkD3", "kknn", "JOUSBoost", "xgboost", "kernlab", "neuralnet", "stats", "ggbiplot", "RCurl")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Packages for Louvain network analysis: igraph, qgraph, corrplot, Hmisc, ggraph, networkD3
# packages for classification/prediction: kknn, JOUSBoost, xgboost, kernlab, neuralnet, stats, ggbiplot

##########################################################################################
# Set up the primary dataframes:

# Read and clean the coaches dataset

file_url <- "https://github.com/rebinion/College-Coaching-Project/blob/main/coach_df1.Rda?raw=true"
load(url(file_url))

coach_df1 <- coach_df1 %>% 
  dplyr::mutate(College = ifelse(College == "FAU", "Florida Atlantic",
                                 ifelse(College == "FIU", "Florida International",
                                        ifelse(College == "Hawaii", "Hawai'i",
                                               ifelse(College == "Massachusetts", "UMass", 
                                                      ifelse(College == "Miami (FL)", "Miami",
                                                             ifelse(College == "San Jose State", "San José State",
                                                                    ifelse(College == "Southern Miss", "Southern Mississippi",
                                                                           ifelse(College == "UConn", "Connecticut",
                                                                                  ifelse(College == "UL Monroe", "Louisiana Monroe",
                                                                                         ifelse(College == "USF", "South Florida",
                                                                                                ifelse(College == "UTSA", "UT San Antonio",
                                                                                                       College))))))))))))
coach_df <- coach_df1 %>%
  filter(Race != "#N/A") %>%
  group_by(Coach, College, Role) %>%
  dplyr::mutate(year_start = min(Season),
                       year_end = max(Season)) %>%
  select(c(College, Coach, Role, Race, year_start, year_end)) %>%
  distinct()
# save(coach_df,file="coach_df.Rda")

coordinators <- coach_df %>% filter(str_detect(Role, "Coordinator"))

# the following dataframes have one row per tenure with year_start and year_end
head_coaches <- coach_df %>% 
  filter(str_detect(Role, "Head")) %>% 
  filter(!str_detect(Role, "Associate Head")) %>% 
  filter(!str_detect(Role, "Assistant Head")) %>% 
  filter(!str_detect(Role, "Interim Head"))
offensive_coordinators <- coordinators %>% 
  filter(str_detect(Role, "Offensive")) %>% 
  filter(!str_detect(Role, "Run Game Coordinator")) %>% 
  filter(!str_detect(Role, "Pass Game Coordinator")) %>% 
  filter(!str_detect(Role, "Associate offensive")) %>% 
  filter(!str_detect(Role, "Special Teams")) %>% 
  filter(!str_detect(Role, "Assistant offensive")) %>% 
  filter(!str_detect(Role, "Offensive Recruiting Coordinator")) %>% 
  filter(!str_detect(Role, "Head Coach"))
defensive_coordinators <- coordinators %>% 
  filter(str_detect(Role, "Defensive")) %>% 
  filter(!str_detect(Role, "Associate Defensive")) %>% 
  filter(!str_detect(Role, "Special Teams")) %>% 
  filter(!str_detect(Role, "Assistant Defensive")) %>% 
  filter(!str_detect(Role, "Recruiting Coordinator")) %>% 
  filter(!str_detect(Role, "Pass Game Coordinator")) %>%
  filter(!str_detect(Role, "Run Game Coordinator")) %>%
  filter(!str_detect(Role, "Head Coach"))

# Currently each DF has separate rows (splits up tenures) when a coach, for example, goes from "Head Coach/Offensive Coordinator" to just Head Coach or "Offensive Coordinator/WR Coach" to just Offensive Coordinator. The following combines those tenures into 1 row..
head_coaches <- head_coaches %>% 
  group_by(College, Coach) %>%
  dplyr::mutate(year_start = min(year_start), 
                year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)
defensive_coordinators <- defensive_coordinators %>%
  group_by(College, Coach) %>%
  dplyr::mutate(year_start = min(year_start),
                year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)
offensive_coordinators <- offensive_coordinators %>%
  group_by(College, Coach) %>%
  dplyr::mutate(year_start = min(year_start),
                year_end = max(year_end)) %>%
  distinct(College, Coach, Race, year_start, year_end, .keep_all = TRUE)
# updating a few of the Race entries
offensive_coordinators <- offensive_coordinators %>%
  dplyr::mutate(Race = ifelse(Coach == "Billy Gonzales", "Other",
                              ifelse(Coach == "Ron Prince", "Black",
                                     Race)))
defensive_coordinators <- defensive_coordinators %>%
  dplyr::mutate(Race = ifelse(Coach == "Phil Elmassian", "White",
                              ifelse(Coach == "Chris Simpson", "Black",
                                     ifelse(Coach == "John Chavis", "White",
                                            ifelse(Coach == "John Papuchis", "White",
                                                   ifelse(Coach == "Justin Ena", "White",
                                                          ifelse(Coach == "Justin Hamilton", "White",
                                                                 Race)))))))

# Coordinators who became head coaches:
offensive_to_head <- offensive_coordinators %>% 
  inner_join(head_coaches, by = "Coach") %>% 
  distinct(Coach, .keep_all = TRUE) %>%
  select(College.x, Coach, Race.x, College.y)
defensive_to_head <- defensive_coordinators %>% 
  inner_join(head_coaches, by = "Coach") %>% 
  distinct(Coach, .keep_all = TRUE) %>% 
  select(College.x, Coach, Race.x, College.y)
colnames(defensive_to_head) <- c("Coordinator_School", "Coach", "Race", "Head_Coach_School")
colnames(offensive_to_head) <- c("Coordinator_School", "Coach", "Race", "Head_Coach_School")

# filtering to only coaches where we will have before/after data
head_coaches_recent <- head_coaches %>% filter(year_start >= 2006)
oc_recent <- offensive_coordinators %>% filter(year_start >= 2006)
dc_recent <- defensive_coordinators %>% filter(year_start >= 2006)

# just to get a basic idea of sample size:
head_coaches_recent %>% group_by(Race) %>% dplyr::summarise(num_race = dplyr::n())

### The following dataframes have 1 row per year rather than 1 row per tenure.

coordinators1 <- coach_df1 %>% filter(str_detect(Role, "Coordinator"))

head_coaches1 <- coach_df1 %>% 
  filter(str_detect(Role, "Head")) %>% 
  filter(!str_detect(Role, "Associate Head")) %>% 
  filter(!str_detect(Role, "Assistant Head")) %>% 
  filter(!str_detect(Role, "Interim Head"))
offensive_coordinators1 <- coordinators1 %>% 
  filter(str_detect(Role, "Offensive")) %>% 
  filter(!str_detect(Role, "Run Game Coordinator")) %>% 
  filter(!str_detect(Role, "Pass Game Coordinator")) %>% 
  filter(!str_detect(Role, "Associate offensive")) %>% 
  filter(!str_detect(Role, "Special Teams")) %>% 
  filter(!str_detect(Role, "Assistant offensive")) %>% 
  filter(!str_detect(Role, "Offensive Recruiting Coordinator")) %>% 
  filter(!str_detect(Role, "Head Coach"))
defensive_coordinators1 <- coordinators1 %>% 
  filter(str_detect(Role, "Defensive")) %>% 
  filter(!str_detect(Role, "Associate Defensive")) %>% 
  filter(!str_detect(Role, "Special Teams")) %>% 
  filter(!str_detect(Role, "Assistant Defensive")) %>% 
  filter(!str_detect(Role, "Recruiting Coordinator")) %>% 
  filter(!str_detect(Role, "Pass Game Coordinator")) %>%
  filter(!str_detect(Role, "Run Game Coordinator")) %>%
  filter(!str_detect(Role, "Head Coach"))

head_coaches1 <- head_coaches1 %>%
  dplyr::mutate(Race = ifelse(Coach == "Ron Prince", "Black", Race))
offensive_coordinators1 <- offensive_coordinators1 %>%
  dplyr::mutate(Race = ifelse(Coach == "Billy Gonzales", "Other",
                              ifelse(Coach == "Ron Prince", "Black",
                                     Race)))
defensive_coordinators1 <- defensive_coordinators1 %>%
  dplyr::mutate(Race = ifelse(Coach == "Phil Elmassian", "White",
                              ifelse(Coach == "Chris Simpson", "Black",
                                     ifelse(Coach == "John Chavis", "White",
                                            ifelse(Coach == "John Papuchis", "White",
                                                   ifelse(Coach == "Justin Ena", "White",
                                                          ifelse(Coach == "Justin Hamilton", "White",
                                                                 Race)))))))
head_coaches %>% group_by(Race) %>% dplyr::summarise(num_race = n())
head_coaches1 %>% group_by(Race) %>% dplyr::summarise(num_race = n())


##########################################################################################
# High Level Analysis using the '1 row per year' dfs
coach_df1 %>% group_by(Race) %>% dplyr::summarise(num_race = n())

head_coaches1 %>% group_by(Race) %>% dplyr::summarise(num_race = n())
#   Race  num_race
#2 Black       219
#3 Other        44
#4 White      2423
defensive_coordinators1 %>% group_by(Race) %>% dplyr::summarise(num_race = n())
#  Race  num_race
#2 Black      414
#3 Other       43
#4 White     2415
offensive_coordinators1 %>% group_by(Race) %>% dplyr::summarise(num_race = n())
#Race  num_race
#2 Black      244
#3 Other       31
#4 White     2484

##########################################################################################
# How long do coaches stay in their roles?

tenure <- head_coaches %>%
  dplyr::mutate(Role = "Head Coach") %>%
  rbind(offensive_coordinators %>%
          dplyr::mutate(Role = "Offensive Coordinator")) %>%
  rbind(defensive_coordinators %>%
          dplyr::mutate(Role = "Defensive Coordinator")) %>%
  dplyr::mutate(Race = ifelse(Race == "?", "Other", Race)) %>%
  dplyr::mutate(duration = year_end - year_start+1)
tenure$Race <- factor(as.factor(tenure$Race), levels = c('White', 'Black', 'Other'))
tenure$Role <- factor(as.factor(tenure$Role), levels = c('Head Coach', 'Offensive Coordinator', 'Defensive Coordinator'))
# Ken Niumatalolo and his OC, Ivin Jasper, are the 2 Other/Black coaches w/ 14 years. Followed by David Shaw, Stanford HC, 11 years; Brian Stewart, Maryland DC, 10 years; John Chavis, Tennessee DC, 9 years; James Franklin, Penn State HC, 8 years.
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
    legend.position = c(0.89, 0.85),
    legend.box.background = element_rect(color="black"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.subtitle = element_text(size=9),
    axis.title.x = element_blank())
tenure_jitter
# ggsave('tenure_jitter.png', height = 5.625, width = 10, dpi = 300)

tenure$Role <- as.character(tenure$Role)
tenure$Race <- as.character(tenure$Race)
tenure %>%
  dplyr::mutate(Role = ifelse(Role=="Offensive Coordinator","Coordinator",
                              ifelse(Role=="Defensive Coordinator","Coordinator",Role))) %>%
  group_by(Race, Role) %>% 
  dplyr::summarise(duration = mean(duration))
# Race  Role        duration
# 1 Black Coordinator     2.25
# 2 Black Head Coach      3.41
# 3 Other Coordinator     2.23
# 4 Other Head Coach      4.9 
# 5 White Coordinator     2.73
# 6 White Head Coach      4.97

# Black coaches have shorter tenures than their white counterparts at every level. Why?

##########################################################################################
# HEAD COACHES
# Analysis of impact on on-field performance of Head Coaches by Race




# FOR loop that will create a huge before/after stat dataframe

### ***** ###
### THIS for LOOP CAN TAKE OVER AN HOUR TO RUN. THE RESULTING DATAFRAME CAN BE LOADED FROM GITHUB USING THE FOLLOWING 2 LINES OF CODE:  
### ***** ###
temp <- "https://github.com/rebinion/College-Coaching-Project/blob/main/head_coach_impact.Rda?raw=true"
load(url(temp))

# create an empty df that we will use to add rows to throughout
# head_coach_impact <- data.frame()
# head_coaches_recent <- head_coaches_recent[!(head_coaches_recent$Coach=="Brad Lambert" | head_coaches_recent$Coach=="Joe Moglia" |head_coaches_recent$Coach=="Larry Coker"),]
# 
# for(i in 1:nrow(head_coaches_recent)){
#   # create a vector of years from start to end - done
#   
#   start_year <- as.integer(head_coaches_recent[i, "year_start"])
#   if (start_year<2005){start_year<-2005}
#   end_year <- as.integer(head_coaches_recent[i,6])
#   years <-start_year:end_year
#   
#   # pull the team name - done
#   team <- toString(head_coaches_recent[i, "College"])
#   
#   # pull the coach's name - done
#   coach <- toString(head_coaches_recent[i, "Coach"])
#   # pull the coach's race - done
#   race <- toString(head_coaches_recent[i, "Race"])
#   
#   # get the advanced stats history - done
#   
#   team_advanced <- data.frame()
#   num_years <- length(years)
#   
#   for(year in years){
#     progressr::with_progress({
#       future::plan("multisession")
#       team_advanced <- team_advanced %>% dplyr::bind_rows(
#         cfbd_stats_season_advanced(year = year, team = team))
#       
#     })
#   }
#   
#   # get the FPI ratings - done
#   
#   team_FPI <- data.frame()
#   for(year in years){
#     fpi_row <- cfbfastR:::espn_ratings_fpi(year=year)%>% filter(name == team) %>% select(fpi) %>%as.double() %>% set_names(c("fpi")) 
#     team_FPI <- team_FPI %>% bind_rows(fpi_row)
#   }
#   
#   # join the advanced stats and FPI to the head_coach_impact df, binding new rows - done
#   
#   for(j in 1:nrow(team_advanced)){
#     row_to_add <- bind_cols(c(coach), team_advanced[j,], team_FPI[j,], c(race), c("after"))
#     colnames(row_to_add)[1] <- toString("Coach")
#     colnames(row_to_add)[83] <- toString("FPI_Rating")
#     colnames(row_to_add)[84] <- toString("Race")
#     colnames(row_to_add)[85] <- toString("BeforeAfter")
#     
#     head_coach_impact <- head_coach_impact %>% bind_rows(row_to_add)
#   }
#   
#   
#   # create a vector of previous years for comparison, will mark data for these years as "before" - done
#   
#   previous_years <- (years[1] - 3):(years[1]-1)
#   
#   # checking to make sure that we have data for the years and adjusting the years vector - done
#   if(previous_years[1] < 2005){
#     previous_years <-2005:tail(previous_years, 1)
#   }
#   
#   # repeat to get advanced stats and FPI and then join to head_coach_impact- done
#   
#   # get the before advanced stats history - done
#   
#   num_years <- length(previous_years)
#   team_advanced <- data.frame()
#   
#   for(year in previous_years){
#     team_advanced <- team_advanced %>% dplyr::bind_rows(
#       cfbd_stats_season_advanced(year = year, team = team))
#   }
#   # get the FPI ratings - done
#   
#   team_FPI <- data.frame()
#   for(year in previous_years){
#     fpi_row <- cfbfastR:::espn_ratings_fpi(year=year)%>% filter(name == team) %>% select(fpi) %>%as.double() %>% set_names(c("fpi")) 
#     team_FPI <- team_FPI %>% bind_rows(fpi_row)
#   }
#   
#   # join the advanced stats and FPI to the head_coach_impact df, binding new rows - done
#   
#   for(j in 1:nrow(team_advanced)){
#     row_to_add <- bind_cols(c(coach), team_advanced[j,], team_FPI[j,], c(race), c("before"))
#     colnames(row_to_add)[1] <- toString("Coach")
#     colnames(row_to_add)[83] <- toString("FPI_Rating")
#     colnames(row_to_add)[84] <- toString("Race")
#     colnames(row_to_add)[85] <- toString("BeforeAfter")
#     head_coach_impact <- head_coach_impact %>% bind_rows(row_to_add)
#   }
#   
# }
# save(head_coach_impact, file="head_coach_impact.Rda")
# load("head_coach_impact.Rda")


head_coach_impact_weighted <- head_coach_impact %>% filter(BeforeAfter == "after") %>% group_by(Coach, team) %>%
  dplyr::summarise(tenure_length = n())
head_coach_impact_weighted <- head_coach_impact_weighted[!(head_coach_impact_weighted$Coach=="Brad Lambert" | head_coach_impact_weighted$Coach=="Joe Moglia" |head_coach_impact_weighted$Coach=="Larry Coker" ),]
head_coach_impact_summary <- head_coach_impact %>% select(c("Coach", "team", "off_ppa", "off_success_rate", "off_stuff_rate", "off_passing_plays_success_rate",
                                                            "def_ppa", "def_success_rate", "def_stuff_rate", "def_passing_plays_success_rate", "FPI_Rating", "Race", "BeforeAfter"))

head_coach_impact_summary <- head_coach_impact_summary %>% group_by(Coach, team, Race, BeforeAfter) %>% dplyr::summarise(
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

# Calculating the net (offense-defense after-before) impact on PPA, SR, Stuff, Pass SR, FPI

### ***** ###
### THIS while LOOP CAN TAKE SEVERAL MINUTES TO RUN. THE RESULTING DATAFRAME CAN BE LOADED FROM GITHUB USING THE FOLLOWING 2 LINES OF CODE:  
### ***** ###
temp <- "https://github.com/rebinion/College-Coaching-Project/blob/main/head_coach_impact_results.Rda?raw=true"
load(url(temp))

# head_coach_impact_results <- data.frame()
# i <- 1
# # this while loop takes about a minute to run
# while (i < nrow(head_coach_impact_summary)){
#   head_coach_results <- data.frame()
#   row_to_add <- data.frame()
#   head_coach_results <- head_coach_impact_summary[i,] %>% group_by(Coach, team, Race) %>% 
#     dplyr::summarise(net_ppa = head_coach_impact_summary[i,"mean_ppa"]-head_coach_impact_summary[i+1, "mean_ppa"] - head_coach_impact_summary[i,"mean_defppa"]+head_coach_impact_summary[i+1, "mean_defppa"],
#                      net_sr = head_coach_impact_summary[i,"mean_sr"]-head_coach_impact_summary[i+1, "mean_sr"] - head_coach_impact_summary[i,"mean_defsr"]+head_coach_impact_summary[i+1, "mean_defsr"],
#                      net_stuff = head_coach_impact_summary[i,"mean_stuff"]-head_coach_impact_summary[i+1, "mean_stuff"] - head_coach_impact_summary[i,"mean_defstuff"]+head_coach_impact_summary[i+1, "mean_defstuff"],
#                      net_pass_sr = head_coach_impact_summary[i,"mean_pass_sr"]-head_coach_impact_summary[i+1, "mean_pass_sr"] - head_coach_impact_summary[i,"mean_defpasssr"]+head_coach_impact_summary[i+1, "mean_defpasssr"],
#                      net_fpi = head_coach_impact_summary[i,"mean_fpi"]-head_coach_impact_summary[i+1, "mean_fpi"])
#   head_coach_impact_results <- head_coach_impact_results %>% bind_rows(head_coach_results)
#   i=i+2
# }
# 
# colnames(head_coach_impact_results) <- c(toString("Coach"), "Team", "Race", "Net_PPA", "Net_SR", "Net_Stuff_Rate", "Net_Pass_SR", "Net_FPI")
# save(head_coach_impact_results, file="head_coach_impact_results.Rda")
# load("head_coach_impact_results.Rda")


# add tenure length to this Df so we can weight

head_coach_tenures <- data.frame(head_coach_impact_weighted$tenure_length)
head_coach_impact_results <- head_coach_impact_results %>% bind_cols(head_coach_tenures)

# Fixing issue where individual columns are lists
head_coach_impact_results_test <- lapply(head_coach_impact_results, unlist)
head_coach_impact_results_test <- data.frame(lapply(head_coach_impact_results_test, `length<-`, max(lengths(head_coach_impact_results_test))))
head_coach_impact_results <- head_coach_impact_results_test
colnames(head_coach_impact_results) <- c(toString("Coach"), "Team", "Race", "Net_PPA", "Net_SR", "Net_Stuff_Rate", "Net_Pass_SR", "Net_FPI", "Tenure")
# adding in cols that multiple tenure by pp metrics
head_coach_impact_results <- head_coach_impact_results %>% dplyr::mutate(total_sr = Net_SR*Tenure, total_stuffs = Net_Stuff_Rate*Tenure, total_pass_sr = Net_Pass_SR*Tenure)
head_coach_impact_results <- head_coach_impact_results %>% dplyr::mutate(total_ppa = Net_PPA*Tenure)
# doing some preliminary analysis
head_coach_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
#   Race  mean_net_ppa_race
#  1 ?              -0.0222 
#2 Black          -0.0292 
#3 Other           0.00167
#4 White           0.0106 

head_coach_impact_results_test <- lapply(head_coach_impact_results, unlist)
head_coach_impact_results_test <- data.frame(lapply(head_coach_impact_results_test, `length<-`, max(lengths(head_coach_impact_results_test))))
head_coach_impact_results <- head_coach_impact_results_test
head_coach_impact_results["Race"][head_coach_impact_results["Race"] == "?"] <- "Other" 

# CHECK FOR NORMALITY AND OUTLIERS
hist(head_coach_impact_results$Net_PPA,
     xlab = "Net PPA",
     main = "Histogram of Net PPA")
# data seems to be normality distributed
boxplot(head_coach_impact_results$Net_PPA,
        ylab = "Net PPA",
        main = "Boxplot of Net PPA")
# remove the outliers
out_vals <- boxplot.stats(head_coach_impact_results$Net_PPA)$out
out_inds <- which(head_coach_impact_results$Net_PPA %in% c(out_vals))
out_inds
head_coach_impact_results <- head_coach_impact_results[-c(out_inds),]
ggqqplot(head_coach_impact_results$Net_PPA)
head_coach_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
head_coach_impact_results %>% group_by(Race) %>% dplyr::summarise(count = n())
# analyze via simple linear regression
lm1 <- lm(Net_PPA ~ Race, head_coach_impact_results)
summary(lm1)
# NO SIGNIFICANT VARIABLES (no P-values less than 0.05)

# check the total metric for normality and outliers
hist(head_coach_impact_results$total_ppa,
     xlab = "Total PPA",
     main = "Histogram of Total PPA")
# data has a decent right tail
boxplot(head_coach_impact_results$total_ppa,
        ylab = "Total PPA",
        main = "Boxplot of Total PPA")
# remove the outliers
out_vals <- boxplot.stats(head_coach_impact_results$total_ppa)$out
out_inds <- which(head_coach_impact_results$total_ppa %in% c(out_vals))
out_inds
head_coach_impact_results_weighted <- head_coach_impact_results[-c(out_inds),]
ggqqplot(head_coach_impact_results_weighted$total_ppa)
head_coach_impact_results_weighted %>% group_by(Race) %>% dplyr::summarise(total_net_ppa_race = mean(total_ppa))
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

head_coach_impact_plot <- head_coach_impact_results %>% ggplot(aes(x = Net_SR, y = Net_PPA, colour = Race)) +
  geom_point(size = 1.5, alpha=0.7) +
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
# ggsave("head_coach_impact_plot.png", height = 7, width = 10, dpi = 300)

head_coach_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_total_ppa_race
# 2 Black             -0.0189
# 3 Other             -0.00311
# 4 White              0.00763 

####################################################################################
# OFFENSIVE COORDINATORS
# Repeat of Analysis of impact on on-field performance for Offensive Coordinators by Race
# pull offensive advanced stats


# for loop that will create a huge before/after stat dataframe

### ***** ###
### THIS for LOOP CAN TAKE AROUND 2 HOURS TO RUN. THE RESULTING DATAFRAME CAN BE LOADED FROM GITHUB USING THE FOLLOWING 2 LINES OF CODE:  
### ***** ###
temp <- "https://github.com/rebinion/College-Coaching-Project/blob/main/oc_impact.Rda?raw=true"
load(url(temp))


# oc_recent <- oc_recent[!(oc_recent$Coach=="Jeff Mullen" | oc_recent$Coach=="Greg Adkins" |oc_recent$Coach=="Kevin Brown" ),]
# creating an empty df that we will use to add rows to throughout
# oc_impact <- data.frame()

# for(i in 1:nrow(oc_recent)){
#   # create a vector of years from start to end - done
#   
#   start_year <- as.integer(oc_recent[i, "year_start"])
#   if (start_year<2005){start_year<-2005}
#   end_year <- as.integer(oc_recent[i,6])
#   years <-start_year:end_year
#   
#   # pull the team name - done
#   team <- toString(oc_recent[i, "College"])
#   
#   # pull the coach's name - done
#   coach <- toString(oc_recent[i, "Coach"])
#   # pull the coach's race - done
#   race <- toString(oc_recent[i, "Race"])
#   
#   # get the advanced stats history - done
#   
#   team_advanced <- data.frame()
#   num_years <- length(years)
#   
#   for(year in years){
#     progressr::with_progress({
#       future::plan("multisession")
#       team_advanced <- team_advanced %>% dplyr::bind_rows(
#         cfbd_stats_season_advanced(year = year, team = team))
#       
#     })
#   }
#   
#   # join the advanced stats and FPI to the oc_impact df, binding new rows - done
#   
#   for(j in 1:nrow(team_advanced)){
#     row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("after"))
#     colnames(row_to_add)[1] <- toString("Coach")
#     colnames(row_to_add)[83] <- toString("Race")
#     colnames(row_to_add)[84] <- toString("BeforeAfter")
#     
#     oc_impact <- oc_impact %>% bind_rows(row_to_add)
#   }
#   
#   
#   # create a vector of previous years for comparison, will mark data for these years as "before" - done
#   
#   previous_years <- (years[1] - 3):(years[1]-1)
#   
#   # checking to make sure that we have data for the years and adjusting the years vector - done
#   if(previous_years[1] < 2005){
#     previous_years <-2005:tail(previous_years, 1)
#   }
#   
#   # repeat to get advanced stats and then join to oc_impact- done
#   
#   # get the before advanced stats history - done
#   
#   num_years <- length(previous_years)
#   team_advanced <- data.frame()
#   
#   for(year in previous_years){
#     team_advanced <- team_advanced %>% dplyr::bind_rows(
#       cfbd_stats_season_advanced(year = year, team = team))
#   }
#   
#   # join the advanced stats to the oc_impact df, binding new rows - done
#   
#   for(j in 1:nrow(team_advanced)){
#     row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("before"))
#     colnames(row_to_add)[1] <- toString("Coach")
#     colnames(row_to_add)[83] <- toString("Race")
#     colnames(row_to_add)[84] <- toString("BeforeAfter")
#     oc_impact <- oc_impact %>% bind_rows(row_to_add)
#   }
#   
# }
# save(oc_impact, file="oc_impact.Rda")
# load("oc_impact.Rda")

oc_impact_summary <-oc_impact %>% select(c("Coach", "team", "off_ppa", "off_success_rate", "off_stuff_rate", "off_passing_plays_success_rate", "Race", "BeforeAfter"))

oc_impact_summary <- oc_impact_summary %>% 
  group_by(Coach, team, Race, BeforeAfter) %>% 
  dplyr::summarise(mean_ppa = mean(off_ppa), 
                   mean_sr = mean(off_success_rate),
                   mean_stuff = mean(off_stuff_rate),
                   mean_pass_sr = mean(off_passing_plays_success_rate)
  )


# Calculating the net (offense-defense after-before) impact on PPA, SR, Stuff, Pass SR, FPI

oc_impact_results <- data.frame()
i <- 1
while (i < nrow(oc_impact_summary)){
  oc_results <- data.frame()
  row_to_add <- data.frame()
  oc_results <- oc_impact_summary[i,] %>% group_by(Coach, team, Race) %>% 
    dplyr::summarise(net_ppa = oc_impact_summary[i,"mean_ppa"]-oc_impact_summary[i+1, "mean_ppa"],
                     net_sr = oc_impact_summary[i,"mean_sr"]-oc_impact_summary[i+1, "mean_sr"],
                     net_stuff = oc_impact_summary[i,"mean_stuff"]-oc_impact_summary[i+1, "mean_stuff"],
                     net_pass_sr = oc_impact_summary[i,"mean_pass_sr"]-oc_impact_summary[i+1, "mean_pass_sr"])
  oc_impact_results <- oc_impact_results %>% bind_rows(oc_results)
  i=i+2
}

# save(oc_impact_results, file="oc_impact_results.Rda")
# load("oc_impact_results.Rda")

# Fixing issue where individual columns are lists
oc_impact_results_test <- lapply(oc_impact_results, unlist)
oc_impact_results_test <- data.frame(lapply(oc_impact_results_test, `length<-`, max(lengths(oc_impact_results_test))))
oc_impact_results <- oc_impact_results_test

# doing some preliminary analysis
oc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(net_ppa))
#Race  mean_net_ppa_race
#  1 ?              0.0380
#2 Black            0.0114
#3 Other           -0.0145
#4 White            0.0259 

# CHECK FOR NORMALITY & OUTLIERS
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
# 8 outliers to remove
oc_impact_results <- oc_impact_results[-c(out_inds),]
ggqqplot(oc_impact_results$net_ppa)
oc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(net_ppa))

# analyze via simple linear regression
lm1 <- lm(net_ppa ~ Race, oc_impact_results)
summary(lm1)
# NO SIGNIFICANT VARIABLES - in other words, Race is not a valid predictor of on-field performance.

oc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_sr_race = mean(net_sr))
oc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_passsr_race = mean(net_pass_sr))

##########################################################################################
# DEFENSIVE COORDINATORS
# Repeat of Analysis of impact on on-field performance for Defensive Coordinators by Race
# pull defensive advanced stats


# for loop that will create a huge before/after stat dataframe

### ***** ###
### THIS for LOOP CAN TAKE OVER AN HOUR TO RUN. THE RESULTING DATAFRAME CAN BE LOADED FROM GITHUB USING THE FOLLOWING 2 LINES OF CODE:  
### ***** ###
temp <- "https://github.com/rebinion/College-Coaching-Project/blob/main/dc_impact.Rda?raw=true"
load(url(temp))


# dc_recent <- dc_recent[!(dc_recent$Coach=="Matt Wallerstedt" | dc_recent$Coach=="Mickey Matthews" |dc_recent$Coach=="Neal Neathery" ),]
# creating an empty df that we will use to add rows to throughout
# dc_impact <- data.frame()

# for(i in 1:nrow(dc_recent)){
#   # create a vector of years from start to end - done
#   
#   start_year <- as.integer(dc_recent[i, "year_start"])
#   if (start_year<2005){start_year<-2005}
#   end_year <- as.integer(dc_recent[i,6])
#   years <-start_year:end_year
#   
#   # pull the team name - done
#   team <- toString(dc_recent[i, "College"])
#   
#   # pull the coach's name - done
#   coach <- toString(dc_recent[i, "Coach"])
#   # pull the coach's race - done
#   race <- toString(dc_recent[i, "Race"])
#   
#   # get the advanced stats history - done
#   
#   team_advanced <- data.frame()
#   num_years <- length(years)
#   
#   for(year in years){
#     progressr::with_progress({
#       future::plan("multisession")
#       team_advanced <- team_advanced %>% dplyr::bind_rows(
#         cfbd_stats_season_advanced(year = year, team = team))
#       
#     })
#   }
#   
#   # join the advanced stats and FPI to the oc_impact df, binding new rows - done
#   
#   for(j in 1:nrow(team_advanced)){
#     row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("after"))
#     colnames(row_to_add)[1] <- toString("Coach")
#     colnames(row_to_add)[83] <- toString("Race")
#     colnames(row_to_add)[84] <- toString("BeforeAfter")
#     
#     dc_impact <- dc_impact %>% bind_rows(row_to_add)
#   }
#   
#   
#   # create a vector of previous years for comparison, will mark data for these years as "before" - done
#   
#   previous_years <- (years[1] - 3):(years[1]-1)
#   
#   # checking to make sure that we have data for the years and adjusting the years vector - done
#   if(previous_years[1] < 2005){
#     previous_years <-2005:tail(previous_years, 1)
#   }
#   
#   # repeat to get advanced stats and then join to oc_impact- done
#   
#   # get the before advanced stats history - done
#   
#   num_years <- length(previous_years)
#   team_advanced <- data.frame()
#   
#   for(year in previous_years){
#     team_advanced <- team_advanced %>% dplyr::bind_rows(
#       cfbd_stats_season_advanced(year = year, team = team))
#   }
#   
#   # join the advanced stats to the oc_impact df, binding new rows - done
#   
#   for(j in 1:nrow(team_advanced)){
#     row_to_add <- bind_cols(c(coach), team_advanced[j,], c(race), c("before"))
#     colnames(row_to_add)[1] <- toString("Coach")
#     colnames(row_to_add)[83] <- toString("Race")
#     colnames(row_to_add)[84] <- toString("BeforeAfter")
#     dc_impact <- dc_impact %>% bind_rows(row_to_add)
#   }
#   
# }
# save(dc_impact, file="dc_impact.Rda")
# load("dc_impact.Rda")


dc_impact_summary <-dc_impact %>% select(c("Coach", "team", "def_ppa", "def_success_rate", "def_stuff_rate", "def_passing_plays_success_rate",
                                           "Race", "BeforeAfter"))

dc_impact_summary <- dc_impact_summary %>% group_by(Coach, team, Race, BeforeAfter) %>% dplyr::summarise(
  mean_ppa = mean(def_ppa), 
  mean_sr = mean(def_success_rate),
  mean_stuff = mean(def_stuff_rate),
  mean_pass_sr = mean(def_passing_plays_success_rate),
)

# Calculating the net (offense-defense after-before) impact on PPA, SR, Stuff, Pass SR, FPI

dc_impact_results <- data.frame()
i <- 1
# flipping signs so that positive change is good for a DC
while (i < nrow(dc_impact_summary)){
  dc_results <- data.frame()
  row_to_add <- data.frame()
  dc_results <- dc_impact_summary[i,] %>% group_by(Coach, team, Race) %>% 
    dplyr::summarise(net_ppa = -dc_impact_summary[i,"mean_ppa"]+dc_impact_summary[i+1, "mean_ppa"],
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

# save(dc_impact_results, file="dc_impact_results.Rda")
# load("dc_impact_results.Rda")

# doing some preliminary analysis
dc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(net_ppa))
#Race  mean_net_ppa_race
# 1 Black           -0.0298
# 2 Other           -0.0135
# 3 White           -0.0243

dc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_sr_race = mean(net_sr))
dc_impact_results %>% group_by(Race) %>% dplyr::summarise(mean_net_passsr_race = mean(net_pass_sr))

# CHECK FOR NORMALITY AND OUTLIERS
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
# (one of these outliers is Brian Norwood so we'll copy the current df which includes this row for later use)
dc_impact_results1 <- dc_impact_results
dc_impact_results1 <- dc_impact_results1[-c(out_inds),]
ggqqplot(dc_impact_results1$net_ppa)
dc_impact_results1 %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(net_ppa))

# analyze via simple linear regression
lm1 <- lm(net_ppa ~ Race, dc_impact_results1)
summary(lm1)

##########################################################################################
# We want to compare the impact of white and black coordinators who got hired as HC

dc_to_head_impact <- data.frame()
dc_to_head_impact <- defensive_to_head %>% 
  inner_join(dc_impact_results, by = "Coach") %>% 
  select(Coach, Race.x, Head_Coach_School, team, net_ppa, net_sr, net_stuff, net_pass_sr)
colnames(dc_to_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                 "Net_SR", "Net_Stuff", "Net_Pass_SR")

dc_to_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
#Race  mean_net_ppa_race
#  1 Black           0.0358 
#2 Other          -0.0133 
#3 White          -0.00339
## - We have something!

dc_to_head_impact_plot <- dc_to_head_impact %>% ggplot(aes(x = Net_SR, y = Net_PPA, colour = Race)) +
  geom_point(inherit.aes = TRUE, stat = "identity", position = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Net Success Rate Change", y= "Net PPA Change",
       title = "DC's --> HC Impact by Race", 
       subtitle = "FBS, 2005-present\nR-Squared for Net_PPA~Race = 0.013",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: @cfb_data with #cfbfastR") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("top"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9))
dc_to_head_impact_plot
# ggsave("dc_to_head_impact_plot.png", height = 7, width = 10, dpi = 300)

sd(dc_to_head_impact$Net_PPA)
# SD is 0.086, so black DCs about 0.5 sd's better

# checking the significance level here:
dc_to_head_model <- lm(Net_PPA~Race, data = dc_to_head_impact)
summary(dc_to_head_model)
summary(aov(Net_PPA~Race, data = dc_to_head_impact))
# p value of .2 for this as a whole. So no real significance here

dc_to_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_sr_race = mean(Net_SR))
#Race  mean_net_sr_race
#  1 Black          0.0164 
#2 Other          0.00652
#3 White         -0.00138

# doing the same for offense

oc_to_head_impact <- data.frame()
oc_to_head_impact <- offensive_to_head %>% inner_join(oc_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, team, net_ppa, net_sr, net_stuff, net_pass_sr
)
colnames(oc_to_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                 "Net_SR", "Net_Stuff", "Net_Pass_SR")

oc_to_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_net_ppa_race
#  1 ?                0.0591
#2 Black            0.00820
#3 Other           -0.0743
#4 White            0.0280

# dc_to_head_impact and oc_to_head_impact had a total of 19 unique Black coaches included. 
# combine datasets and minority races to increase sample size
coord_to_head_impact <- dc_to_head_impact %>%
  rbind(oc_to_head_impact)
coord_to_head_impact$Race <- ifelse(coord_to_head_impact$Race == "?", "Non-white",
                                    ifelse(coord_to_head_impact$Race == "Other", "Non-white",
                                           ifelse(coord_to_head_impact$Race == "Black", "Non-white", "White")))
coord_to_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
# Race      mean_net_ppa_race
# 1 Non-white           0.00942
# 2 White               0.0162 

# No signal here.

oc_to_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_sr_race = mean(Net_SR))
oc_to_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_stuff_race = mean(Net_Stuff))

## need to run again with the numbers for their impact as head coaches

former_dc_head_impact <- data.frame()
former_dc_head_impact <- defensive_to_head %>% inner_join(head_coach_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, Team, Net_PPA, Net_SR, Net_Stuff_Rate, Net_Pass_SR, Net_FPI
)
colnames(former_dc_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                     "Net_SR", "Net_Stuff", "Net_Pass_SR", "Net_FPI")

former_dc_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_net_ppa_race
# <chr>             <dbl>
#   1 Black         -0.0437  
# 2 Other         -0.0344  
# 3 White         0.00308

former_dc_head_impact %>% group_by(Race) %>% filter(!is.na(Net_FPI)) %>% dplyr::summarise(mean_net_fpi_race = mean(Net_FPI))
# Race  mean_net_fpi_race
# <chr>             <dbl>
#   1 Black           -6.80  
# 2 Other           -1.96  
# 3 White            0.0311

# Then propose some black defensive coordinators as good head coach options?
# Brian Norwood

# running oc's with their head coach numbers
former_oc_head_impact <- data.frame()
former_oc_head_impact <- offensive_to_head %>% inner_join(head_coach_impact_results, by = "Coach") %>% select(
  Coach, Race.x, Head_Coach_School, Team, Net_PPA, Net_SR, Net_Stuff_Rate, Net_Pass_SR, Net_FPI
)
colnames(former_oc_head_impact) <- c("Coach", "Race", "Head_Coach_School", "Coordinator School", "Net_PPA",
                                     "Net_SR", "Net_Stuff", "Net_Pass_SR", "Net_FPI")

former_oc_head_impact %>% group_by(Race) %>% dplyr::summarise(mean_net_ppa_race = mean(Net_PPA))
# Race  mean_net_ppa_race
# <chr>             <dbl>
#   1 ?              -0.0928 
# 2 Black          -0.00573
# 3 Other           0.0377 
# 4 White           0.00955 

former_oc_head_impact$Race <- ifelse(former_oc_head_impact$Race == "?", "Non-white",
                                     ifelse(former_oc_head_impact$Race == "Other", "Non-white",
                                            ifelse(former_oc_head_impact$Race == "Black", "Non-white", "White")))
former_oc_head_impact %>% group_by(Race) %>% filter(!is.na(Net_FPI)) %>% dplyr::summarise(mean_net_fpi_race = mean(Net_FPI))
# Race  mean_net_fpi_race
# <chr>             <dbl>
#   1 ?                4     
# 2 Black           -0.998 
# 3 Other            4.44  
# 4 White           -0.0952

# Race      mean_net_fpi_race
# <chr>                 <dbl>
#   1 Non-white           -0.384 
# 2 White               -0.234
# Then propose some black offensive coordinators as good head coach options?
# Maurice Harris
# Alex Atkins 
# Josh Gattis
# Newland Isaac

##########################################################################################

# Are there certain HCs who seem to more readily give minorities promotions / coordinator opportunities?

coaching_tree <- head_coaches1 %>%
  select(Coach, Season, College, Race) %>%
  left_join((defensive_coordinators1 %>% 
               select(Coach, Season, College, Race) %>% 
               dplyr::rename(Coordinator = Coach)), 
            by = c("Season", "College"))
coaching_tree1 <- head_coaches1 %>%
  select(Coach, Season, College, Race) %>%
  left_join((offensive_coordinators1 %>%
               select(Coach, Season, College, Race) %>%
               dplyr::rename(Coordinator = Coach)),
            by = c("Season", "College"))
coaching_tree <- coaching_tree %>%
  rbind(coaching_tree1) %>%
  filter(!is.na(Coordinator))

# Remove this next line if you want to see white vs. black instead of white vs minority
coaching_tree$Race.y <- ifelse(coaching_tree$Race.y == "?", "Non-white",
                               ifelse(coaching_tree$Race.y == "Other", "Non-white",
                                      ifelse(coaching_tree$Race.y == "Black", "Non-white", "White")))
# The following counts each year separately, so for example Andrew Thacker would count as 3 years of a White DC.
hires_by_years <- coaching_tree %>% 
  group_by(Coach, Race.x) %>%
  dplyr::count(Race.y, name = "years") %>%
  dplyr::mutate(total_years = sum(years)) %>%
  ungroup() %>%
  dplyr::mutate(percent_of_years_POC = years/total_years) %>%
  filter(Race.y == "Non-white")
# Now count each coordinator's tenure as 1 (not weighted for how long they held the position)
hires_by_coord <- coaching_tree %>%
  select(Coach, Race.x, College, Coordinator, Race.y) %>%
  distinct() %>%
  group_by(Coach, Race.x) %>%
  dplyr::count(Race.y, name = "coordinators") %>%
  dplyr::mutate(total_coordinators = sum(coordinators)) %>%
  ungroup() %>%
  dplyr::mutate(percent_of_coords_POC = coordinators/total_coordinators) %>%
  filter(Race.y == "Non-white")
minority_hires <- hires_by_years %>%
  left_join(hires_by_coord) %>%
  dplyr::mutate(years_rank = rank(desc(percent_of_years_POC)),
                coords_rank = rank(desc(percent_of_coords_POC)),
                rank = rank(years_rank + coords_rank))

# 538-style table from Thomas Mock's tutorial using the gt package
colnames(minority_hires) <- c("Coach", "Coach_Race", "Alternate_Race", "Non-White_Coordinator_Seasons", 
                              "Available_Coordinator_Seasons", "Available_Seasons_with_Non-White_Coordinator_Percentage", 
                              "Non-White_Coordinators", "Possible_Coordinators", "Non-White_Coordinators_Percentage", 
                              "Non-White_Coordinator_Seasons_Rank", "Non-White_Coordinators_Rank", "Combined_Rank")

minority_hires_for_table <- minority_hires %>% select(Coach, `Available_Seasons_with_Non-White_Coordinator_Percentage`, `Non-White_Coordinators_Percentage`, Combined_Rank)
minority_hires_for_table <- minority_hires_for_table %>% dplyr::mutate_if(is.numeric, round, digits = 2)
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
        # color = "transparent" does not work in latest version of gt(). "#FFFFFF00" is the Hex version of transparent.
        sides = "bottom", color = "#FFFFFF00", weight = px(2)
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
      table.border.top.color = "#FFFFFF00",
      table.border.bottom.color = "#FFFFFF00",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "#FFFFFF00",
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
minority_hires_bottom20 <-minority_hires_for_table[183:202,]
colnames(minority_hires_top20) <- c('Coach', 'Non-White Coordinator Seasons', 'Non-White Coordinators', "Combined Rank")
colnames(minority_hires_bottom20) <- c('Coach', 'Non-White Coordinator Seasons', 'Non-White Coordinators', "Combined Rank")

minority_hiring_table_top20 <- minority_hires_top20 %>% gt() %>%  tab_spanner(
  label = "Who Hires Non-White Coordinators?\nTop 20",
  columns = c('Non-White Coordinator Seasons', 'Non-White Coordinators')) %>% 
  data_color(
    columns = c('Non-White Coordinator Seasons', 'Non-White Coordinators'),
    colors = scales::col_numeric(
      # use a 3-color scale with the 1st 2 colors from RColorBrewer Set2
      palette = c(brewer.pal(n=5,"Set2")[[2]], "white", brewer.pal(n=5,"Set2")[[1]]),
      domain = c(0:1)
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
# gtsave(minority_hiring_table_top20, "Minority_Hiring_Table_Top20.png")

minority_hiring_table_bottom20 <- minority_hires_bottom20 %>% gt() %>%  tab_spanner(
  label = "Who Hires Non-White Coordinators?\nBottom 20",
  columns = c('Non-White Coordinator Seasons', 'Non-White Coordinators')) %>% 
  data_color(
    columns = c('Non-White Coordinator Seasons', 'Non-White Coordinators'),
    colors = scales::col_numeric(
      palette = c(brewer.pal(n=5,"Set2")[[2]], "white", brewer.pal(n=5,"Set2")[[1]]),
      domain = c(0:1)
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
# gtsave(minority_hiring_table_bottom20, "Minority_Hiring_Table_bottom20.png")

# Do minority HCs hire more minority coordinators than white HCs?
minority_hires$Coach_Race <- ifelse(minority_hires$Coach_Race == "?", "Non-white",
                                    ifelse(minority_hires$Coach_Race == "Other", "Non-white",
                                           ifelse(minority_hires$Coach_Race == "Black", "Non-white", "White")))
minority_hires %>%
  group_by(Coach_Race) %>%
  dplyr::summarise(percent_of_years_POC = mean(`Available_Seasons_with_Non-White_Coordinator_Percentage`),
                   percent_of_coords_POC = mean(`Non-White_Coordinators_Percentage`))

# Coach_Race percent_of_years_POC percent_of_coords_POC
# <chr>                     <dbl>                 <dbl>
#   1 Non-white                 0.339                 0.341
# 2 White                     0.263                 0.280
# Coach_Race percent_of_years_POC percent_of_coords_POC
# <chr>                     <dbl>                 <dbl>
#   1 ?                         0.5                   0.417
# 2 Black                     0.334                 0.349
# 3 Other                     0.353                 0.258
# 4 White                     0.265                 0.281
# minority HCs are 6-8 percentage points more likely than white HCs to hire a minority coordinator (20-30% more likely).
# sum(minority_hires$total_coordinators) # <- sample size

# later we will combine this data with the centrality data from the Network Analysis...

##########################################################################################

# https://ona-book.org/community.html

# Use the Louvain algorithm to further analyze the social impact of coaching hires?

coaching_tree1 <- coaching_tree %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())

###
# # To create a  graph from an edgelist:
# edgelist <- coaching_tree %>%
#   select(Coach, Coordinator) %>%
#   dplyr::rename(c("from" = "Coach", "to" = "Coordinator"))
# edgelist_matrix <- as.matrix(edgelist)
# graph1 <- igraph::graph_from_edgelist(el = edgelist_matrix, directed = TRUE)
# graph1
# # IGRAPH 5da95ff DN-- 1431 5643 -- 
# # ^ denotes 'D'irected graph with 'N'amed vertices containing 1,431 vertices and 5,643 edges
# To create a weighted graph from a dataframe:
edges_df <- coaching_tree1 %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))
vertex_df <- head_coaches1 %>%
  rbind(defensive_coordinators1) %>%
  rbind(offensive_coordinators1) %>%
  select(Coach, Race) %>%
  distinct()
# this checks to see if there are any duplicates, because if there are, the following graph function will error out.
# n_occur <- data.frame(table(vertex_df$Coach))
# create the graph object
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
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
# V(graph)$label <- ifelse(V(graph)$name %in% c("Nick Saban", "Geoff Collins"),
#                          V(graph)$name,
#                          "")

# change label font color, size, and font family
V(graph)$label.color <- "black"
V(graph)$label.cex <- 0.8
# V(graph)$label.family <- "arial"

# VERTICES
V(graph)$size <- 3 
V(graph)$frame.color = "black"

# EDGES
#E(graph)$width <- graph$weight
E(graph)$arrow.size <- 0.5
# #create plot layout
# layout1 = layout_randomly(graph)
# # shape-oriented layouts
# layout2 = layout_as_tree(graph)
# layout3 = layout_in_circle(graph)
# layout4 = layout_on_sphere(graph)
# # force-directed layouts - use algorithms to attract connected vertices together and repel non-connected vertices
# layout5 = layout_with_fr(graph)
# layout6 = layout_with_kk(graph)
# # Sugiyama is suitable for directed graphs and minimizes edge crossings by introducing bends on edges
# layout7 = layout_with_sugiyama(graph) # says layout must be a matrix?
# # for large graphs
# layout8 = layout_with_lgl(graph)
# layout9 = layout_with_drl(graph)
# layout10 = layout_with_graphopt(graph)
# # plot with base R:
# plot(graph, layout = layout1)
# plot(graph, layout = layout2)
# plot(graph, layout = layout3)
# plot(graph, layout = layout4)
# plot(graph, layout = layout5)
# plot(graph, layout = layout6)
# plot(graph, layout = layout7)
# plot(graph, layout = layout8)
# plot(graph, layout = layout9)
# plot(graph, layout = layout10)

# # # ...or use ggraph similar to ggplot2
# ggraph(graph, layout = "kk") +
#   geom_edge_link(show.legend = FALSE) +
#   geom_node_point(color = "blue", size = 1) +
#   # geom_node_label(aes(label = name), color = "blue") +
#   # or geom_node_point(aes(color = community))
#   theme_void() +
#   labs(title = "Coaching Tree")
# # how do you get alpha or width of edges to change based on weights?
# # I tried  putting the following in geom_edge_link() but it didn't work --> aes(edge_width = weight), color = "grey", alpha = 0.7,
# 
# # or use library(networkD3) for interactive graphs (I don't know how to embed this in an article while keeping the interactive features though)
# networkD3::simpleNetwork(coaching_tree1)
# V(graph)$group <- ifelse(V(graph)$name %in% c("Nick Saban", "Geoff Collins"), 1, 2)
# netd3_list <- networkD3::igraph_to_networkD3(graph, group = V(graph)$group)
# networkD3::forceNetwork(
#   Links = netd3_list$links,
#   Nodes = netd3_list$nodes,
#   NodeID = "name",
#   Source = "source",
#   Target = "target",
#   Group = "group"
# )

# PATHS, DISTANCE, & CENTRALITY
edge_density(graph)
# 0.001081937 (it's a sparse graph)
get_diameter(graph)
get_diameter(graph, weights = NA)
diameter_list <- as.list(get_diameter(graph, weights = NA))
# Bret was a coordinator for Bill, Paul was a coordinator for Bret, Dave was a coordinator for Paul, etc. This is the longest "shortest path".
# # how to create a subgraph with only certain coaches included - this subgraph only includes coaches (nodes) from the diameter above:
# subgraph <- induced_subgraph(graph, vids = c(diameter_list))
# plot(subgraph, layout = layout.auto, label = get_diameter(graph))
# diameter_plot <- ggraph(subgraph, layout = "tree") +
#   geom_edge_link(show.legend = FALSE) +
#   geom_node_point(color = "blue", size = 1) +
#   geom_node_label(aes(label = name), color = "blue") +
#   # or geom_node_point(aes(color = community))
#   theme_void() +
#   labs(title = "The Longest Direct Line in a Coaching Tree Since 2000")
# diameter_plot
# ggsave('diameter_plot.png', height = 10, width = 4.55, dpi = 300)
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
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Todd Graham at 21, Nick Saban and Tommy Tubberville at 20. I wonder why it says Saban has the most neighbors at 20, but Todd Graham has the highest Degree Centrality at 21.
# Ego Size: the n-th order ego network of a given vertex v is a set including v and all vertices of distance at most n from v. (Saban has a 1st-order ego size of 20)
ego(graph, order=2)
ego_size(graph, order=2, nodes = V(graph))
# Saban has 111 2nd-degree connections (aka 2nd-order edges)
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
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

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))
view(centrality_mean)
# Comparing connection between white and black coaches
centrality_mean[4,5]/centrality_mean[2,5]
# So white coaches are 1.39 times more connected than Black coaches (degree) on average.
# Comparing eigen influence between white and black coaches
centrality_mean[4,2]/centrality_mean[2,2]
# So white coaches are 7 times more influential than Black coaches (degree) on average.
# Comparing betweenness between white and black coaches
centrality_mean[4,3]/centrality_mean[2,3]
# So white coaches are 2.9 times more super connectors than Black coaches (degree) on average.
centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector
# so now you can adjust the size of the node (for example) according to the degree centrality (aes(size = degree))

########

# 538-style table showing influence and connectivity

centrality_df_modified <- centrality_df %>% select(variable, closeness, degree, Race)
colnames(centrality_df_modified) <- c("Coach", "Influence", "Connections",  "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at("Influence", ~(scale(., center = FALSE) %>% as.vector))
centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

minority_hires_influence <- minority_hires %>% left_join(centrality_df_scaled, by = "Coach")
minority_hires_influence <- minority_hires_influence %>% select(Coach, Coach_Race, `Available_Seasons_with_Non-White_Coordinator_Percentage`, `Non-White_Coordinators_Percentage`, Combined_Rank, Connections, Influence)
minority_hires_influence <- minority_hires_influence %>% dplyr::mutate_if(is.numeric, round, digits = 2)
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
  label = "Most Connected Coaches' Coordinator Hiring",
  columns = c('Non-White Coordinator Seasons', 'Non-White Coordinators', "Non-White Coordinator Hiring Rank")) %>% 
  data_color(
    columns = c("Non-White Coordinator Hiring Rank"),
    colors = scales::col_numeric(
      palette = c(brewer.pal(n=5,"Set2")[[2]], "white", brewer.pal(n=5,"Set2")[[1]]),
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
# gtsave(minority_hiring_table_connected20, "minority_hiring_table_connected20.png")

minority_hiring_table_influential20 <- minority_hires_top20_influential %>% gt() %>%  tab_spanner(
  label = "Most Influential Coaches' Coordinator Hiring",
  columns = c('Non-White Coordinator Seasons', 'Non-White Coordinators', "Non-White Coordinator Hiring Rank")) %>% 
  data_color(
    columns = c("Non-White Coordinator Hiring Rank"),
    colors = scales::col_numeric(
      palette = c(brewer.pal(n=5,"Set2")[[2]], "white", brewer.pal(n=5,"Set2")[[1]]),
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
# gtsave(minority_hiring_table_influential20, "minority_hiring_table_influential20.png")

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
graph1 <- igraph::graph_from_data_frame(d = edges_df, directed = FALSE, vertices = vertex_df)
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
# visualize louvain communities in 1 plot
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

# plot individual communities grouped by Race. (probably don't want to label these w/ all coach's names shown)
# Bill Snyder's
community <- induced_subgraph(graph1, vids = communities[[23]])
labs23 <- c("Bill Snyder", "Turner Gill")
v_labels <- which(V(community)$name %in% labs23)
for(i in 1:length(V(community))){
  if(!(i %in% v_labels)) {V(community)$name[i] <- ""}
}
V(community)$name
# only label certain coaches:
race_plot23 <- ggraph(community, layout = "kk") +
  geom_edge_link(color = "grey") +
  geom_node_point(size = 4, aes(color = Race)) +
  geom_node_text(aes(label = name), repel=T, force=100) +
  scale_colour_brewer(palette = "Set2") +
  theme_light() +
  labs(title = "Bill Snyder's Community",
       caption = "Plot: @markwood14 and @robert_binion") +
  theme(# panel.grid.minor.x = element_line(linetype = 1, color = "red"),
    # legend.position = c(0.89, 0.85),
    # legend.box.background = element_rect(color="black"),
    panel.background = element_rect(fill = "#F5F5F5"),
    # plot.subtitle = element_text(size=9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
race_plot23
# ggsave('race_plot23.png', height = 5.625, width = 10, dpi = 300)
communities[[23]]

# Nick Saban's
community <- induced_subgraph(graph1, vids = communities[[9]])
labs9 <- c("Nick Saban")
v_labels <- which(V(community)$name %in% labs9)
for(i in 1:length(V(community))){
  if(!(i %in% v_labels)) {V(community)$name[i] <- ""}
}
V(community)$name

race_plot9 <- ggraph(community, layout = "kk") +
  geom_edge_link(color = "grey") +
  geom_node_point(size = 4, aes(color = Race)) +
  geom_node_text(aes(label = name), repel=T, force=100) +
  scale_colour_brewer(palette = "Set2") +
  theme_light() +
  labs(title = "Nick Saban's Community",
       caption = "Plot: @markwood14 and @robert_binion") +
  theme(# panel.grid.minor.x = element_line(linetype = 1, color = "red"),
    # legend.position = c(0.89, 0.85),
    # legend.box.background = element_rect(color="black"),
    panel.background = element_rect(fill = "#F5F5F5"),
    # plot.subtitle = element_text(size=9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
race_plot9
# ggsave('race_plot9.png', height = 5.625, width = 10, dpi = 300)
communities[[9]]

# Least diverse communities (by %): 2, 11, 24, 28 
# most diverse communities: 3, 18, 35, 41
##########################################################################################

# Now analyze the demographics of the candidate pool compared to current demographics of coaches

# Racial demographics of CFB Football Players (2011 - 2020) (self-reported  data from NCAA member schools' and published at NCAA.org: https://www.ncaa.org/about/resources/research/ncaa-demographics-database)

athletes <- read.csv("https://raw.githubusercontent.com/rebinion/College-Coaching-Project/main/racial_demographics_of_cfb_athletes.csv?token=GHSAT0AAAAAABX2FEXJBZ6OGWJJHJWJGUPIYX7SMMA", check.names=FALSE) %>% filter(Race.Ethnicity != "TOTAL")
athletes$Race.Ethnicity <- ifelse(athletes$Race.Ethnicity == "White", "White",
                                  ifelse(athletes$Race.Ethnicity == "Black", "Black", "Other"))
athletes <- athletes %>%
  group_by(Division, Race.Ethnicity) %>%
  dplyr::summarise(Percent = sum(Percent))
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
# ggsave('athletes_plot.png', height = 5.625, width = 10, dpi = 300)

## Racial demographics of CFB COACHES (2011 - 2020) (self-reported  data from NCAA member schools' and published at NCAA.org: https://www.ncaa.org/about/resources/research/ncaa-demographics-database)

# This code was used to pull demographics of coaches from NCAA.org, which includes Assistants. We chose to use our data set from Hutch instead, which has fewer race categories and will align better w/ the rest of our findings.
# coaches <- read.csv("https://raw.githubusercontent.com/rebinion/College-Coaching-Project/main/racial_demographics_of_cfb_coaches.csv?token=GHSAT0AAAAAABX2FEXJPX3AFJZKDTODDDJ4YX7SNFA", check.names=FALSE) %>% filter(Race != "TOTAL")
# coaches$Race <- ifelse(coaches$Race == "White", "White",
#                        ifelse(coaches$Race == "Black", "Black", "Other"))
# coaches <- coaches %>%
#   group_by(Position, Race) %>%
#   dplyr::summarise(Percent = sum(Percent))

hc <- head_coaches1 %>%
  filter(Season > 2010) %>%
  dplyr::mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>%
  dplyr::summarise(num_race = n()) %>%
  dplyr::mutate(Percent = num_race / sum(num_race),
                Position = "Head")
oc <- offensive_coordinators1 %>%
  filter(Season > 2010) %>%
  dplyr::mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>%
  dplyr::summarise(num_race = n()) %>%
  dplyr::mutate(Percent = num_race / sum(num_race),
                Position = "OC")
dc <- defensive_coordinators1 %>%
  filter(Season > 2010) %>%
  dplyr::mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>%
  dplyr::summarise(num_race = n()) %>%
  dplyr::mutate(Percent = num_race / sum(num_race),
                Position = "DC")
Race <- c("White", "Black", "Other")
Percent <- c(0.514, 0.370, 0.116)
expected <- data.frame(Position = "Expectation",
                       Race,
                       Percent)
coaches <- hc %>%
  rbind(oc) %>%
  rbind(dc) %>%
  select(Position, Race, Percent) %>%
  rbind(expected)

coaches$Race <- relevel(as.factor(coaches$Race), 'White')
coaches$Position <- factor(as.factor(coaches$Position), levels = c('Expectation', 'Head', 'OC', 'DC'))
# Plot demographics of HCs, OCs, and DCs, compared to the expected demographics based on the hiring candidate pool.
coaches_plot <- coaches %>%
  ggplot(aes(fill = Race, y = Percent, x = forcats::fct_rev(Position), label = round(Percent,2))) +
  geom_col(position = position_fill(reverse = TRUE), alpha = 0.8, width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(size = 3, position = position_stack(vjust = 0.5, reverse = TRUE)) +
  annotate("segment",x=3.5, y=0, xend=3.5, yend=1,
           lwd=1.3, col="black") +
  coord_flip() +
  theme_light() +
  labs(title = "Racial Demographics of D-I CFB Coaches", 
       subtitle = "From 2011 to 2020",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: NCAA.org & Team Info Pages") +
  xlab("Role") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("bottom"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9),
        axis.title.x = element_blank())
coaches_plot
# ggsave('coaches_plot.png', height = 5.625, width = 10, dpi = 300)

# what should the coaching demographics actually look like? (Not percentages but total quantities)
ideal_demog <- data.frame(Race = c("White", "Black", "Other"),
                          united_states = c(0.601, 0.122, 0.277),
                          cfb = c(0.5065, 0.3900, 0.1035))
ideal_demog <- ideal_demog %>%
  dplyr::mutate(ideal_percent = 0.927*cfb + 0.073*united_states) %>%
  select(Race, ideal_percent)
hc_2021 = head_coaches1 %>%
  filter(Season == 2021) %>% 
  dplyr::mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>% 
  dplyr::summarise(num_race = n()) %>%
  dplyr::mutate(actual_2021 = num_race / sum(num_race)) %>%
  left_join(ideal_demog) %>%
  dplyr::mutate(num_ideal = round(sum(num_race)*ideal_percent, 0),
                difference = num_ideal - num_race)
oc_2021 = offensive_coordinators1 %>%
  filter(Season == 2021) %>% 
  dplyr::mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>% 
  dplyr::summarise(num_race = n()) %>%
  dplyr::mutate(actual_2021 = num_race / sum(num_race)) %>%
  left_join(ideal_demog) %>%
  dplyr::mutate(num_ideal = round(sum(num_race)*ideal_percent, 0),
                difference = num_ideal - num_race)
dc_2021 = defensive_coordinators1 %>%
  filter(Season == 2021) %>% 
  dplyr::mutate(Race = ifelse(Race=="?","Other",Race)) %>%
  group_by(Race) %>% 
  dplyr::summarise(num_race = n()) %>%
  dplyr::mutate(actual_2021 = num_race / sum(num_race)) %>%
  left_join(ideal_demog) %>%
  dplyr::mutate(num_ideal = round(sum(num_race)*ideal_percent, 0),
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
  reshape2::melt(id="role")
ideal_change$role <- factor(as.factor(ideal_change$role), levels = c('Head Coach', 'Offensive Coordinator', 'Defensive Coordinator'))
# force the bar labels to have + or - in front of the values
ideal_change$value_text <- c("-45","-48","-39","+35","+35","+24","+10","+13","+14")
# Plot how many fewer white coaches and more Black and Other coaches are needed to align w/ candidate pool.
ideal_plot <- ggplot(data=ideal_change, aes(x = role, y = value, fill = variable, label = value_text)) +
  geom_col(width = 0.6, alpha=0.8) +
  geom_hline(yintercept=0) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_light() +
  labs(title = "Change in Total Number of Coaches by Race Needed to Reflect the Demographics of the Candidate Pool", 
       subtitle = "Based on 2021 coaching staffs",
       fill = "Race: ",
       caption = "Plot: @markwood14 & @robert_binion\nData: U.S. Census Bureau & NCAA.org") +
  ylab("Change in Number of Coaches") +
  theme(panel.grid.minor = element_blank(),
        legend.position = ("bottom"),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.subtitle = element_text(size=9),
        axis.title.x = element_blank())
ideal_plot
# ggsave('ideal_plot.png', height = 5.625, width = 10, dpi = 300)

##########################################################################################  

# Time Series Data

head_coach_time_series <- head_coaches1 %>% 
  group_by(Season) %>% 
  dplyr::mutate(num_coaches = n()) %>%
  ungroup() %>% 
  group_by(Season, Race) %>% 
  filter(Race == "Black") %>% 
  dplyr::mutate(percent_black = n()/num_coaches) %>% 
  distinct(Season, .keep_all = TRUE) %>% 
  select(Season, percent_black) %>% 
  dplyr::mutate_if(is.numeric, round, digits = 2)
head_coach_race_time_plot <- head_coach_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Head Coaches in the FBS",
       title = "Percentage of Black Head Coaches since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
head_coach_race_time_plot
# ggsave('head_coach_race_time_plot.png', height = 7, width = 10, dpi = 300)

oc_time_series <- offensive_coordinators1 %>% 
  group_by(Season) %>% 
  dplyr::mutate(num_coaches = n()) %>%
  ungroup() %>% 
  group_by(Season, Race) %>% 
  filter(Race == "Black") %>% 
  dplyr::mutate(percent_black = n()/num_coaches) %>% 
  distinct(Season, .keep_all = TRUE) %>% 
  select(Season, percent_black) %>% 
  dplyr::mutate_if(is.numeric, round, digits = 2)
oc_race_time_plot <- oc_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Offensive Coordinators in the FBS",
       title = "Percentage of Black Offensive Coordinators since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
oc_race_time_plot
# ggsave('oc_race_time_plot.png', height = 7, width = 10, dpi = 300)

dc_time_series <- defensive_coordinators1 %>% 
  group_by(Season) %>% 
  dplyr::mutate(num_coaches = n()) %>%
  ungroup() %>% 
  group_by(Season, Race) %>% 
  filter(Race == "Black") %>% 
  dplyr::mutate(percent_black = n()/num_coaches) %>% 
  distinct(Season, .keep_all = TRUE) %>% 
  select(Season, percent_black) %>% 
  dplyr::mutate_if(is.numeric, round, digits = 2)
dc_race_time_plot <- dc_time_series %>% ggplot(aes(x=Season, y=percent_black)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent_black), vjust=1.6, color="white", size=3)+
  labs(x = "Season", y= "Percentage of Black Defensive Coordinators in the FBS",
       title = "Percentage of Black Defensive Coordinators since 2000",
       caption = "Figure: @robert_binion and @markwood 14| Data: Team Info Pages") +
  theme_minimal()
dc_race_time_plot
# ggsave('hdc_race_time_plot.png', height = 7, width = 10, dpi = 300)

# Exponential Smoothing
# dataset includes player demographics from NCAA Student-Athlete Ethnicity Reports from all Divisions (I, II, III)
player_ts1 <- read.csv("https://raw.githubusercontent.com/rebinion/College-Coaching-Project/main/player_race_time_series.csv?token=GHSAT0AAAAAABX2FEXI7M5KPNS4KEG2T4NIYX7SNPA", check.names=FALSE)
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
  reshape2::melt(variable.name = "Season",
                 value.name = "percent_black") %>%
  dplyr::mutate(Role = "Player")
player_ts1$Season <- as.numeric(as.character(player_ts1$Season))

coach_time_series <- head_coaches1 %>% 
  dplyr::mutate(Role = "Head Coach") %>%
  rbind(offensive_coordinators1 %>%
          dplyr::mutate(Role = "Offensive Coordinator")) %>%
  rbind(defensive_coordinators1 %>%
          dplyr::mutate(Role = "Defensive Coordinator")) %>%
  group_by(Season, Role) %>% 
  dplyr::mutate(num_coaches = n()) %>%
  ungroup() %>% 
  filter(Race == "Black") %>% 
  group_by(Season, Role) %>% 
  dplyr::mutate(percent_black = n()/num_coaches) %>% 
  select(Season, Role, percent_black) %>%
  distinct(Season, Role, percent_black) %>%
  rbind(player_ts1)

# Plot of percentage of black players, HCs, OCs, and DCs over time
coach_race_time_plot1 <- coach_time_series %>% 
  ggplot(aes(x=Season, y=percent_black, color = Role)) +
  geom_line(stat="identity", size=1)+
  geom_point(size=2)+
  scale_colour_brewer(palette = "Set2") +
  labs(x = "Season", y= "Percentage",
       title = "% of Black Coaches in FBS vs. Black Players in CFB",
       caption = "Plot: @markwood14 and @robert_binion\nData: Team Info Pages & NCAA.org") +
  theme_light() +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"))
coach_race_time_plot1
# ggsave('coach_race_time_plot1.png', height = 5.625, width = 10, dpi = 300)

coach_time_series1 <- head_coaches1 %>%
  rbind(offensive_coordinators1) %>%
  rbind(defensive_coordinators1) %>%
  dplyr::mutate(Role = "Coach") %>%
  group_by(Season) %>% 
  dplyr::mutate(num_coaches = n()) %>%
  ungroup() %>% 
  filter(Race == "Black") %>% 
  group_by(Season) %>% 
  dplyr::mutate(percent_black = n()/num_coaches) %>% 
  select(Season, Role, percent_black) %>%
  distinct(Season, Role, percent_black) %>%
  rbind(player_ts1)

df1 <- coach_time_series1 %>%
  filter(Season == 2000 | Season == 2020)
df<- data.frame(x1 = 2000, x2 = 2000, x3 = 2020, x4 = 2020,
                y1 = df1[df1$Role == "Player" & df1$Season == 2000, "percent_black"][[1]],
                y2 = df1[df1$Role == "Coach" & df1$Season == 2000, "percent_black"][[1]],
                y3 = df1[df1$Role == "Player" & df1$Season == 2020, "percent_black"][[1]],
                y4 = df1[df1$Role == "Coach" & df1$Season == 2020, "percent_black"][[1]])
hc_2021[hc_2021$Race == "White", "difference"][[1]]
coach_time_series <- coach_time_series %>% dplyr::mutate_if(is.numeric, round, digits = 2)

# Plot of percentage of black players vs black coaches over time (HC, OC, and DC combined)
coach_race_time_plot <- coach_time_series1 %>% 
  ggplot(aes(x=Season, y=percent_black, color = Role)) +
  geom_line(stat="identity", size=1)+
  geom_point(size=2)+
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
# ggsave('coach_race_time_plot1.png', height = 5.625, width = 10, dpi = 300)

##########################################################################################

# Recommend minority coordinators for promotions who are performing well in their current positions

# STAT PROFILES for 
# Brian Norwood
# Maurice Harris
# Alex Atkins 
# Josh Gattis
# Newland Isaac

best_candidates_df <- data.frame()
# for now we're manually adjusting net_ppa and net_sr for Brian Norwood and Josh Gattis to weight their multiple tenures according to # of seasons. (for instance, Norwood's exceptional 1 year at Navy should have less weight than his below-average 3-years at Tulsa) (Need to do a sumproduct of net_ppa*years_in_program/total_years_coaching)
coach_1 <- dc_impact_results1 %>%
  filter(str_detect(Coach, "Brian Norwood")) %>% 
  group_by(Coach) %>% 
  dplyr::summarise(net_ppa = 0.010282,
                   net_sr = -0.00156, 
                   net_stuff = mean(net_stuff), 
                   net_pass_sr = mean(net_pass_sr))
coach_2 <- oc_impact_results%>%
  filter(str_detect(Coach, "Alex Atkins"))
coach_3 <- oc_impact_results %>% 
  filter(str_detect(Coach, "Josh Gattis")) %>% 
  group_by(Coach) %>% 
  dplyr::summarise(net_ppa = 0.080055,
                   net_sr = 0.020677, 
                   net_stuff = mean(net_stuff), 
                   net_pass_sr = mean(net_pass_sr))
coach_4 <- oc_impact_results %>%
  filter(str_detect(Coach, "Maurice Harris"))
coach_5 <- oc_impact_results %>%
  filter(str_detect(Coach, "Newland Isaac"))
best_candidates_df <- bind_rows(coach_1, coach_2, coach_3, coach_4, coach_5)

# add a column with headshots
best_candidates_df$headshot <- c("https://d3kmx57qvxfvw9.cloudfront.net/images/2021/8/23/Norwood_Brian_0125Cropped.jpg?width=300",
                                 "https://seminoles.com/wp-content/uploads/2020/01/Atkins-Alex-scaled.jpg",
                                 "https://d1a8hwz3c6qyrc.cloudfront.net/images/2018/3/24/Gattis_Josh.JPG?width=300",
                                 "https://www.liberty.edu/flames/wp-content/uploads/staff/Harris,%20Maurice%20(2019).jpg",
                                 "https://asugoldenrams.com/images/2017/5/5/Isaac_Newland.jpg?width=1884&quality=80&format=jpg")

best_candidates_plot <- best_candidates_df %>% ggplot(aes(x=net_ppa, y=net_sr, label=Coach)) + 
  #geom_smooth(method=lm, se=FALSE, col='red', size=0.5) +
  geom_image(image = best_candidates_df$headshot, asp = 16/9) +
  geom_label(aes(label = Coach), vjust = 1.2) +
  geom_vline(xintercept = mean(dc_impact_results$net_ppa), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = mean(dc_impact_results$net_sr), linetype = "dashed", color = "red", alpha = 0.5) +
  labs(x = "Net Impact on PPA", y= "Net Impact on Success Rate",
       title = "5 Top Black Head Coaching Candidates",
       subtitle = "Impact While Coordinator Compared to Average (Dotted Red Lines)",
       caption = "Figure: @robert_binion @markwood14 | Data: @CFB_Data with @cfbfastR") +
  coord_cartesian(xlim = c(-0.03, 0.225),
                  ylim = c(-0.01, 0.085)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        legend.position = "right")
best_candidates_plot
# ggsave('best_candidates_plot.png', height = 7, width = 10, dpi = 300)

##########################################################################################
# miscellaneous analysis

centrality_df$Race <- ifelse(centrality_df$Race == "?", "Non-white",
                             ifelse(centrality_df$Race == "Other", "Non-white",
                                    ifelse(centrality_df$Race == "Black", "Non-white", "White")))
lm4 <- lm(closeness~Race, centrality_df)
summary(lm4)
lm5 <- lm(degree~Race, centrality_df)
summary(lm5)


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################





# USE VARIOUS CLASSIFICATION METHODS TO TRY TO PREDICT WHICH COORDINATORS WILL BECOME HEAD COACHES
# potential race-based implications: if a predictive model regularly predicts more black coordinators to become head coaches than those who actually do

# Compile dataset with all independent and dependent variables from years 2015-2021.

#Start with target year t=2021
target_year = 2021

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year
coordinators_training_year_2021_with_metrics <- coordinators_training_year_with_metrics


# repeat for future years...

#continue with target year t=2020
target_year = 2020

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year

coordinators_training_year_2020_with_metrics <- coordinators_training_year_with_metrics

target_year = 2019

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year
coordinators_training_year_2019_with_metrics <- coordinators_training_year_with_metrics

target_year = 2018

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year

coordinators_training_year_2018_with_metrics <- coordinators_training_year_with_metrics

target_year = 2017

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year

coordinators_training_year_2017_with_metrics <- coordinators_training_year_with_metrics

target_year = 2016

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))


# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year

coordinators_training_year_2016_with_metrics <- coordinators_training_year_with_metrics

target_year = 2015

# Filter data set for coaches who were coordinators in t-1. 

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))

centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods", 
                                                                                                                                               Coach))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches1 %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% dplyr::mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year

coordinators_training_year_2015_with_metrics <- coordinators_training_year_with_metrics

# stop after target_year=2015

# bind each year together

coordinators_training_year_with_metrics_all <- rbind(coordinators_training_year_2021_with_metrics,
                                                     coordinators_training_year_2020_with_metrics,
                                                     coordinators_training_year_2019_with_metrics,
                                                     coordinators_training_year_2018_with_metrics,
                                                     coordinators_training_year_2017_with_metrics,
                                                     coordinators_training_year_2016_with_metrics,
                                                     coordinators_training_year_2015_with_metrics)

# check to see if NAs exist:
which(is.na(coordinators_training_year_with_metrics_all), arr.ind=TRUE)
# NA issue is resolved. 

# now want to change values in Role column to either be 0 or 1 (offense is 0, defense is 1)
coordinators_training_year_with_metrics_all$role_DC1 <- ifelse(grepl("Defensive", coordinators_training_year_with_metrics_all$Role), 1, NA)
coordinators_training_year_with_metrics_all$role_DC1 <- ifelse(grepl("Offensive", coordinators_training_year_with_metrics_all$Role), 0, coordinators_training_year_with_metrics_all$role_DC1)

# how many total coordinators became head coaches?
sum(coordinators_training_year_with_metrics_all$BecameHC)
# 61.

tenure_df <- defensive_coordinators1 %>%
  rbind(offensive_coordinators1) %>%
  group_by(Coach) %>% 
  dplyr::summarise(tenure_length = n())
coordinators_training_year_with_metrics_all <- coordinators_training_year_with_metrics_all %>%
  left_join(tenure_df)
coordinators_training_year_with_metrics_all <- coordinators_training_year_with_metrics_all %>%
  dplyr::mutate(Race = ifelse(Race=="Black", 1, 0))

# Correlation Plot
png(height=600, width=600, file="coach_corr_plot.png", type = "cairo")
corrplot(cor(coordinators_training_year_with_metrics_all[,c("Networking", "Influence", "Connections", "InfluencePlus", "net_ppa",
                                                            "net_sr","net_stuff","net_pass_sr","role_DC1","Race","tenure_length")]),
         method="circle")
dev.off()

# Now we will use that DF for training/Test
# split into training/test

#make this example reproducible
set.seed(5)

#create ID column
coordinators_training_year_with_metrics_all$id <- 1:nrow(coordinators_training_year_with_metrics_all)

#use 70% of dataset as training set and 30% as test set 
train <- coordinators_training_year_with_metrics_all %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(coordinators_training_year_with_metrics_all, train, by = 'id')

# run classification algorithms on training

# start with variable selection
# experiement with elastic net
pkgs <- list("glmnet", "doParallel", "foreach", "pROC")
lapply(pkgs, require, character.only = T)
# ELASTIC NET WITH 0 < ALPHA < 1
set.seed(5)
a <- seq(0.1, 0.9, 0.05)
# mdlX <- coordinators_training_year_with_metrics_all %>% select(Networking, Influence, Connections, InfluencePlus, net_ppa, net_sr, net_stuff, net_pass_sr, role_DC1)
# mdlY <- coordinators_training_year_with_metrics_all %>% select(BecameHC)

# need Elastic Net inputs to be in matrix form
mdlX <- cbind(coordinators_training_year_with_metrics_all$Networking, 
              coordinators_training_year_with_metrics_all$Influence, 
              coordinators_training_year_with_metrics_all$Connections, 
              coordinators_training_year_with_metrics_all$InfluencePlus, 
              coordinators_training_year_with_metrics_all$net_ppa, 
              coordinators_training_year_with_metrics_all$net_sr, 
              coordinators_training_year_with_metrics_all$net_stuff, 
              coordinators_training_year_with_metrics_all$net_pass_sr, 
              coordinators_training_year_with_metrics_all$role_DC1,
              coordinators_training_year_with_metrics_all$Race,
              coordinators_training_year_with_metrics_all$tenure_length)
mdlY <- coordinators_training_year_with_metrics_all$BecameHC

# mat <- model.matrix(BecameHC ~ Networking + Influence + Connections + InfluencePlus + net_ppa + net_sr + net_stuff + net_pass_sr + role_DC1, data = coordinators_training_year_with_metrics_all)

# Simple Elastic Net w/ alpha equal to 0.5:
enet.cv <- cv.glmnet(mdlX, mdlY, alpha = 0.5, nfolds = 10)
enet.cv$lambda.min
enet.model <- glmnet(mdlX, mdlY, alpha = 0.5, nlambda=100)
coef(enet.model, s = enet.cv$lambda.min)
# s1
# (Intercept)  0.040502719
# V1           .          
# V2           .          
# V3           0.006788291
# V4           .          
# V5           0.057978094
# V6           .          
# V7           .          
# V8           0.215003488
# V9          -0.008334123
# V10         -0.011392621
# V11         -0.002905420
# Elastic Net selected the following 4 variables: Connections, net_ppa, net_pass_sr, and role_DC1, Race, and tenure_length

search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
# the minimum cvm was obtained from alpha=0.1

# cv3 <- search[search$cvm == min(search$cvm), ]
enet.cv <- cv.glmnet(mdlX, mdlY, alpha = 0.1, nfolds = 10)
enet.cv$lambda.min
enet.model <- glmnet(mdlX, mdlY, alpha = 0.1, nlambda=100)
coef(enet.model, s = enet.cv$lambda.min)
# still chooses the same 6 variables with optimal alpha=0.1.
# Elastic Net plot:
png(file="coach_elastic_net.png")
plot(enet.model)
legend("bottomleft",legend=c("Networking", "Influence", "Connections", "InfluencePlus", "net_ppa",
                             "net_sr","net_stuff","net_pass_sr","role_DC1","Race","tenure_length"), 
       fill=c(2,3,4,5,6,7,8,9,10,11,12), cex=0.75)
dev.off()

# start with basic Logistic Regression
# running with variables selected from Elastic Net

coach_predictions_log_model_reduced = glm(BecameHC ~ Connections+net_ppa+net_pass_sr+role_DC1+Race+tenure_length, data=train, family=binomial)
summary(coach_predictions_log_model_reduced)
# with the further reduced model, net_ppa and Connections are both stronger predictors of who will get hired

# predict on test data and compare to actual 
coach_predictions_log_model_predict = predict(coach_predictions_log_model_reduced, test, type="response")

# starting with threshold of 0.5 but will probably adjust this
predicted_hire <- ifelse(coach_predictions_log_model_predict > 0.5, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# now let's look at this
test$PredictedHC <-predicted_hire
# We get 97% accuracy because this doesn't predict anyone to get hired! 
# need to try again at a different threshold

print(coach_predictions_log_model_predict)
max(coach_predictions_log_model_predict)
sort(coach_predictions_log_model_predict)
# it looks look about 0.29 is the highest prediction anyone gets
# so let's try with a threshold of 0.09

predicted_hire <- ifelse(coach_predictions_log_model_predict > 0.09, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
#92.7% accuracy with 0.09 threshold
# now let's look at this
test$PredictedHC <-predicted_hire
# at this threshold, we predict 21 to become HCs, and we get 2 right/ 4 eventually become HCs

# try again at 0.06
predicted_hire <- ifelse(coach_predictions_log_model_predict > 0.06, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# 92% accuracy
test$PredictedHC <-predicted_hire
# now we predict 31 to become HCs and still only 2 are right

# let's try to implement KNN

library(kknn)

kmodel <- train.kknn(BecameHC~Connections+net_ppa+net_pass_sr+role_DC1+Race+tenure_length, train, kmax=10, kernel = "rectangular", scale = TRUE)
kmodel_predictions <- predict(kmodel, test)

print(kmodel_predictions)
max(kmodel_predictions)
sort(kmodel_predictions)
# max is 0.375, 19 results >=0.25
# let's try a threshold of 0.2 after looking at the predictions
predicted_hire <- ifelse(kmodel_predictions > 0.2, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# this gives us 93.8% accuracy
# now let's look at this
test$PredictedHC <-predicted_hire
sum(test$PredictedHC)
# predicts 19 to become HCs, only 1 of those is correct. 5 eventually become HCs

# AdaBoost
# install.packages("JOUSBoost")
library(JOUSBoost)
set.seed(5)
trainX <- cbind(train$Connections, 
                train$net_ppa, 
                train$net_pass_sr, 
                train$role_DC1,
                train$Race,
                train$tenure_length)
trainY <- train %>% 
  dplyr::mutate(BecameHC = ifelse(BecameHC == 0, -1, 1)) %>%
  select(BecameHC)
trainY <- trainY$BecameHC
testX <- cbind(test$Connections, 
               test$net_ppa, 
               test$net_pass_sr, 
               test$role_DC1,
               test$Race,
               test$tenure_length)
testY <- test %>% 
  dplyr::mutate(BecameHC = ifelse(BecameHC == 0, -1, 1)) %>%
  select(BecameHC)
testY <- testY$BecameHC
# obtained tree_depth and n_rounds just from some guessing & checking
boost <- adaboost(trainX, trainY, tree_depth = 3, n_rounds = 5000)
pred <- predict(boost, testX)
# misclassification rate:
mean(testY != pred)
test$PredictedHC <-pred
boost$confusion_matrix = table(testY, pred)
boost
# predicted 4 to become HC, none are correct. 1 eventually become HC
# How about training accuracy?
pred <- predict(boost, trainX)
boost$confusion_matrix = table(trainY, pred)
boost
mean(trainY != pred)
# training accuracy is very good but not testing accuracy.

# XGBoost
# install.packages("xgboost")
library(xgboost)
trainY <- train$BecameHC
testY <- test$BecameHC
dtrain <- xgb.DMatrix(data = trainX, label = trainY)
dtest <- xgb.DMatrix (data = testX, label = testY)
watchlist <- list(train=dtrain, test=dtest)
xboost <- xgb.train(data = dtrain, max.depth = 5, eta = 1, nthread = 2, nrounds = 1500, eval.metric = "error", eval.metric = "logloss", watchlist=watchlist, objective = "binary:hinge") #objective = "binary:logistic" #objective = "multi:softmax", num_class = 2 <- neither of these did any better...
pred <- predict(xboost, testX)
print(pred)
max(pred)
sort(pred)
# gives 6 values of 1, the rest 0
predicted_hire <- ifelse(pred > 0.1, 1,0)
test$PredictedHC <-predicted_hire
mean(testY != predicted_hire)
xboost$confusion_matrix = table(testY, predicted_hire)
xboost$confusion_matrix
# predicts 6 hires, 1 is correct

# SVM
library(kernlab)

# with RBF kernel
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=50,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 3 to be hired
testaccuracy=mean(svm_pred == testY)
#96.5% accuracy because almost all are predicted not to be hired
test$PredictedHC <-svm_pred
# of the three predicted, none were correct

# try again with a different C value
# with RBF kernel, C= 1000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=1000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts only 3 to be hired
testaccuracy=mean(svm_pred == testY)
#95.5% accuracy
test$PredictedHC <-svm_pred

# try again with a different C value
# with RBF kernel, C= 10000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=10000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 8 to be hired
testaccuracy=mean(svm_pred == testY)
#95% accuracy
test$PredictedHC <-svm_pred

# keep going higher with C value
# try again with a different C value
# with RBF kernel, C= 100000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=100000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 9 to be hired
testaccuracy=mean(svm_pred == testY)
#94.8% accuracy
test$PredictedHC <-svm_pred


# keep going higher with C value
# try again with a different C value
# with RBF kernel, C= 1000000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=1000000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 12 to be hired
testaccuracy=mean(svm_pred == testY)
#94.5% accuracy
test$PredictedHC <-svm_pred
# 12 ends up bing the most using this kernel
# 2 end up becoming head coaches, none in target year

# keep going higher with C value
# try again with a different C value
# with RBF kernel, C= 10000000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=10000000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 10 to be hired
testaccuracy=mean(svm_pred == testY)
#94% accuracy
test$PredictedHC <-svm_pred

# try again with a different kernel
# with polydot kernel, C=100000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="polydot" ,C=100000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 0 to be hired
testaccuracy=mean(svm_pred == testY)
#96% accuracy because all are predicted not to be hired
test$PredictedHC <-svm_pred

# try again with a different kernel
# with laplacedot kernel, C=1000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="laplacedot" ,C=10^8,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 11 to be hired
testaccuracy=mean(svm_pred == testY)
#94.8% accuracy because almost all are predicted not to be hired
test$PredictedHC <-svm_pred
# # of the 11 predicted, 0 were correct in that year, but 9 were named head coaches
# at one point. so there is something good happening here. i wonder how to best 
#enfold that model knowledge 

# trying neural network 

library(neuralnet)

neuralnet_model <- neuralnet(BecameHC~Connections+net_ppa+net_pass_sr+role_DC1+Race+tenure_length, data= train) 
neural_pred <- predict(neuralnet_model,test)
print(neural_pred)
max(neural_pred)
sort(neural_pred)
# max is about 0.11, will first set threshold at 0.03
predicted_hire <- ifelse(neural_pred > 0.03, 1,0)
# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# this gives us 93.3% accuracy
# now let's look at this
test$PredictedHC <-predicted_hire
# # of the 21 predicted, 1 was correct in that year, but 3 were named head coaches
# at one point. # this is a much worse result than SVM and others

# IMO, SVM with rbf kernel is the most impressive so far
# With C=1000, 6/10 predictions were eventually named HCs
# With C=10000, 9/21 predictions were eventually named HCs



# Principal Component Analysis to reduce to 2 variables or Principal Components:
library(stats)
pca <- prcomp(coordinators_training_year_with_metrics_all[,c("Connections", 
                                                             "net_ppa", 
                                                             "net_pass_sr", 
                                                             "role_DC1",
                                                             "Race",
                                                             "tenure_length")], center = TRUE, scale. = TRUE)
summary(pca)
# So the 1st two Principal Components basically contains 57.71% of the useful information in the total dataset (explains 57.7% of the variance)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca)
# Connections and tenure link are closely related. So are net_ppa and net_pass_sr. This obviously makes sense.
reduced_data <- pca$x[,c(1,2)]
reduced_data[,'PC1']
reduced_data <- cbind.data.frame(coordinators_training_year_with_metrics_all$Coach, 
                                 coordinators_training_year_with_metrics_all$Race,
                                 coordinators_training_year_with_metrics_all$BecameHC,
                                 reduced_data[,'PC1'],
                                 reduced_data[,'PC2']) 
names(reduced_data) <- c('Coach', 'Race', 'BecameHC', 'PC1', 'PC2')


set.seed(5)
# now run Logistic Regression on the PC1 & PC2
pca_logistic = glm(BecameHC ~ PC1+PC2, data=reduced_data[,c(4,5,3)], family=binomial(link='logit'))
summary(pca_logistic)
# since logistic regression couldn't form a great decision boundary on the whole dataset, 
#and the 1st 2 Principal Components only explain 58% of the data, this just isn't going to be 
# useful.

########################################################################################
# Use the classification model from above that performed best to make predictions for 2022

target_year = 2022

d_coordinators_training_year <- defensive_coordinators1 %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators1 %>% filter(Season == target_year-1)
coordinators_training_year <- rbind(o_coordinators_training_year,d_coordinators_training_year)
coordinators_training_year <- coordinators_training_year %>% select(College, Season, Coach, Role, Race)
#Include race of each coach in this df.

# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1)
# (not just training set, not just coordinators)
coaching_tree_training_years <- coaching_tree %>% filter(Season >=target_year-15 & Season <=target_year-1)
coaching_tree_sums_training_years <- coaching_tree_training_years %>% 
  group_by(Coach, Coordinator) %>%
  dplyr::summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  dplyr::rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

head_coaches_training_years <- head_coaches1 %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators1 %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators1 %>% filter(Season>=target_year-15)
vertex_df <- head_coaches_training_years %>%
  rbind(d_coordinators_training_period) %>%
  rbind(o_coordinators_training_period) %>%
  select(Coach, Race) %>%
  distinct()
graph <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE, vertices = vertex_df)
v_name <- c()
n_neighbors <- c()
for(v in V(graph)$name) {
  v_name <- append(v_name, v)
  n_neighbors <- append(n_neighbors, 
                        length(neighbors(graph, v)))
}
v_name[which.max(n_neighbors)]
n_neighbors[which.max(n_neighbors)]
dt <- distance_table(graph, directed = TRUE)
# calculate Degree Centrality for all vertices. Degree Centrality is the # of edges connected to the vertex, a measure of immediate connection.
degree_centrality <- reshape2::melt(data.frame(as.list(degree(graph))))
# Closeness Centrality
closeness_centrality <- reshape2::melt(data.frame(as.list(closeness(graph))))
# Betweenness Centrality: a measure of how important a given vertex is in connecting other pairs of vertices in the graph. People with high Betweenness Centrality are known as Superconnectors (or networkers)
betweenness_centrality <- reshape2::melt(data.frame(as.list(betweenness(graph))))
# Eigenvector Centrality: A measure of overall influence (if you're equally interested in lots of direct connections as well as few connections to other highly connected people)
eigenvector_centrality <- reshape2::melt(data.frame(as.list(eigen_centrality(graph)$vector)))
# Now reformat the coach's names, add back race, and check mean or median degree centrality by race. Can do this for 1st-degree, 2nd-degree, etc. if we want.
eigenvector_centrality$metric <- "eigen"
betweenness_centrality$metric <- "betweenness"
closeness_centrality$metric <- "closeness"
degree_centrality$metric <- "degree"

melted_vertex <- eigenvector_centrality %>%
  rbind(betweenness_centrality) %>%
  rbind(closeness_centrality) %>%
  rbind(degree_centrality)
centrality_df <- melted_vertex %>% reshape2::dcast(variable ~ metric)
centrality_df$variable <- gsub("\\."," ", centrality_df$variable)
centrality_df <- centrality_df %>%
  left_join(vertex_df, by = c("variable" = "Coach"))

centrality_df <- centrality_df %>%
  dplyr::mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
                               ifelse(variable == "Joe Salave a", "Other",
                                      ifelse(variable == "O Neill Gilbert", "Black",
                                             ifelse(variable == "Brian Jean Mary", "Black",
                                                    ifelse(variable == "Re quan Boyette", "Black",
                                                           ifelse(variable == "J D Williams", "Black",
                                                                  ifelse(variable == "Maurice Crum Jr ", "Black",
                                                                         ifelse(variable == "Time Harris Jr ", "Black",
                                                                                Race)))))))))

centrality_df$Race <- ifelse(is.na(centrality_df$Race), "White", centrality_df$Race)
centrality_mean <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = mean(eigen),
                   betweenness = mean(betweenness),
                   closeness = mean(closeness),
                   degree = mean(degree))
# So white coaches are 1.43 times more connected than Black coaches (degree) on average.
centrality_median <- centrality_df %>% 
  group_by(Race) %>%
  dplyr::summarise(eigen = median(eigen),
                   betweenness = median(betweenness),
                   closeness = median(closeness),
                   degree = median(degree))
# To add centralities as vertex properties in graphs:
V(graph)$degree <- degree(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$eigen <- eigen_centrality(graph)$vector

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% dplyr::mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% dplyr::mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  dplyr::mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
                               ifelse(Coach == "A J  Ricker", "A.J. Ricker",
                                      ifelse(Coach == "Frank Cignetti Jr ", "Frank Cignetti Jr.",
                                             ifelse(Coach == "G J  Kinne", "G.J. Kinne", 
                                                    ifelse(Coach == "D J  Durkin", "D.J. Durkin",
                                                           ifelse(Coach == "A J  Milwee", "A.J. Milwee",
                                                                  ifelse(Coach == "Aazaar Abdul Rahim", "Aazaar Abdul-Rahim",
                                                                         ifelse(Coach == "Brian Jean Mary", "Brian Jean-Mary",
                                                                                ifelse(Coach == "Charlie Weis Jr ", "Charlie Weis Jr.",
                                                                                       ifelse(Coach == "D J  Eliot", "D.J. Eliot",
                                                                                              ifelse(Coach == "J C  Price", "J.C. Price",
                                                                                                     ifelse(Coach == "Joe Salave a", "Joe Salave'a",
                                                                                                            ifelse(Coach == "Mark D Onofrio", "Mark D'Onofrio",
                                                                                                                   ifelse(Coach == "Mike Sanford Jr ", "Mike Sanford Jr.",
                                                                                                                          ifelse(Coach == "Steve Spurrier Jr ", "Steve Spurrier Jr.",
                                                                                                                                 ifelse(Coach == "T J  Weist", "T.J. Weist",
                                                                                                                                        ifelse(Coach == "T J  Woods", "T.J. Woods",
                                                                                                                                               ifelse(Coach == 'Jim O Neil', "Jim O'Neil",
                                                                                                                                                      ifelse(Coach == 'Bill O Brien', "Bill O'Brien",
                                                                                                                                                             ifelse(Coach == 'Re quan Boyette', "Re'quan Boyette",
                                                                                                                                                                    ifelse(Coach == 'Tim Harris Jr ', "Tim Harris Jr.",
                                                                                                                                                                           
                                                                                                                                                                           Coach))))))))))))))))))))))

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Networking, Influence, Connections, InfluencePlus, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# save current year
coordinators_training_year_2022_with_metrics <- coordinators_training_year_with_metrics

# now want to change values in Role column to either be 0 or 1 (offense is 0, defense is 1)
coordinators_training_year_2022_with_metrics$role_DC1 <- ifelse(grepl("Defensive", coordinators_training_year_2022_with_metrics$Role), 1, NA)
coordinators_training_year_2022_with_metrics$role_DC1 <- ifelse(grepl("Offensive", coordinators_training_year_2022_with_metrics$Role), 0, coordinators_training_year_with_metrics_all$role_DC1)



tenure_df <- defensive_coordinators1 %>%
  rbind(offensive_coordinators1) %>%
  group_by(Coach) %>% 
  dplyr::summarise(tenure_length = n())
coordinators_training_year_2022_with_metrics <- coordinators_training_year_2022_with_metrics %>%
  left_join(tenure_df)
coordinators_training_year_2022_with_metrics <- coordinators_training_year_2022_with_metrics %>%
  dplyr::mutate(Race = ifelse(Race=="Black", 1, 0))

testX <- cbind(coordinators_training_year_2022_with_metrics$Connections, 
               coordinators_training_year_2022_with_metrics$net_ppa, 
               coordinators_training_year_2022_with_metrics$net_pass_sr, 
               coordinators_training_year_2022_with_metrics$role_DC1,
               coordinators_training_year_2022_with_metrics$Race,
               coordinators_training_year_2022_with_metrics$tenure_length)

# using model of SVM with laplacedot kernel, C=10000
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 2 to be hired
coordinators_training_year_2022_with_metrics$PredictedHC <-svm_pred
# Bill O'Brien and Jeff Grimes - good candidates to keep an eye on for future

testX2022 <- as.data.frame(cbind(coordinators_training_year_2022_with_metrics$Connections, 
                                 coordinators_training_year_2022_with_metrics$net_ppa, 
                                 coordinators_training_year_2022_with_metrics$net_pass_sr, 
                                 coordinators_training_year_2022_with_metrics$role_DC1,
                                 coordinators_training_year_2022_with_metrics$Race,
                                 coordinators_training_year_2022_with_metrics$tenure_length))
colnames(testX2022) <- c("Connections", "net_ppa", "net_pass_sr", "role_DC1", "Race", "tenure_length")
#using kknn model to predict
kmodel_predictions <- predict(kmodel, testX2022)
print(kmodel_predictions)
max(kmodel_predictions)
sort(kmodel_predictions)
# max is 0.375, 10 results >=0.25
# let's try a threshold of 0.2 after looking at the predictions
predicted_hire <- ifelse(kmodel_predictions > 0.2, 1,0)

# predicts 10 to be hired, all white
coordinators_training_year_2022_with_metrics$PredictedHC <-kmodel_predictions
