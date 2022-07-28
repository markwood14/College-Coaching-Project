# Predicting Coach hires
# builds on Minority_Coach_Analysis.R

#packages used in original work
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
library(RColorBrewer)
# Packages for Louvain network analysis:
library(igraph)
library(qgraph)
library(corrplot)
library(Hmisc)
library(ggraph)
library(networkD3)

# Dataframes used from Minority_Coach_Analysis.R:
load("defensive_coordinators1.Rda")
load("offensive_coordinators1.Rda")
load("coaching_tree.Rda")
load("head_coaches1.Rda")
load("dc_impact_results.Rda")
load("oc_impact_results.Rda")




#Start with target year t=2021
target_year = 2021

# Filter data set for coaches who were coordinators in t-1. 

# List of dataframes used from Minority_Coach_Analysis.R:
# defensive_coordinators1 (not 'defensive_coordinators'?)
# offensive_coordinators1 (not 'offensive_coordinators'?)
# coaching_tree
# head_coaches1 (not 'head_coaches'?)
# dc_impact_results (not 'dc_impact_results1'?)
# oc_impact_results (not 'oc_impact_results1'?)



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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))
# So white coaches are 1.43 times more connected than Black coaches (degree) on average.
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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))

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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))

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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))

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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))

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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))

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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
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
  summarise(years_together = n())
edges_df <- coaching_tree_sums_training_years %>%
  rename(c("from" = "Coach", "to" = "Coordinator", "weight" = "years_together"))

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
  mutate(Race == ifelse(variable == "Aazaar Abdul Rahim", "Black",
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
  summarise(eigen = mean(eigen),
            betweenness = mean(betweenness),
            closeness = mean(closeness),
            degree = mean(degree))

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

centrality_df_modified <- centrality_df %>% select(variable, betweenness, closeness, degree, eigen, Race)
colnames(centrality_df_modified) <- c("Coach", "Networking", "Influence", "Connections",  "InfluencePlus", "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at(c("Networking", "Influence", "InfluencePlus"), ~(scale(.) %>% as.vector))
# centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# the coaches in this DF have no punctuation in their names...
centrality_df_scaled <- centrality_df_scaled %>% 
  mutate(Coach = ifelse(Coach == "Maurice Crum Jr ", "Maurice Crum Jr.",
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

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# save current year

coordinators_training_year_2015_with_metrics <- coordinators_training_year_with_metrics

# stop after target_year=2015

# bind with previous years

coordinators_training_year_with_metrics_all <- rbind(coordinators_training_year_2021_with_metrics,
                                                     coordinators_training_year_2020_with_metrics,
                                                     coordinators_training_year_2019_with_metrics,
                                                     coordinators_training_year_2018_with_metrics,
                                                     coordinators_training_year_2017_with_metrics,
                                                     coordinators_training_year_2016_with_metrics,
                                                     coordinators_training_year_2015_with_metrics)

# deal with Na's
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
  summarise(tenure_length = n())
coordinators_training_year_with_metrics_all <- coordinators_training_year_with_metrics_all %>%
  left_join(tenure_df)
coordinators_training_year_with_metrics_all <- coordinators_training_year_with_metrics_all %>%
  mutate(Race = ifelse(Race=="Black", 1, 0))

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
# it looks look about 0.20 is the highest prediction anyone gets
# so let's try with a threshold of 0.07

predicted_hire <- ifelse(coach_predictions_log_model_predict > 0.07, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
#93.8% accuracy with 0.07 threshold
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
# max is 0.5, 18 results >=0.25
# let's try a threshold of 0.2 after looking at the predictions
predicted_hire <- ifelse(kmodel_predictions > 0.2, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# this gives us 94.2% accuracy
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
  mutate(BecameHC = ifelse(BecameHC == 0, -1, 1)) %>%
  select(BecameHC)
trainY <- trainY$BecameHC
testX <- cbind(test$Connections, 
               test$net_ppa, 
               test$net_pass_sr, 
               test$role_DC1,
               test$Race,
               test$tenure_length)
testY <- test %>% 
  mutate(BecameHC = ifelse(BecameHC == 0, -1, 1)) %>%
  select(BecameHC)
testY <- testY$BecameHC
# obtained tree_depth and n_rounds just from some guessing & checking
boost <- adaboost(trainX, trainY, tree_depth = 2, n_rounds = 500)
pred <- predict(boost, testX)
# misclassification rate:
mean(testY != pred)
test$PredictedHC <-pred
boost$confusion_matrix = table(testY, pred)
boost
# predicted 9 to become HC, none are correct. 2 eventually become HCs
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
xboost <- xgb.train(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, eval.metric = "error", eval.metric = "logloss", watchlist=watchlist, objective = "binary:hinge") #objective = "binary:logistic" #objective = "multi:softmax", num_class = 2 <- neither of these did any better...
pred <- predict(xboost, testX)
print(pred)
max(pred)
sort(pred)
# gives 6 values of 1, the rest 0
predicted_hire <- ifelse(pred > 0.1, 1,0)
mean(testY != predicted_hire)
xboost$confusion_matrix = table(testY, predicted_hire)
xboost$confusion_matrix
# predicts 6 hires, none are correct

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
# predicts 10 to be hired
testaccuracy=mean(svm_pred == testY)
#94.5% accuracy
test$PredictedHC <-svm_pred
# of the ten predicted, none were correct in that year, but 6 were named head coaches
# at one point. so there is something good happening here. i wonder how to best 
#enfold that model knowledge

# try again with a different C value
# with RBF kernel, C= 10000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="rbfdot" ,C=10000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 21 to be hired
testaccuracy=mean(svm_pred == testY)
#94% accuracy
test$PredictedHC <-svm_pred
# of the 21 predicted, 1 was correct in that year, but 9 were named head coaches
# at one point. so there is something good happening here. i wonder how to best 
#enfold that model knowledge 

# try again with a different kernel
# with polydot kernel, C=10000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="polydot" ,C=10000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 0 to be hired
testaccuracy=mean(svm_pred == testY)
#96.7% accuracy because almost all are predicted not to be hired
test$PredictedHC <-svm_pred

# try again with a different kernel
# with laplacedot kernel, C=10000
svm_model <-  ksvm(trainX,trainY,type="C-svc",kernel="laplacedot" ,C=10000,scaled=TRUE)
svm_pred <- predict(svm_model,testX)
print(svm_pred)
max(svm_pred)
sort(svm_pred)
# predicts 13 to be hired
testaccuracy=mean(svm_pred == testY)
#94.8% accuracy because almost all are predicted not to be hired
test$PredictedHC <-svm_pred
# # of the 13 predicted, 0 were correct in that year, but 6 were named head coaches
# at one point. so there is something good happening here. i wonder how to best 
#enfold that model knowledge 

# trying neural network 

install.packages('neuralnet')
library(neuralnet)

neuralnet_model <- neuralnet(BecameHC~Connections+net_ppa+net_pass_sr+role_DC1+Race+tenure_length, data= train) 
neural_pred <- predict(neuralnet_model,test)
print(neural_pred)
max(neural_pred)
sort(neural_pred)
# max is about 0.09, will first set threshold at 0.065
predicted_hire <- ifelse(neural_pred > 0.065, 1,0)
# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# this gives us 93.3% accuracy
# now let's look at this
test$PredictedHC <-predicted_hire
testaccuracy=mean(svm_pred == testY)
#94.8% accuracy because almost all are predicted not to be hired
test$PredictedHC <-svm_pred
# # of the 22 predicted, 1 was correct in that year, but 3 were named head coaches
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
# since logistic regression couldn't form a great decision boundary on the whole dataset, and the 1st 2 Principal Components only explain 58% of the data, this just isn't going to be useful.
