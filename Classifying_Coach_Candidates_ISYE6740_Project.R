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

# inserting missing values from centrality_df_scaled, didn't transfer properly for coaches with punctuation in their name
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Durkin", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Durkin", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Durkin", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Durkin", 8] <- -0.054605147

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "A.J. Milwee", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "A.J. Milwee", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "A.J. Milwee", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "A.J. Milwee", 8] <- -0.054605147

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Aazaar Abdul-Rahim", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Aazaar Abdul-Rahim", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Aazaar Abdul-Rahim", 7] <- 0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Aazaar Abdul-Rahim", 8] <- -0.0503580954

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Brian Jean-Mary", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Brian Jean-Mary", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Brian Jean-Mary", 7] <- 0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Brian Jean-Mary", 8] <- -0.0503580954

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Charlie Weis Jr.", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Charlie Weis Jr.", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Charlie Weis Jr.", 7] <- 0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Charlie Weis Jr.", 8] <- -0.0503580954

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Eliot", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Eliot", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Eliot", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "D.J. Eliot", 8] <- -0.054605147

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "J.C. Price", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "J.C. Price", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "J.C. Price", 7] <- 0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "J.C. Price", 8] <- -0.050358095

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Joe Salave'a", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Joe Salave'a", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Joe Salave'a", 7] <- 0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Joe Salave'a", 8] <- -0.050358095

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mark D'Onofrio", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mark D'Onofrio", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mark D'Onofrio", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mark D'Onofrio", 8] <- -0.027674484

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mike Sanford Jr.", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mike Sanford Jr.", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mike Sanford Jr.", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Mike Sanford Jr.", 8] <- -0.05288769

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Steve Spurrier Jr.", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Steve Spurrier Jr.", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Steve Spurrier Jr.", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "Steve Spurrier Jr.", 8] <- -0.05460515

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Weist", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Weist", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Weist", 7] <- 1
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Weist", 8] <- -0.05172009

coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Woods", 5] <- -0.19565156
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Woods", 6] <- -0.29642807
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Woods", 7] <- 0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Coach == "T.J. Woods", 8] <- -0.0503581

# NA issue is resolved. 

# now want to change values in Role column to either be 0 or 1 (offense is 0, defense is 1)
# you are going to need to use some str_detect here, i did multiple steps but pasted over
# **sorry! I can show you what is needed, i was in a hurry to finish
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Role== 0, 4] <-0.0
coordinators_training_year_with_metrics_all[coordinators_training_year_with_metrics_all$Role== 1, 4] <-1.0
coordinators_training_year_with_metrics_all[,4] <-sapply(coordinators_training_year_with_metrics_all[,4], as.numeric)

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
a <- seq(0.1, 0.9, 0.05)
mdlX <- coordinators_training_year_with_metrics_all %>% select(Role, Networking, Influence, Connections, InfluencePlus, net_ppa, net_sr, net_stuff, net_pass_sr)
mdlY <- coordinators_training_year_with_metrics_all %>% select(BecameHC)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
##*** getting an error here because of datatype stuff but cant figure out why**
#cv3 <- search[search$cvm == min(search$cvm), ]
elasticNetModel <- glmnet(mdlX, mdlY, family = "binomial", lambda = 0.1, alpha = 0.5)
coef(md3)

# start with basic Logistic Regression
coach_predictions_log_model = glm(BecameHC ~ Influence+Connections+net_ppa+net_sr+net_stuff+net_pass_sr, data=train, family=binomial)
summary(coach_predictions_log_model)

# net_ppa is significant below 0.01
#connections is close, .07
#net_pass_sr is third, 0.11
# running again with those three

coach_predictions_log_model_reduced = glm(BecameHC ~ Connections+net_ppa, data=train, family=binomial)
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
# We get 96% accuracy because this doesn't predict anyone to get hired! 
# need to try again at a different threshold

print(coach_predictions_log_model_predict)
# it looks look about 0.09 is the highest prediction anyone gets
# so let's try with a threshold of 0.06?

predicted_hire <- ifelse(coach_predictions_log_model_predict > 0.06, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# now let's look at this
test$PredictedHC <-predicted_hire
# at this threshold, we predict 37 to become HCs. 22 do, and we get 2 right
# overall accuracy is 0.90

# try again at 0.07
predicted_hire <- ifelse(coach_predictions_log_model_predict > 0.07, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# now let's look at this
test$PredictedHC <-predicted_hire
# 92.4% accuracy
# now we predict 25 to become HCs (many are duplicates of each other, 
# had good multi-year runs)

# let's try to implement KNN

library(kknn)

kmodel <- train.kknn(BecameHC~Connections+net_ppa, train, kmax=10, kernel = "rectangular", scale = TRUE)
kmodel_predictions <- predict(kmodel, test)

print(kmodel_predictions)
# let's try a threshold of 0.1 after looking at the predictions
predicted_hire <- ifelse(kmodel_predictions > 0.1, 1,0)

# assess accuracy, how many predictions correct?
mean(predicted_hire==test$BecameHC)
# this gives us 93.6% accuracy
# now let's look at this
test$PredictedHC <-predicted_hire
# predicts 16 to become HCs, only one of those is correct
