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

d_coordinators_training_year <- defensive_coordinators %>% filter(Season ==target_year-1)
o_coordinators_training_year <- offensive_coordinators %>% filter(Season == target_year-1)
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

head_coaches_training_years <- head_coaches %>% filter(Season >=target_year-15)
d_coordinators_training_period <- defensive_coordinators %>% filter(Season>=target_year-15)
o_coordinators_training_period <- offensive_coordinators %>% filter(Season>=target_year-15)
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

centrality_df_modified <- centrality_df %>% select(variable, closeness, degree, Race)
colnames(centrality_df_modified) <- c("Coach", "Influence", "Connections",  "Race")
centrality_df_scaled <- centrality_df_modified %>% mutate_at("Influence", ~(scale(., center = FALSE) %>% as.vector))
centrality_df_scaled <- centrality_df_scaled%>% mutate_if(is.numeric, round, digits = 2)

# Assign scores for each Louvain metric to each coach in the filtered data set for training year

coordinators_training_year_with_louvain <- coordinators_training_year %>% left_join(centrality_df_scaled, by = "Coach")
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1).
# Join to the data frame.
# merge(df1, df2, by.x=c('col1', 'col2'), by.y=c('col1', 'col2'))
coordinators_training_year_with_dcmetrics <-merge(coordinators_training_year_with_louvain, dc_impact_results1, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_ocmetrics <-merge(coordinators_training_year_with_louvain, oc_impact_results1, by.x = c("Coach", "College"), by.y = c("Coach", "team"))
coordinators_training_year_with_metrics <- rbind(coordinators_training_year_with_dcmetrics, coordinators_training_year_with_ocmetrics) %>% select(Coach, College, Season, Role, Influence, Connections, Race, net_ppa, net_sr, net_stuff, net_pass_sr)

# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.

head_coaches_target_year <- head_coaches %>% filter(Season == target_year)

coordinators_became_hc_target_year <- coordinators_training_year %>% inner_join(head_coaches_target_year, by = "Coach")

#A[A$C %in% B$C,  ]
# data$num1[data$num1 == 1] <- 99  

coordinators_training_year_with_metrics <- coordinators_training_year_with_metrics%>% mutate("BecameHC"=0)
coordinators_training_year_with_metrics$BecameHC[coordinators_training_year_with_metrics$Coach %in% coordinators_became_hc_target_year$Coach] <- 1

# split into training/test

#make this example reproducible
set.seed(5)

#create ID column
coordinators_training_year_with_metrics$id <- 1:nrow(coordinators_training_year_with_metrics)

#use 70% of dataset as training set and 30% as test set 
train <- coordinators_training_year_with_metrics %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(coordinators_training_year_with_metrics, train, by = 'id')

# run classification algorithms on training

# start with basic Logistic Regression

coach_predictions_log_model = glm(BecameHC ~ Influence+Connections+Race+net_ppa+net_sr+net_stuff+net_pass_sr, data=train, family=binomial)
summary(coach_predictions_log_model)

# predict on test data and compare to actual 
coach_predictions_log_model_predict = predict(coach_predictions_log_model, test, type="response")
# predictions need to be translated into hire or no hire and then compared to BecameHC
# repeat for future years...
