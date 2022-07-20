# Predicting Coach hires
# builds on Minority_Coach_Analysis.R



#Start with target year t=2021
# Filter training data set for coaches who were coordinators in t-1. 
#Include race of each coach in this df.
# Run Louvain on entire coaching data set from previous 15 years (2006-2020, or t-15 to t-1) 
# (not just training set, not just coordinators)
# Assign scores for each Louvain metric to each coach in the filtered training set
# Obtain performance metrics (averages) for each coach from 2005 to 2020 (t-1). 
# Join to the data frame.
# Create new columns populated with 1 if the coach became a HC in target year, 0 if they did not. 
# This will be the response variable.
