rm(list=ls())


# First, get matched sets that will be used for the paper 
source("scripts/Getting_matched_sets.R")

# Then, getting objects that will later be used for making figures of covariate balance
source("scripts/Preparing_covariate_balance.R")

save.image("data/processed/PanelMatch_temp1.RData")

# getting estimates 
source("Getting_PanelEstimates.R")

save.image("data/processed/PanelMatch_temp2.RData")