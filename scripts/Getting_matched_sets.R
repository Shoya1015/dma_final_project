##### This script creates matched sets that will be used to make figures for the paper #####
##### Please read the readme file first

# For annotations and naming conventions in this script, "Ace" denotes the study by Acemoglu et al., while 
# "SS" denotes the study by Scheve and Stasavage



options("max.print" = 1000,digits=4)
library(PanelMatch)
library(ggplot2)
library(gridExtra)


# load datasets

load("data/raw/Acemoglu.RData")
# # d3 <- d3[order(d3$ccode, d3$year), ] # reorder d3
# 
# d2$logpop <- 100 * log(d2$PopulationtotalSPPOPTOTL) # get logpop as done in the original paper
# 
# 
# # # modifying names
# # d3$name <- Hmisc::capitalize(d3$name)
# # d3$name[d3$name == "Uk"] <- "UK"
# # d3$name[d3$name == "Usa"] <- "USA"
# # d3$ccode <- as.numeric(d3$ccode)
# 
# # as.integer unit and time ids (currently required for the PanelMatch package)
# d2$wbcode2 <- as.integer(d2$wbcode2)
# d2$year <- as.integer(d2$year)
# 
# d3$ccode <- as.integer(d3$ccode)
# d3$year <- as.integer(d3$year)

### Getting matched sets ###
# Note that because the PanelMatch function now takes lists of arguments,
# we pre-define these lists here
## Universal Parameters ##
lags <- as.list(
  c(rep(rep(1, 7), 5),
    rep(rep(4, 7), 5))
) # the choice of lags


# this is to indicate whether for 
# a specific matched set we use refinement.
# this is not required by the function, but 
# will be used to name the matched sets object 
# this way, bookkeeping is made easier
whether_refinements <- as.list(rep(c("no", "no", "yes", "yes", "yes", "yes",
                                     "yes"), 10))

# the refinment methods
# note that later we will substitute "CBPS.msm.weight" for "CBPS.weight"
refinement.methods <- as.list(rep(c("none", "mahalanobis", "mahalanobis", "mahalanobis", "CBPS.match", "CBPS.match",
                                    "CBPS.weight"), 10))
match.missings <- as.list(rep(FALSE, 70)) # whether we match on missing data, currently we don't
listwise.deletes <- as.list(rep(TRUE, 70)) # we apply listwise deletion to all 

# qois
atts <- as.list(rep("att", 70))
atcs <- as.list(rep("atc", 70))

leads <- as.list(rep(c(rep(0,7), rep(1,7), rep(2,7), 
                       rep(3,7), rep(4,7)), 2)) # the choice of leads

forbid.treatment.reversals <- as.list(rep(F, 70)) # currently we allow treatment reversal, later will forbid
verboses <- as.list(rep(T, 70))

use.diagonal.variance.matrixs <- as.list(rep(T, 70)) # we use diagonal variance matrices for Maha
exact.match.variabless <- lapply(as.list(rep(0, 70)), 
                                 function(x) x <- NULL)

# Whether we match on treatment history - we will present balances without matching on history to demonstrate
# the utility of matching. So there is an "F" in addition to the 6 "T"s. 
matchings <- as.list(rep(c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 10))

## Parameters specific to Ace ##
time.ids <- as.list(paste(rep("year", 70))) # time ids
unit.ids <- as.list(paste(rep("wbcode2", 70))) # unit ids
treatments <- as.list(paste(rep("dem", 70))) # treatment
# formulae for both L = 1 (the first 35 formulae) and L = 4 (the second 35 formulae)
covs.formulae <- as.list(lapply(c(
  rep("~ I(lag(Populationages014oftotal, 1)) + 
      I(lag(Populationages1564oftota, 1)) + I(lag(unrest, 1)) + 
      I(lag(tradewb, 1)) + I(lag(nfagdp, 1)) + I(lag(logpop, 1))", 35), 
  rep("~ I(lag(Populationages014oftotal, 1:4)) + 
      I(lag(Populationages1564oftota, 1:4)) + I(lag(unrest, 1:4)) + 
      I(lag(tradewb, 1:4)) + I(lag(nfagdp, 1:4)) + I(lag(logpop, 1:4))", 35)),
  as.formula))
# match sizes
size.matches <- as.list(rep(c(10, 5, 5, 10, 5, 10, 10), 10))
outcome.vars <- as.list(rep("y", 70)) # outcome variables
# covariates
covariates_Ace = c("y", "unrest", 
                   "logpop", 
                   "Populationages014oftotal",
                   "Populationages1564oftota",
                   "tradewb", "nfagdp"
)

### Note that for this script, "nonrestricted" means allowing for treatment reversal, 
# while "restricted" means not allowing for treatment reversal

# Getting matched sets for ATT in Acemolgu, allowing for treatment reversal

Ace_att <- PanelMatch(lag = lags, 
                      time.id = time.ids,
                      unit.id = unit.ids,
                      treatment = treatments,
                      refinement.method = refinement.methods,
                      data = d2,
                      match.missing = match.missings,
                      listwise.delete = listwise.deletes,
                      covs.formula = covs.formulae,
                      size.match = size.matches,
                      qoi = atts,
                      outcome.var = outcome.vars,
                      lead = leads,
                      forbid.treatment.reversal = forbid.treatment.reversals,
                      verbose = verboses,
                      # listwise.delete = list(F,F, F, F, F),
                      use.diagonal.variance.matrix = use.diagonal.variance.matrixs,
                      exact.match.variables = exact.match.variabless,
                      matching = matchings
)


names(Ace_att) <- paste(unlist(lags), 
                        unlist(leads), 
                        unlist(whether_refinements),
                        unlist(refinement.methods),
                        unlist(size.matches), matchings, atts) # naming the list

# getting the same matched sets for ATC
Ace_atc <- PanelMatch(lag = lags, 
                      time.id = time.ids,
                      unit.id = unit.ids,
                      treatment = treatments,
                      refinement.method = refinement.methods,
                      data = d2,
                      match.missing = match.missings,
                      listwise.delete = listwise.deletes,
                      covs.formula = covs.formulae,
                      size.match = size.matches,
                      qoi = atcs,
                      outcome.var = outcome.vars,
                      lead = leads,
                      forbid.treatment.reversal = forbid.treatment.reversals,
                      verbose = verboses,
                      # listwise.delete = list(F,F, F, F, F),
                      use.diagonal.variance.matrix = use.diagonal.variance.matrixs,
                      exact.match.variables = exact.match.variabless,
                      matching = matchings
)

names(Ace_atc) <- paste(unlist(lags), 
                        unlist(leads), 
                        unlist(whether_refinements),
                        unlist(refinement.methods),
                        unlist(size.matches), matchings,
                        atcs) # naming the list

# Getting matched sets for ATT in Acemolgu, not allowing for treatment reversal
# we use the word "restricted" to denote objects for "not allowing for treatment reversal"
forbid.treatment.reversals <- as.list(rep(T, 70))
refinement.methods <- as.list(rep(c("none", "mahalanobis", "mahalanobis", "mahalanobis", "CBPS.match", "CBPS.match",
                                    "CBPS.msm.weight"), 10))

# getting matched sets for ATT
Ace_att_restricted <- PanelMatch(lag = lags, 
                                 time.id = time.ids,
                                 unit.id = unit.ids,
                                 treatment = treatments,
                                 refinement.method = refinement.methods,
                                 data = d2,
                                 match.missing = match.missings,
                                 listwise.delete = listwise.deletes,
                                 covs.formula = covs.formulae,
                                 size.match = size.matches,
                                 qoi = atts,
                                 outcome.var = outcome.vars,
                                 lead = leads,
                                 forbid.treatment.reversal = forbid.treatment.reversals,
                                 verbose = verboses,
                                 # listwise.delete = list(F,F, F, F, F),
                                 use.diagonal.variance.matrix = use.diagonal.variance.matrixs,
                                 exact.match.variables = exact.match.variabless,
                                 matching = matchings
)


names(Ace_att_restricted) <- paste(unlist(lags), 
                                   unlist(leads), 
                                   unlist(whether_refinements),
                                   unlist(refinement.methods),
                                   unlist(size.matches), matchings, atts) # naming the list

# getting the same matched sets for ATC
Ace_atc_restricted <- PanelMatch(lag = lags, 
                                 time.id = time.ids,
                                 unit.id = unit.ids,
                                 treatment = treatments,
                                 refinement.method = refinement.methods,
                                 data = d2,
                                 match.missing = match.missings,
                                 listwise.delete = listwise.deletes,
                                 covs.formula = covs.formulae,
                                 size.match = size.matches,
                                 qoi = atcs,
                                 outcome.var = outcome.vars,
                                 lead = leads,
                                 forbid.treatment.reversal = forbid.treatment.reversals,
                                 verbose = verboses,
                                 # listwise.delete = list(F,F, F, F, F),
                                 use.diagonal.variance.matrix = use.diagonal.variance.matrixs,
                                 exact.match.variables = exact.match.variabless,
                                 matching = matchings
)

names(Ace_atc_restricted) <- paste(unlist(lags), 
                                   unlist(leads), 
                                   unlist(whether_refinements),
                                   unlist(refinement.methods),
                                   unlist(size.matches), matchings,
                                   atcs) # naming the list