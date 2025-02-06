
######################### Getting outputs used for plotting balances ###########################
# Acemoglu, allowing for treatment reversal
balance_Ace_nonrestricted_att <- list() # this list stores ATT
balance_Ace_nonrestricted_atc <- list() # this list stores ATC
for (i in 1:length(Ace_att)){
  if(grepl("no ", names(Ace_att)[i])) {
    # if refinement is not used, then we let the balance
    # function use *equal weights* - a.k.a not really refining for anything
    use.equal.weights <- T
  } else {
    use.equal.weights <- F
  }
  
  balance_Ace_nonrestricted_att[[i]] <-
    get_covariate_balance(matched.sets = Ace_att[[i]][1]$att,# att or atc
                          data = d2, 
                          covariates = covariates_Ace, 
                          use.equal.weights = use.equal.weights)  
  
  balance_Ace_nonrestricted_atc[[i]] <-
    get_covariate_balance(matched.sets = Ace_atc[[i]][1]$atc,# att or atc
                          data = d2, 
                          covariates = covariates_Ace, 
                          use.equal.weights = use.equal.weights)  
}

# SS, allowing for treatment reversal
balance_SS_nonrestricted_att <- list() # this list stores ATT
for (i in 1:length(SS_att)){
  if(grepl("no ", names(SS_att)[i])) {
    # if refinement is not used, then we let the balance
    # function use *equal weights* - a.k.a not really refining for anything
    use.equal.weights <- T
  } else {
    use.equal.weights <- F
  }
  
  balance_SS_nonrestricted_att[[i]] <-
    get_covariate_balance(matched.sets = SS_att[[i]][1]$att,# att or atc
                          data = d3, 
                          covariates = covariates_SS, 
                          use.equal.weights = use.equal.weights)  
  
}


# Acemoglu, not allowing for treatment reversal 
balance_Ace_restricted_att <- list() # this list stores ATT
balance_Ace_restricted_atc <- list() # this list stores ATC
for (i in 1:length(Ace_att_restricted)){
  if(grepl("no ", names(Ace_att_restricted)[i])) {
    # if refinement is not used, then we let the balance
    # function use *equal weights* - a.k.a not really refining for anything
    use.equal.weights <- T
  } else {
    use.equal.weights <- F
  }
  
  balance_Ace_restricted_att[[i]] <-
    get_covariate_balance(matched.sets = Ace_att_restricted[[i]][1]$att,# att or atc
                          data = d2, 
                          covariates = covariates_Ace, 
                          use.equal.weights = use.equal.weights)  
  
  balance_Ace_restricted_atc[[i]] <-
    get_covariate_balance(matched.sets = Ace_atc_restricted[[i]][1]$atc,# att or atc
                          data = d2, 
                          covariates = covariates_Ace, 
                          use.equal.weights = use.equal.weights)  
}

# SS, not allowing for treatment reversal
balance_SS_restricted_att <- list() # this list stores ATT
for (i in 1:length(SS_att_restricted)){
  if(grepl("no ", names(SS_att_restricted)[i])) {
    # if refinement is not used, then we let the balance
    # function use *equal weights* - a.k.a not really refining for anything
    use.equal.weights <- T
  } else {
    use.equal.weights <- F
  }
  
  balance_SS_restricted_att[[i]] <-
    get_covariate_balance(matched.sets = SS_att_restricted[[i]][1]$att,# att or atc
                          data = d3, 
                          covariates = covariates_SS, 
                          use.equal.weights = use.equal.weights)  
  
}

# naming the lists
names(balance_Ace_nonrestricted_att) <- names(Ace_att)
names(balance_Ace_nonrestricted_atc) <- names(Ace_atc)
names(balance_SS_nonrestricted_att) <- names(SS_att)

## Putting SS and Ace together, allowing for treatment reversal 
# att
all_nonrestricted <- list("L1_Ace" = balance_Ace_nonrestricted_att[substr(names(balance_Ace_nonrestricted_att), start = 1, stop = 1)=="1"],
                          "L4_Ace" = balance_Ace_nonrestricted_att[substr(names(balance_Ace_nonrestricted_att), start = 1, stop = 1)=="4"],
                          "L1_SS" = balance_SS_nonrestricted_att[substr(names(balance_SS_nonrestricted_att), start = 1, stop = 1)=="1"],
                          "L4_SS" = balance_SS_nonrestricted_att[substr(names(balance_SS_nonrestricted_att), start = 1, stop = 1)=="4"])
# atc
all_nonrestricted_atc <- list("L1_Ace" = balance_Ace_nonrestricted_atc[substr(names(balance_Ace_nonrestricted_atc), start = 1, stop = 1)=="1"],
                              "L4_Ace" = balance_Ace_nonrestricted_atc[substr(names(balance_Ace_nonrestricted_atc), start = 1, stop = 1)=="4"])

# further naming
names(balance_Ace_restricted_att) <- names(Ace_att_restricted)
names(balance_Ace_restricted_atc) <- names(Ace_atc_restricted)
names(balance_SS_restricted_att) <- names(SS_att_restricted)

## Putting SS and Acemoglu together, not allowing for treatment reversal
# att
all_restricted <- list("L1_Ace" = balance_Ace_restricted_att[substr(names(balance_Ace_restricted_att), start = 1, stop = 1)=="1"],
                       "L4_Ace" = balance_Ace_restricted_att[substr(names(balance_Ace_restricted_att), start = 1, stop = 1)=="4"],
                       "L1_SS" = balance_SS_restricted_att[substr(names(balance_SS_restricted_att), start = 1, stop = 1)=="1"],
                       "L4_SS" = balance_SS_restricted_att[substr(names(balance_SS_restricted_att), start = 1, stop = 1)=="4"])
# atc
all_restricted_atc <- list("L1_Ace" = balance_Ace_restricted_atc[substr(names(balance_Ace_restricted_atc), start = 1, stop = 1)=="1"],
                           "L4_Ace" = balance_Ace_restricted_atc[substr(names(balance_Ace_restricted_atc), start = 1, stop = 1)=="4"])

# save.image(file='myEnvironment.RData')