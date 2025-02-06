## Acemoglu
# pre-define some parameters
data <- d2
multiplier <- 1/100 # this only matters to Ace, as we wanna use the scale adopted by the original study
CI <- .95
se.method <- "conditional"

## allowing for treatment reversal
# get all the estimation results. THIS STEP TAKES A LONG TIME TO RUN
results_Ace_att <- list()
for (i in 1:length(Ace_att)) {
  set.seed(2019)
  # calling the "PanelEstimate()"
  results <- PanelEstimate(#inference = "bootstrap", 
    number.iterations = 500,
    se.method = se.method,
    data = data, sets = Ace_att[[i]])  

  P <- results$estimates*multiplier
  CIs <- as.numeric(summary(results)$summary[,3:4, drop = F])*multiplier
  #}
  L <- CIs[1]
  U <- CIs[2]
  results_Ace_att[[i]] <- list("P" = P, "L" = L, "U" = U)
  
}

results_Ace_atc <- list()
for (i in 1:length(Ace_atc)) {
  set.seed(2019)
  # calling the "PanelEstimate()"
  results <- PanelEstimate(#inference = "bootstrap", 
    number.iterations = 500,
    se.method = se.method,
    data = data, sets = Ace_atc[[i]])  
  P <- results$estimates*multiplier
  CIs <- as.numeric(summary(results)$summary[,3:4, drop = F])*multiplier
  #}
  L <- CIs[1]
  U <- CIs[2]
  results_Ace_atc[[i]] <- list("P" = P, "L" = L, "U" = U)
  
}

names(results_Ace_att) <- names(Ace_att)
names(results_Ace_atc) <- names(Ace_atc)


## Not allowing for treatment reversal
# getting all the results: THIS STEP TAKES LONG TO RUN

data <- d2
multiplier <- 1/100 # this only matters to Ace, as we wanna use the scale adopted by the original study
CI <- .95

results_Ace_att_restricted <- list()
for (i in 1:length(Ace_att_restricted)) {
  set.seed(2019)
  # calling the "PanelEstimate()"
  results <- PanelEstimate(#inference = "bootstrap", 
    number.iterations = 500,
    se.method = se.method,
    data = data, sets = Ace_att_restricted[[i]])  
  P <- results$estimates*multiplier
  CIs <- as.numeric(summary(results)$summary[,3:4, drop = F])*multiplier
  #}
  L <- CIs[1]
  U <- CIs[2]
  results_Ace_att_restricted[[i]] <- list("P" = P, "L" = L, "U" = U)
  
}

results_Ace_atc_restricted <- list()
for (i in 1:length(Ace_atc_restricted)) {
  set.seed(2019)
  # calling the "PanelEstimate()"
  results <- PanelEstimate(#inference = "bootstrap", 
    number.iterations = 500,
    se.method = se.method,
    data = data, sets = Ace_atc_restricted[[i]])  
  P <- results$estimates*multiplier
  CIs <- as.numeric(summary(results)$summary[,3:4, drop = F])*multiplier
  #}
  L <- CIs[1]
  U <- CIs[2]
  results_Ace_atc_restricted[[i]] <- list("P" = P, "L" = L, "U" = U)
  
}

names(results_Ace_att_restricted) <- names(Ace_att_restricted)
names(results_Ace_atc_restricted) <- names(Ace_atc_restricted)

### SS
# pre-define parameters
data <- d3
multiplier <- 1
CI <- .95

## allowing for treatment reversal
results_SS_att <- list()
for (i in 1:length(SS_att)) {
  set.seed(2019)
  # calling the "PanelEstimate()"
  results <- PanelEstimate(#inference = "bootstrap", 
    number.iterations = 500,
    se.method = se.method,
    data = data, sets = SS_att[[i]])  
 
  P <- results$estimates*multiplier
  CIs <- as.numeric(summary(results)$summary[,3:4, drop = F])*multiplier
  #}
  L <- CIs[1]
  U <- CIs[2]
  results_SS_att[[i]] <- list("P" = P, "L" = L, "U" = U)
  
}

## not allowing for treatment reversal
results_SS_att_restricted <- list()
for (i in 1:length(SS_att_restricted)) {
  set.seed(2019)
  # calling the "PanelEstimate()"
  results <- PanelEstimate(#inference = "bootstrap", 
    number.iterations = 500,
    se.method = se.method,
    data = data, sets = SS_att_restricted[[i]])  
  P <- results$estimates*multiplier
  CIs <- as.numeric(summary(results)$summary[,3:4, drop = F])*multiplier
  #}
  L <- CIs[1]
  U <- CIs[2]
  results_SS_att_restricted[[i]] <- list("P" = P, "L" = L, "U" = U)
  
}

names(results_SS_att) <- names(SS_att)
names(results_SS_att_restricted) <- names(SS_att_restricted)
# save.image(file='myEnvironment.RData')
