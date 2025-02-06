rm(list=ls())

## please set the working directory
setwd("~/gitrepos/tscs/code/AJPS_tscs_replication/")

## check/create the folder to put outputs
if (file.exists(file.path(getwd(), "output"))){
    OUT_DIR <- file.path(getwd(), "output")
} else {
    dir.create(file.path(file.path(getwd(), "output")))
    OUT_DIR <- file.path(getwd(), "output")
}

## This script takes the environment saved in Getting_PanelEstimates.R
## and Getting_PanelEstimates.R

load("./PanelMatch_temp1.RData")
load("./PanelMatch_temp2.RData")

## making PDFs
# L = 4 
number_total <- 10
nrow_plot <- 2
result.matrix <- matrix(1:number_total, nrow = nrow_plot, ncol = number_total/nrow_plot)
result.matrix[1,] <- 1:(number_total/nrow_plot)
result.matrix[2,] <- (number_total/nrow_plot + 1):(number_total/nrow_plot + number_total/nrow_plot)
post_periods <- 5
pdf(file = file.path(OUT_DIR, "Figure6.pdf"), width = 10, height = 6)
layout(
  result.matrix
)
specific_lag <- "4"
par(mar = c(1.5, 2, 2 , 1), oma = c(4, 4,1.5, 0))
for (i in 1:10) {
  plot(1, xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n",
       xlim = c(-.5,4.5),
       ylim = c(-.15, .05), pch = 16, cex = 1.4) 
  k <- 1
  if (i == 1|i == 6) {
    method <- "mahalanobis 5"
  } else if (i == 2|i == 7) {
    method <- "mahalanobis 10"
  } else if (i == 3|i == 8) {
    method <- "CBPS.match 5"
  } else if (i == 4|i == 9) {
    method <- "CBPS.match 10"
  } else {
    method <- "CBPS.weight 10"
  }
  # inside each plot
  #for (k in 1:length(all_L4)){
  qoi <- ifelse(i <= 5, "att", "atc")
  if (qoi == "att") {
    tmp_results <- results_Ace_att
  } else {
    tmp_results <- results_Ace_atc
  }
  # index <- ifelse(i<=5, i, i-5)
  one_row <- tmp_results[substr(names(tmp_results), 1,1) == specific_lag & 
                           stringr::str_detect(names(tmp_results), "no ") == FALSE & 
                           stringr::str_detect(names(tmp_results), method)]
  
  
  
  reverse <- ifelse(qoi == "atc", -1, 1)
  P <- sapply(one_row, function(x) x[[1]]) * reverse
  L <- sapply(one_row, function(x) x[[2]]) * reverse
  U <- sapply(one_row, function(x) x[[3]]) * reverse
  if (k == 1){
    pch <- 16
    x_c <- 0:(length(as.numeric(P))-1)
  } else {
    pch <- 25
    x_c <-seq(0.25, 4.25, 1) 
  }
  # pch <- ifelse(k == 1, 16, 25)
  # x_c <- ifelse(k == 1, 0:(length(as.numeric(P))-1), 
  #               seq(0.25, 4.25, 1))
  if (i == 3&k==2|i == 4&k==2|i == 8&k==2|i == 9&k==2) {
    P <- U <- L <- rep(NA, length(P))
  }
  points(x_c, P, pch = pch)
  for (j in x_c) {
    lines(c(j,j), c(L[j+1], U[j+1]))
  }
  abline(h = 0, lty = 3)
  
  if (i == 1) {
    mtext(3, text = "Up to 5 matches", line = 0.17)
    
    # legend(x = .6, y = .2,  
    #        legend = c("Treatment reversal \n allowed",
    #                   "Treatment reversal \n not allowed"),
    #        y.intersp = 2,
    #        # x.intersp = 0.3,
    #        xjust = 0,
    #        pch = c(16, 25), pt.cex = 1,
    #        bty = "n", ncol = 1, cex = 1, bg = "white")
    
  } else if (i == 2){
    mtext(3, text = "Up to 10 matches", line = 0.17)
  } else if (i == 3) {
    mtext(3, text = "Up to 5 matches", line = 0.17)
  } else if (i == 4) {
    mtext(3, text = "Up to 10 matches", line = 0.17)
  }
  
  if (i == 1 | i == 6){
    axis(side = 2, at = seq(-.3, .3, by = .05), 
         labels = c("-.3", "-.25", "-.2", "-.15", "-.1", "-.05", "0", 
                    ".05", ".1", ".15", ".2", ".25", ".3"),
         cex.axis = .8
         # seq(-.3, .3, by = .05))
    )
    # title(ylab = "Estimated Effects", line = 2.1)
    mtext("", side = 2, line = 2, outer=FALSE,
          cex = 0.8)
    # axis(2, at=i,labels=-10:15, col.axis="red", las=2)
    
  }
  axis(side = 1, at = 0:4, labels = 0:4)
  
  
}
mtext("Mahalanobis Matching", line = 0, at = 0.21, outer = TRUE, cex = 1.2)
mtext("Propensity Score Matching", line = 0, at = 0.61, outer = TRUE, cex = 1.2)
mtext("Propensity Score \n Weighting", line = -1.8, at = 0.9, outer = TRUE, cex = 1.2)
mtext("Estimated Effect of \n Democratization", side = 2, line = .3, at = 0.73, 
      outer = TRUE, cex = 1.2)
mtext("Estimated Effect of \n Authoritarian Reversal", side = 2, line = .3, at = 0.22, 
      outer = TRUE, cex = 1.2)
mtext(1, text = "Years relative to the administration of treatment", line = 1.2,
      at = 0.52, outer = TRUE, cex = 1.2)

dev.off()
