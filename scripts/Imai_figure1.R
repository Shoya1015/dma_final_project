rm(list=ls())

pacman::p_load(
  PanelMatch,
  tidyverse,
  gridExtra
)

##### Making Figure 1, the Treatment Variation Plot #####

load("data/raw/Acemoglu.RData")

###### Making Figure 1: the Treatment Variation Plot ######

ADis <- DisplayTreatment(unit.id = "wbcode2",
                         time.id = "year", 
                         xlab = "from 1960 to 2010", ylab = "Countries", legend.position = "bottom",
                         legend.labels = c("Autocracy (Control)", 
                                           "Democracy (Treatment)"),
                         treatment = "dem", data = d2) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_blank()) + 
  scale_y_discrete(breaks = c(1960, 1970, 1980, 1990, 2000, 2010))


ggsave(file = "output/imai_figure_1.pdf", 
       height = 8, 
       width = 14,
       units = "cm",
       ADis)



