pacman::p_load(
  tidyverse,
  haven
)

data_ipw <- read_dta("data/raw/impulse_ipw_alt.dta")

data_ipw <- data_ipw |> 
  separate(parm, 
           into = c("parm1", "parm2"), 
           sep = "c", 
           extra = "merge", 
           fill = "right")

data_ipw <- data_ipw |> 
  filter(parm2 != "") |> 
  mutate(parm2 = as.numeric(parm2))


data_ipw <- data_ipw |> 
  mutate(time = parm2 - 16)

figure_4 <- ggplot(data_ipw, aes(x = time, y = estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = min95, ymax = max95), fill = "skyblue", alpha = 0.3) +
  labs(x = "Years around democratization",
       y = "Change in GDP per capita (log points)") +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  theme_bw() 

ggsave("output/figure_4.pdf", 
       figure_4, 
       width = 14, 
       height = 8, 
       units = "cm")
