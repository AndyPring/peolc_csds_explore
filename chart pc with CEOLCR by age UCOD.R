library(tidyverse)
library(afcharts)

data <- read.csv("input/summary_eol_ref_pc_mod.csv") %>%
  arrange(char, str_detect(value,"Other"), value)


chart <- ggplot(
    filter(data, !char=="All") %>%
      mutate(value = factor(value, levels = .$value)), 
    aes(y=value, x = eol_ref_pc, fill = char)) +
  geom_vline(aes(xintercept = eol_ref_pc, colour = "England"), 
             filter(data, char=="All"),
             show.legend = TRUE) +
  geom_col(colour = NA) +
  scale_y_discrete(limits=rev) +
  theme_af(base_size = 20) +
  labs(y=NULL, 
       x = "Percentage",
       fill = NULL,
       colour = NULL,
       # title = "Percentage of people with a community EOLC referral\n(deaths in 2024)"
       )

chart