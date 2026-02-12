library(tidyverse)
library(afcharts)

data <- read.csv("input/spc_summary.csv") %>%
  mutate(
    pal =case_when(
      (eol_csds_referral & hospital_spc) ~ "Community \nand hospital",
      eol_csds_referral ~ "Community\nonly",
      hospital_spc ~ "Hospital\nonly",
      .default = "None"))

data %>%
  group_by(pal) %>%
  summarise(dths = sum(dths), .groups = "drop") %>%
  mutate(
    pc = 100 * dths/sum(dths),
    fill_lbl = str_replace(pal, "\n", " ")) %>%
  ggplot(aes(x=pal, y=pc, fill = fill_lbl)) +
  geom_col() +
  theme_af(base_size = 11, axis = "none", ticks = "none") +
  labs (
    y = "Percentage\nof all\ndeaths",
    x = NULL,
    fill = "Palliative care")

data %>%
  group_by(pod_hospital,pal ) %>%
  summarise(dths = sum(dths), .groups = "drop_last") %>%
  mutate(
    pc = 100 * dths/sum(dths),
    fill_lbl = str_replace(pal, "\n", " "),
    pod_hospital = factor(pod_hospital, levels = c(T, F), labels = c("Died in hospital", "Died elsewhere"))) %>%
  ggplot(aes(x=pal, y=pc, fill = fill_lbl)) +
  geom_col() +
  facet_wrap(~pod_hospital) +
  theme_af(base_size = 12
           , axis = "none", ticks = "none") +
  theme(panel.spacing.x = unit(2, "lines"),
        strip.text = element_text(size=24)) +
  labs (
    y = "Percentage\nof all\ndeaths",
    x = "Place of recorded palliative care",
    fill = "Place of palliative care")

# Not sure how this got here .... 
#          
# ref_eoltrend %>%
#   mutate(referrals=as.integer(referrals)) %>%
#   group_by(weeks) %>%
#   summarise(
#     eol=sum(referrals[eol_referral==1]),
#     all = sum(referrals)
#   ) %>%
#   mutate(pc = 100*eol/all) %>%
#   ggplot(aes(y=pc, x=weeks)) +
#   geom_line() +
#   geom_point(size=2) +
#   scale_x_reverse() +
#   theme_af()
# 
# +
#   
#   expand_limits(y=0)+
#   labs(
#     x="Year",
#     y="Number of\nreferrals") +
#   scale_y_continuous(labels=scales::comma) +
#   scale_x_continuous(breaks = 2018:2024) +
#   theme_af(
#     axis="x", 
#     ticks="none") +
#   theme(axis.line = element_blank())