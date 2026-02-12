library(tidyverse)
library(afcharts)

ref_eoltrend <- readxl::read_xlsx("./input/csds_ref_reasons.xlsx", sheet = "Eol referral trend")

ref_eoltrend %>%
  mutate(referrals=as.integer(referrals)) %>%
  group_by(weeks) %>%
  summarise(
    eol=sum(referrals[eol_referral==1]),
    all = sum(referrals)
  ) %>%
  mutate(pc = 100*eol/all) %>%
  ggplot(aes(y=pc, x=weeks)) +
  geom_line() +
  geom_point(size=2) +
  scale_x_reverse() +
  theme_af()
  
  +

      expand_limits(y=0)+
  labs(
    x="Year",
    y="Number of\nreferrals") +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks = 2018:2024) +
  theme_af(
    axis="x", 
    ticks="none") +
  theme(axis.line = element_blank())

