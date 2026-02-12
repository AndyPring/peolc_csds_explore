library(tidyverse)
library(afcharts)

ref_trend <- readxl::read_xlsx("./input/csds_ref_annual_trend.xlsx")

ref_trend %>%
  filter(!year==2025) %>%
  ggplot(aes(y=as.integer(referrals),x=year)) +
  geom_line() +
  geom_point(size=5) +
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
  
  