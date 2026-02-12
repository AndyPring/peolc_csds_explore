library(tidyverse)
library(afcharts)

contact_trend <- readxl::read_xlsx("./input/contacts.xlsx", sheet=1)

contact_trend %>%
  filter(between(year, 2018, 2024)) %>%
  ggplot(aes(y=as.integer(contacts),x=year)) +
  geom_line() +
  geom_point(size=5) +
  expand_limits(y=0)+
  labs(
    x="Year",
    y="Number of\ncontacts") +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks = 2018:2024) +
  theme_af(
    axis="x", 
    ticks="none") +
  theme(axis.line = element_blank())
