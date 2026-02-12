library(tidyverse)
library(afcharts)

contact_trend_week <- readxl::read_xlsx("./input/contacts.xlsx", sheet=2)

contact_trend_week <- contact_trend_week %>%
  mutate(contacts = as.integer(contacts),
         totdths = as.integer(totdths)) %>%
  filter(!week==53)

contact_trend_week %>%
  group_by(week)  %>%
  summarise(
    contacts = sum(contacts)) %>%
  ggplot(aes(y=contacts, x=week)) +
  geom_line() +
  geom_point(size=2) +
  scale_x_reverse() +
  theme_af()

contact_trend_week %>%
  filter(!is.na(eol_pod)) %>%
  ggplot(aes(y=contacts, x=week)) +
  geom_line() +
  geom_point(size=2) +
  scale_x_reverse() +
  theme_af() +
  facet_wrap(~eol_pod)

contact_trend_week %>%
  filter(!is.na(eol_pod)) %>%
  mutate(contacts_per_person = contacts/totdths) %>%
  ggplot(aes(y=contacts_per_person, x=week)) +
  geom_line() +
  geom_point(size=1) +
  scale_x_reverse() +
  theme_af() +
  facet_wrap(~eol_pod)



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

