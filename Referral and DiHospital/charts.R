
library(tidyverse)
library(afcharts)

data <- read.csv("./Referral and DiHospital/referrals_death_in hospital.csv", header=TRUE)

chart_data <- data %>%
  setNames(c(
    "age",
    "ucod_cat",
    "dths_all",
    "dths_ref",
    "dths_eol",
    "dih_all",
    "dih_ref",
    "dih_eol"
  )) %>% 
  mutate(dih_eol = as.integer(dih_eol)) %>% 
  pivot_longer(
    cols=c(starts_with("dths"),starts_with("dih")),
    names_sep = "_",
    names_to = c(".value", "stat")
  ) %>%
  group_by(age, ucod_cat) %>%
  mutate(
    dths_pc = 100 * dths/max(dths),
    dih_pc = 100 * dih / dths,
    stat=factor(
      stat, 
      levels = c( "ref", "eol", "all"), 
      labels = c( "Any CSDS referral", "Any CSDS EOL referral","Number of deaths"))) %>%
  ungroup()

chart_data2 <- data %>%
  setNames(c(
    "age",
    "ucod_cat",
    "dths_all",
    "dths_ref",
    "dths_eol",
    "dih_all",
    "dih_ref",
    "dih_eol"
  )) %>% 
  mutate(
    dths_eol,
    age,
    ucod_cat,
    dih_eol = as.integer(dih_eol),
    dths_xeol = dths_all - dths_eol,
    dih_xeol = dih_all - dih_eol,
    .keep = "none") %>% 
  pivot_longer(
    cols=c(starts_with("dths"),starts_with("dih")),
    names_sep = "_",
    names_to = c(".value", "stat")
  ) %>%
  group_by(age, ucod_cat) %>%
  mutate(
    dths_pc = 100 * dths/max(dths),
    dih_pc = 100 * dih / dths,
    stat=factor(
      stat, 
      levels = c( "xeol", "eol"), 
      labels = c( "No CSDS EOL referral", "Any CSDS EOL referral"))) %>%
  ungroup()
  
ref_chart <- 
  chart_data %>%
  filter(!stat=="Number of deaths") %>%
  mutate(stat=fct_drop(stat, c("Number of deaths"))) %>%
  ggplot() +
  geom_col(aes(x = age, fill = stat, y = dths_pc), position = "dodge") +
  geom_line(
    data=chart_data %>% 
      mutate(stat = as.character(stat)) %>% 
      filter(stat == "Number of deaths"),
    aes(
      x = age,
      y = 100 * dths / max(chart_data$dths),
      #colour=stat,
      group = TRUE),
    linewidth = 1 ,
    show.legend = FALSE) +
  scale_y_continuous(sec.axis=sec_axis(~./(100/max(chart_data$dths)), 
                                       name = "Number\nof deaths",
                                       breaks = c(0, 12500, 25000, 37500, 50000) )) +
  facet_wrap(~ucod_cat) +
  theme_af(base_size=11, grid = "y", axis = "none", legend = "bottom", ticks = "none") +
  labs( 
    title = "Percentage of deaths with a CSDS referral",
    x = "Age at death",
    y = "Percentage",
    fill = NULL) 

ggsave(file = "ref_chart.svg", plot = ref_chart, units = "cm"  )

dih_chart <- chart_data %>%
  mutate(stat=factor(
    stat, 
    levels = c( "Number of deaths", "Any CSDS referral", "Any CSDS EOL referral"),
    labels = c( "All deaths", "Any CSDS referral", "Any CSDS EOL referral"))) %>%
  ggplot(aes(x=age, fill=stat)) +
  geom_col(aes(y=dih_pc), position="dodge") +
  facet_wrap(~ucod_cat) +
  theme_af(legend = "bottom", ticks = "none", base_size=11) +
  labs( 
    title = "Percentage of deaths in hospital by CSDS referral status",
    x = "Age at death",
    y = "Percentage",
    fill = NULL) 


ggsave(file = "dih_chart.svg", plot = dih_chart, units = "cm"  )

dih_chart2 <- chart_data2 %>%
#  mutate(stat=factor(
#    stat, 
#    levels = c( "Number of deaths", "Any CSDS referral", "Any CSDS EOL referral"),
#    labels = c( "All deaths", "Any CSDS referral", "Any CSDS EOL referral"))) %>%
  ggplot(aes(x=age, fill=stat)) +
  geom_col(aes(y=dih_pc), position="dodge") +
  facet_wrap(~ucod_cat) +
  theme_af(legend = "bottom", ticks = "none", base_size=11) +
  labs( 
    title = "Percentage of deaths in hospital by CSDS referral status",
    x = "Age at death",
    y = "Percentage",
    fill = NULL) 


ggsave(file = "dih_chart2.svg", plot = dih_chart2, units = "cm"  )

  
