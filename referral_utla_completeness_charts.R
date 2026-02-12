library(tidyverse)
library(afcharts)

data1 <- read.csv("input/referals_utla.csv") %>%
  set_names(c("utla", "dths", "refany_c", "refeol_c")) %>%
  mutate(
    across(everything(), as.integer),
    
    refany_pc = 100 * refany_c /dths,
    refeol_pc = 100 * refeol_c/dths,
    refonlyxeol_c = refany_c - refeol_c,
    refonlyxeol_pc = refany_pc - refeol_pc) %>%
  pivot_longer(
    starts_with("ref"),
    names_to = c("cohort", ".value"),
    names_prefix ="ref",
    names_sep="_")

utla_order <- data %>% filter(cohort == "eol")  %>% arrange(pc) %>% pull(utla)

data %>% 
  filter(!cohort == "any") %>% 
  mutate(
    utla = factor(utla, levels = utla_order),
    cohort = factor(cohort, 
                    levels = c("onlyxeol", "eol"),
                    labels = c("No", "Yes"))) %>%
  ggplot(
  aes(x = utla, y = pc,  fill = cohort)) +
  geom_col() +
  theme_af() +
  labs(
    x = "UTLA",
    y = "Percentage\nof deaths",
    fill = "Referrals\ninclude\nPEOLC") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  )

# ============================================================== #

data2 <- read.csv("input/referals_xact.csv") 

data2 %>%
  mutate( pc = 100 * people / sum(people),
          category = factor(
            category,
            levels = c("people with eol referral(s) but no activity",   
                       "people with eol referral(s) including activity",
                       "people with referral but none eol"),
            labels = c("EOL referral(s)\nincluding activity",
                       "EOL referral(s)\nbut no activity",   
                       "None EOL")
          )) %>%
  ggplot(aes(y = pc, x = category)) +
  geom_col() + 
  geom_text(aes(y=pc/2, label = round(pc))) +
  theme_af() +
  labs(y = "Percentage\nof people\nwith a LYOL\nCSDS referral")
