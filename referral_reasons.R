library(tidyverse)
library(afcharts)

xl_data <- "./input/csds_ref_reasons.xlsx"
ref_reasons <- map(readxl::excel_sheets(xl_data) %>% set_names(), function(x) {
  readxl::read_xlsx(path = xl_data, sheet=x) %>%
    mutate(referrals = as.integer(referrals))})

ref_reasons$`Referral reasons 2024 LYOL` %>%
  mutate(
    pc = 100 * referrals / sum(referrals, na.rm = TRUE))


ref_reasons$`Referral reasons period` %>% 
  group_by(period) %>%
  mutate(
    total = sum(referrals),
    pc = 100 * referrals / sum(referrals, na.rm = TRUE),
    period = paste0(period,"\n(N=", total,")")) %>%
  arrange(period,referrals) %>%
  mutate(cumsum = cumsum(pc)) %>% 
  ggplot(aes(x=period, y=pc, fill=reason)) +
    geom_col()
  
           
