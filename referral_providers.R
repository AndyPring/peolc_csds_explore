library(tidyverse)
library(afcharts)

xl_data <- "./input/csds_ref_providers.xlsx"
ref_providers <- map(readxl::excel_sheets(xl_data) %>% set_names(), function(x) {readxl::read_xlsx(path = xl_data, sheet=x)})

ref_providers$`LYOL referrals 2024` %>%
  mutate(
    referrals = as.integer(referrals),
    pc = 100 * referrals / sum(referrals))

ref_providers$`Biggest independents` %>%
  mutate(
    referrals = as.integer(referrals))
