library(tidyverse)
library(survival)
sdata_sp <- sdataf %>%
  group_by(PERSON_ID_DEID) %>%
  group_modify(~spfunction(.x)) 