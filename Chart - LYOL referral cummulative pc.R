library(tidyverse)
library(afcharts)

data <- read.csv("./input/firstref_cumulativetotal2024.csv")

data2 <- data %>%
  group_by(ref) %>%
  arrange(ref, desc(days)) %>%
  mutate(
    cumsum = cumsum(n),
    cumsum_pc = 100 * cumsum / sum(n),
    c25 = rank(abs(cumsum_pc -25)),
    c50 = rank(abs(cumsum_pc -50)),
    c75 = rank(abs(cumsum_pc -75))
  )
  

data2 %>% ggplot(aes(x=days, y = cumsum_pc, colour = ref)) +
  geom_line(linewidth = 1.2) +
  geom_text(
    data = ~filter(.x, ref=="EOL" & (c25==1|c50==1|c75==1)),
    aes(label = paste(days, "days")),
    nudge_x = c(-45, -40, -30),
    show.legend = FALSE,
    colour = "black",
    size = 5) +
  geom_point(data = ~filter(.x, ref=="EOL" & (c25==1|c50==1|c75==1)),
             size = 3) +
  labs(
    y = "Cumulative\npercentage",
    x = "Days before end of life",
    colour = "Referral type") +
  scale_x_reverse() +
  theme_af(base_size = 20, grid="xy", axis = "y")

  