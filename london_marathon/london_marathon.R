##### Libraries ----
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(transformr)
library(hms)

##### Data ----
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)

winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon

##### data for labeling - peak times
winners_labels <- winners %>%
  filter(Category %in% c("Men", "Women")) %>%
  filter(Year %in% c(1981, 1985, 2003, 2019, 2022)) # start and end year + 2019 = men's record, 2003 = women's record


##### Plotting ----
plot1 <- winners %>%
  filter(Category %in% c("Men", "Women")) %>%
  ggplot(aes(x = Year, y = Time, group = Category, color = Category)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_text(aes(x = max(Year), label = Time)) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  geom_hline(yintercept = hms(0, 0, 2), linetype = "dashed", color = "red", show.legend = F) +
  transition_reveal(Year, keep_last = F) +
  view_follow(fixed_y = T)

plot1 <- animate(plot1, end_pause = 10)
plot1
