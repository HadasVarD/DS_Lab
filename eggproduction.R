##### Libraries ----
# install.packages("tidytuesdayR")
# install.packages("tidyverse")
library(tidytuesdayR)
library(tidyverse)
library(ggstream)
library(lubridate)
library(ggthemes)
library(showtext)
library(survival)

##### Data Loading ----
eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')


##### Cleaning ----
egg_clean <- eggproduction %>%
  filter(prod_type != "hatching eggs" & prod_process == "all") %>% # Leave only eggs meant for eating and general data
  select(-source, -prod_type, -prod_process, n_hens_all = n_hens, n_eggs_all = n_eggs) # Irrelevant columns

egg_clean2 <- cagefreepercentages %>%
  drop_na(percent_eggs) %>%
  select(-source, -percent_eggs, cagefree_percent_hens = percent_hens) %>% # Irrelevant\to many NA's columns + renaming
  inner_join(egg_clean, by = "observed_month", multiple = "all") %>%
  mutate(Cagefree = (cagefree_percent_hens * n_hens_all) / 100) %>% # Number of cage-free hens
  mutate(Traditional = n_hens_all - Cagefree) %>% # Number of traditional housing hens
  select(observed_month, Cagefree, Traditional) %>%
  pivot_longer(cols = c("Cagefree", "Traditional"), names_to = "housing", values_to = "n_hens")


##### Loading fonts ----
font_add(family = "Stencil", regular = "STENCIL.TTF")
showtext_auto()

##### Plotting ----
#### Defining dates to show in plot
dates_for_plot <- seq.Date(egg_clean2$observed_month[1], egg_clean2$observed_month[nrow(egg_clean2)], length.out = 6)
dates_for_plot[2] <- as.Date("2017-07-31")
dates_for_plot[3] <- as.Date("2018-05-31")
dates_for_plot[4] <- as.Date("2019-04-30")
dates_for_plot[5] <- as.Date("2020-03-31")

# Creating a subset of data to plot only certain points
subset_for_points <- egg_clean2 %>%
  filter((housing == "Cagefree" & observed_month %in% dates_for_plot) |
           (housing == "Traditional" & (observed_month == dates_for_plot[1] | observed_month == dates_for_plot[6]))) %>%
  inner_join(select(egg_clean, observed_month, n_hens = n_hens_all), by = "observed_month") %>%
  mutate(n_hens = case_when(housing == "Cagefree" ~ n_hens.x,
                            housing == "Traditional" ~ n_hens.y)) %>%
  select(-n_hens.x, -n_hens.y)

# Actually plotting
plot <- egg_clean2 %>%
  ggplot(aes(x = observed_month, y = n_hens/1000000, fill = factor(housing, levels = c("Traditional", "Cagefree")),
             color = factor(housing, levels = c("Traditional", "Cagefree")),
             label = n_hens/1000000)) +
  geom_density(position = 'stack', stat = 'identity') +
  geom_point(data = subset_for_points) +
  geom_text(data = subset_for_points, aes(label = round(n_hens/1000000)), hjust = 0.5, vjust = -1, size = 6, family = "serif") +
  scale_fill_manual(values = c("#f8f8fa", "#e1bf92")) +
  scale_color_manual(values = c("#898da5", "#b48811")) +
  annotate(geom = "text", x = as_date("2019/1/1"), y = 30, label = "Cage-Free", color = "#808080",
           family = "Stencil", angle = 2.5, size = 15, alpha = 0.5) +
  annotate(geom = "text", x = as_date("2019/1/1"), y = 190, label = "Traditional", color = "#808080",
           family = "Stencil", angle = 2.5, size = 15, alpha = 0.5) +
  scale_x_date(breaks = dates_for_plot) +
  xlab("") +
  ylab("Number of hens (millions)") +
  labs(fill = "Housing", title = "Number of Cage-free hens in the US is constatly rising",
       subtitle = "Relative number of Cage-free hens in the US in the years 2016-2021") +
  guides(color = "none", fill = "none") +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 13, family = "serif"))
plot

# Saving
ggsave("eggs.png", plot, path = "plots/", height = 1003, width = 1600, units = "px", dpi = 96)

