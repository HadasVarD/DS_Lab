##### Libraries ----
library(tidyverse)
library(readxl)
library(tidytuesdayR)
library(maps)
library(lubridate)
library(jpeg)
library(ggimage)
library(showtext)
library(patchwork)


##### Loading data ----
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")
usa <- map_data("state")
state_codes <- read_csv("ufo/state_code.csv") %>%
  select(state, code) %>%
  mutate(state = tolower(state), code = tolower(code))
uspop <- read_excel("ufo/uspop.xlsx",col_names = c("region", "pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014")) %>%
  mutate(region = tolower(str_remove(region, "."))) %>%
  rowwise() %>%
  mutate(mean_pop = mean(c(pop_2010, pop_2011, pop_2012, pop_2013, pop_2014))) %>%
  ungroup() %>%
  select(region, mean_pop)


##### Functions ----
convert_to_date <- function(x) {
  sub_string <- str_sub(x, 1, 10)
  d <- mdy(sub_string)
  return(as.numeric(d))
}
floor_decade <- function(x){return(lubridate::year(x) - lubridate::year(x) %% 10)}


##### Cleaning data ----
ufo_sightings <- ufo_sightings %>%
  mutate(date = as_date(purrr::map_dbl(date_time, ~convert_to_date(.)))) # Convert to 'Date' format. Run only once, its slow af


# Some globals for ease
nightsky_img <- "ufo/nightsky2.jpg"
#font_files() %>% tibble() %>% filter(str_detect(family, "Showcard Gothic"))
font_add(family = "Showcard Gothic", regular = "SHOWG.TTF")
font_add(family = "Chiller", regular = "CHILLER.TTF")
showtext_auto()

ufo <- ufo_sightings %>%
  filter(country == "us") %>% # Leaving only sightings in US
  filter(!(state %in% c("ak", "pr", "hi"))) %>% # Only mainland US
  select(date, code = state, description, encounter_length, latitude, longitude) %>%
  left_join(state_codes, by = "code") %>%
  mutate(decade = as.factor(purrr::map_dbl(date, ~floor_decade(.)))) %>% # Create decade variable
  drop_na(decade)


##### Cases by state in 2000-2010 ----
## Data preprocessing
by_state <- ufo %>%
    group_by(state, decade, .drop = F) %>%
    summarise(cases = n(),
              .groups = "drop")
  
by_state2 <- left_join(usa, by_state, by = c("region" = "state"), multiple = "all") %>%
  filter(decade %in% c(2000, 2010)) %>%
    left_join(uspop, by = "region")
  
cases_per_capita <- by_state2 %>%
  group_by(region) %>%
  summarise(cases = sum(cases), .groups = "drop") %>%
  left_join(uspop, by = "region") %>%
  mutate(cases_per_capita = cases/mean_pop)
  

by_state2 <- left_join(by_state2, select(cases_per_capita, region, cases_per_capita), by = "region")

# Actually plotting
plot <- ggplot(by_state2, aes(x = long, y = lat, fill = cases_per_capita, group = group)) +
  geom_polygon(color = "#00670c", show.legend = T) +
  scale_fill_gradient(low = "black", high = "#5dff00", limits = c(0, 0.2), breaks = seq(0, 0.2, length.out = 6), guide = guide_colorbar("Number of reported cases per capita", 
                                                                               title.position = "top",
                                                                               title.theme = element_text(color = "#5dff00", family = "serif"),
                                                                               title.hjust = 0.5,
                                                                               barwidth = 30,
                                                                               ticks.colour = NA)) +
  labs(title = "15 years of UFO sightings in the US between 2000 and 2014",
       caption = "Tomer Zipori | #TidyTuesday | Source: National UFO Reporting Center") +
  coord_fixed(1.3, clip = "off") +
  theme_minimal() +
  annotate("label", x = -130, y = 45.4, label = "Washington is spooky!\n # of cases: 1,228,975\n Cases per capita: 0.18",
           color = "#5dff00", fill = "black", family = "serif", fontface = "bold") +
  geom_curve(aes(x = -127.5, y = 46.4, xend = -124.48, yend = 47.4), color = "#5dff00", linewidth = 1, curvature = -0.35,
             arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("label", x = -124, y = 30.4, label = "Utah has the lowest rate in the US\n # of cases: 22,715\n Cases per capita: 0.0079",
           color = "#5dff00", fill = "black", family = "serif", fontface = "bold") +
  geom_curve(aes(x = -119.4, y = 31.4, xend = -111.5, yend = 39), color = "#5dff00", linewidth = 1, curvature = 0.3,
             arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  theme(plot.title = element_text(size = 24, vjust = -4, hjust = 0.5, color = "#5dff00", family = "Showcard Gothic"),
        plot.caption = element_text(color = "#5dff00", hjust = 1.05, family = "serif", size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(color = "#5dff00", family = "mono", size = 14))
plot <- ggbackground(plot, nightsky_img)

plot


##### Saving ----
ggsave(filename = "ufo.png", plot = plot, path = "plots/", height = 1009, width = 1920, dpi = 96, units = "px")

