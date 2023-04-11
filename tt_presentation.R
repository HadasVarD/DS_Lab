##### Libraries ----
# install.packages("tidytuesdayR")
# install.packages("tidyverse")
library(tidytuesdayR)
library(tidyverse)

##### Data Loading
eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

