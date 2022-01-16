# Load packages ----

library(pacman)
p_load(tidyverse, janitor, extrafont, glue)

extrafont::font_import()

# Load data ----

db <- readRDS("data_out/paw_patrol_db.rds")

glimpse(db)

char_appear <- db %>% 
  select(overall, characters) %>% 
  unnest(cols = c(characters)) %>% 
  count(characters) %>% 
  arrange(desc(n)) %>% 
  mutate(characters = factor(characters)) %>% 
  slice_head(n=30) %>% 
  ggplot(aes(x = reorder(characters, n), y = n)) +
  geom_col() +
  coord_flip()


pups <- c("Chase", "Rocky", "Zuma", "Skye", "Marshall", "Rubble", "Everest", "Tracker")


(
  cumsum_char <- db %>%
    select(season, overall, characters) %>%
    unnest(cols = c(characters)) %>%
    filter(characters %in% pups) %>%
    mutate(appear = 1,
           overall = factor(overall)) %>%
    group_by(characters, overall) %>%
    mutate(cumsum = cumsum(appear)) %>%
    ungroup() %>% 
    ggplot(aes(overall, cumsum)) +
    geom_line()
)