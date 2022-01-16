# Load packages ----
library(pacman)
p_load(tidyverse, janitor, glue, extrafont, lubridate, scales)

# Load & clean data ----
data_raw <- readRDS("data_out/paw_patrol_db.rds")

data <- data_raw %>%
  unnest(characters) %>% 
  mutate(overall = as.numeric(overall),
         characters = str_replace_all(characters, "Precious['] owner", "Katie"),
         characters = str_replace_all(characters, "^Cat\\s.*", "Kitten Catastrophe Crew"),
         season = as.numeric(season))

pup_colours <- c("Zuma" = "darkorange",
                 "Chase" = "blue",
                 "Everest" = "deepskyblue2",
                 "Marshall" = "red2",
                 "Rex" = "darkgreen",
                 "Robo-Dog" = "azure2",
                 "Rocky" = "limegreen",
                 "Rubble" = "gold",
                 "Ryder" = "grey15",
                 "Skye" = "hotpink",
                 "Tracker" = "chocolate4")

pups <- c("Zuma", "Skye", "Rubble", "Rocky", "Chase", "Marshall", "Ryder", "Robo-Dog", "Everest", "Tracker", "Rex")

##Top 30 Appearances ----

#How many times each pup has appeared in the show, cumulatively. 

data %>%
  select(season, episode, overall, characters) %>%
  unnest(characters) %>%
  # filter(characters %in% pups) %>%
  count(characters) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 30) %>%
  mutate(characters = factor(characters)) %>% 
  ggplot(aes(x = reorder(characters, n), y = n)) +
  geom_col(aes(fill = characters)) +
  geom_text(aes(label = n, hjust = -0.5), size = 3) +
  coord_flip() +
  theme_minimal() +
  expand_limits(y = c(0,350)) +
  labs(title = "Number of appearances per character",
       subtitle = "Top 30 characters by appearances",
       x = "Characters",
       y = "Appearances") +
  scale_fill_manual(values = pup_colours)

## Cumulative appearances (pups only) ----

dff <- tibble(characters = rep(c("Zuma", "Skye", "Rubble", "Rocky", "Chase", "Marshall", "Ryder", "Robo-Dog", "Everest", "Tracker", "Rex"), times = 369))

tibble(overall = rep(1:369, times = 11)) %>%
  arrange(overall) %>%
  bind_cols(dff) %>%
  left_join(data %>% 
              select(overall, characters) %>%
              filter(characters %in% pups) %>%
              group_by(characters) %>% 
              mutate(appearance = 1,
                     cumsum = cumsum(appearance)) %>% 
              select(overall, characters, cumsum), 
            by = c("overall", "characters")) %>%
  mutate(cumsum = if_else(overall == 1 & is.na(cumsum), 0, cumsum)) %>%
  group_by(characters) %>% 
  fill(cumsum, .direction = "downup") %>% 
  ggplot(aes(x = overall, y = cumsum, group = characters)) +
  geom_line(aes(color = factor(characters))) +
  theme_minimal() +
  scale_colour_manual(values = c("Zuma" = "darkorange",
                                 "Chase" = "blue",
                                 "Everest" = "deepskyblue2",
                                 "Marshall" = "red2",
                                 "Rex" = "darkgreen",
                                 "Robo-Dog" = "dimgrey",
                                 "Rocky" = "limegreen",
                                 "Rubble" = "gold",
                                 "Ryder" = "grey15",
                                 "Skye" = "hotpink",
                                 "Tracker" = "chocolate4")) +
  labs(title = "Cumulative appearances of the Paw Patrol",
       x = "Episodes",
       y = "Appearances (cumulative)")

## Pups appearances as first responders ----

data_raw %>%
  unnest(first) %>%
  mutate(season = as.numeric(season)) %>%
  select(season, episode, overall, first) %>%
  count(season, first) %>%
  left_join(
    data %>%
      filter(characters %in% pups) %>%
      count(season, characters) %>%
      rename(appearances = n,
             first = characters),
    by = c("season", "first")
  ) %>%
  mutate(appearances = if_else(appearances < n, n, appearances),
         perc_first = n/appearances) %>% 
  arrange(desc(perc_first)) %>% 
  ggplot(aes(x = season, y = perc_first, group = first)) +
  geom_line(aes(colour = first)) +
  scale_color_manual(values = pup_colours, breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8")) +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal() +
  labs(title = "Pups as first reponders per season",
       subtitle = "Nnmber of times as a first responder as a percentage of total appearances",
       x = "Season",
       y = "Percentage of total appearances as a first responder")

## Pups appearances as back-up responders ----

data_raw %>%
  unnest(backup) %>%
  mutate(season = as.numeric(season)) %>%
  select(season, episode, overall, backup) %>%
  count(season, backup) %>%
  left_join(
    data %>%
      filter(characters %in% pups) %>%
      count(season, characters) %>%
      rename(appearances = n,
             backup = characters),
    by = c("season", "backup")
  ) %>%
  mutate(appearances = if_else(appearances < n, n, appearances),
         perc_backup = n/appearances) %>% 
  arrange(desc(perc_backup)) %>% 
  ggplot(aes(x = season, y = perc_backup, group = backup)) +
  geom_line(aes(colour = backup)) +
  scale_color_manual(values = pup_colours, breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8")) +
  scale_y_continuous(labels = label_percent()) +
  theme_minimal() +
  labs(title = "Pups as backup reponders per season",
       subtitle = "Nnmber of times as a backup responder as a percentage of total appearances",
       x = "Season",
       y = "Percentage of total appearances as a backup responder")

## Episode titles

special_eps <- c("Ultimate Rescue",
                 "Dino Rescue",
                 "Mission PAW",
                 "Sea Patrol",
                 "Mighty Pups",
                 "Rescue Knights",
                 "Moto Pups")

## Special episodes per season ----

data_raw %>%
  select(title, season, episode, overall) %>% 
  mutate(episode_type = factor(case_when(str_detect(title, "^Ultimate") ~ "Ultimate Rescue",
                                  str_detect(title, "^Dino") ~ "Dino Rescue",
                                  str_detect(title, "^Mission") ~ "Mission PAW",
                                  str_detect(title, "^Sea") ~ "Sea Patrol",
                                  str_detect(title, "^Mighty") ~ "Mighty Pups",
                                  str_detect(title, "^Rescue") ~ "Rescue Knights",
                                  str_detect(title, "^Moto") ~ "Moto Pups",
                                  str_detect(title, "^Pups (S|s)top") ~ "Pups stop...",
                                  str_detect(title, "^Pups (G|g)et") ~ "Pups get...",
                                  str_detect(title, "^Pups and") ~"Pups and...",
                                  str_detect(title, "Pups (S|s)ave") ~ "Pups save...",
                                  str_detect(title, "^Pups vs") ~ "Pups versus...",
                                  TRUE ~ "Other")),
         special_episode = factor(if_else(episode_type %in% special_eps, "Special", "Regular"))
           ) %>%
  group_by(season) %>% 
  summarise(fct_count(special_episode, sort = TRUE, prop = TRUE)) %>%
  mutate(season = glue("Season {season}")) %>% 
  ggplot(aes(x = reorder(f, p), y = p)) +
  geom_col() +
  geom_text(aes(label = label_percent(accuracy = 1)(p), hjust = 1.1), colour = "white")+
  scale_y_continuous(label = label_percent()) +
  facet_wrap(~season, nrow = 4) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Special episodes per season",
       subtitle = "Regular vs special episodes percentage per season",
       x = "Episode type",
       y = "Percentage of episodes")

tabyl(data_raw, season)

