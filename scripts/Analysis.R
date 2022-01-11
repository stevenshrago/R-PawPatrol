library(pacman)

p_load(tidyverse, janitor, glue, extrafont, lubridate)

data_raw <- readRDS("data_out/paw_patrol_db.rds")


data_raw

#Pups appearances

pups <- c("Zuma", "Skye", "Rubble", "Rocky", "Chase", "Marshall", "Ryder", "Robo-Dog", "Everest", "Tracker", "Tuck", "Ella", "Rex", "Liberty")

(
  appearances <- data_raw %>%
    select(season, episode, overall, characters) %>%
    unnest(characters) %>%
    # filter(characters %in% pups) %>%
    count(characters) %>%
    # group_by(season) %>%
    slice_max(n, n = 30) %>%
    mutate(characters = factor(characters)) %>%
    ggplot(aes(
      x = reorder(characters, n), y = n
    )) +
    geom_col() +
    geom_text(aes(label = n, hjust = -0.5)) +
    coord_flip()
)

appearances %>% tabyl(characters)

cum_appearances <- data_raw %>% 
  select(overall, characters) %>%
  unnest(characters) %>%
  filter(characters %in% pups) %>%
  group_by(characters) %>% 
  mutate(appearance = 1,
         cumsum = cumsum(appearance)) %>% 
  select(overall, characters, cumsum) %>% 
  print()


ggplot(cum_appearances, aes(x = overall, y = cumsum, group = characters)) +
  geom_line(aes(color = factor(characters)))


  
dff <-
  tibble(characters = rep(
    c(
      "Zuma",
      "Skye",
      "Rubble",
      "Rocky",
      "Chase",
      "Marshall",
      "Ryder",
      "Robo-dog",
      "Everest",
      "Tracker",
      "Tuck",
      "Ella",
      "Rex",
      "Liberty"
    ),
    times = 369
  )) %>%
  print()

df <- tibble(overall = rep(1:369, times = 14)) %>%
  arrange(overall) %>%
  bind_cols(dff) %>%
  left_join(cum_appearances %>%
              mutate(overall = as.numeric(overall)),
            by = c("overall", "characters")) %>%
  mutate(cumsum = if_else(overall == 1 & is.na(cumsum), 0, cumsum)) %>%
  group_by(characters) %>% 
  fill(cumsum, .direction = "downup") %>%
 view()


appearances <- data_raw %>%
  select(season, episode, overall, characters) %>%
  unnest(characters)