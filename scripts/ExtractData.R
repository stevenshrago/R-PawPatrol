# Load packages ----

library(pacman)
p_load(tidyverse, janitor, rvest, jsonlite, regexplain)

## Basic string extract function ----

extract_info <- function(pattern){
  
  str_extract(data$value, pattern) %>% 
    as_tibble() %>% 
    drop_na() %>%
    pull(value)
  
}

strip_html <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# Download all episodes ----

all_eps <- c("https://pawpatrol.fandom.com/api.php?action=parse&page=List_of_episodes&prop=wikitext&format=json")

eps <- all_eps %>% 
  fromJSON() %>%
  unlist() %>%
  as_tibble() %>% 
  mutate(long = str_split(value, pattern = "\\n")) %>% 
  select(long) %>% 
  unlist() %>% 
  as_tibble() %>% 
  filter(str_detect(value, "^#")) %>% 
  mutate(long = str_split(value, "\\/")) %>% 
  select(long) %>% 
  unlist() %>%
  as_tibble() %>% 
  mutate(value = str_replace_all(value, "(\\[|#|]])", ""),
         value = str_trim(value),
         value = str_replace_all(value, "\\s", "_")) %>% 
  pull(value)
 

full_data <- tibble(title = character(),
                    season = character(),
                    episode = character(),
                    overall = character(),
                    date = character(),
                    writers = list(),
                    director = character(),
                    title_card = list(),
                    first = list(),
                    backup = list(),
                    caller = list(),
                    needs = list(),
                    characters = list()
                    )

for (i in eps) {
  
  url <- paste0("https://pawpatrol.fandom.com/api.php?action=parse&page=",i,"&prop=wikitext&format=json")
  
  data <- url %>%
    fromJSON() %>%
    unlist() %>%
    as_tibble() %>% 
    mutate(long = str_split(value, pattern = "\\n")) %>% 
    select(long) %>% 
    unlist() %>% 
    as_tibble()
  
  ep_title <- data$value[[1]]
  ep_season <- extract_info("(?<=season=\\[\\[Season )\\d")
  ep_episode <- extract_info("(?<=\\|number=)\\d{1,2}( |A|B)")
  ep_overall <- extract_info("(?<=\\|overall=)\\d{1,3}")
  ep_aired <- str_replace_all(str_extract(data$value, "(?<=\\|airdate=).*"), "<.*?>", "") %>% 
    str_replace_all(., "\\[\\[", "") %>%
    str_extract(., "(\\w{4,9})\\s(\\d{1,2})[,]\\s20(1|2)[0-9]") %>%
    as_tibble() %>% 
    drop_na() %>% 
    pull(value)
  ep_writers <- extract_info("(?<=Written by\\s).+(?=])") 
  ep_director <- extract_info("(?<=director=\\[\\[).+(?=]])")
  ep_title_card <- extract_info("(?<=:).+(?=\\sis on the title card)")
  ep_first_responder <- extract_info("(?<=:).+(?=\\sis a first responder)")
  ep_backup_responder <- extract_info("(?<=:).+(?=\\sis a backup responder)")
  ep_pawpatrol_caller <- extract_info("(?<=:).+(?=\\scalls the PAW Patrol)")
  ep_needs_rescuing <- extract_info("(?<=:).+(?=\\sneeds rescuing)")
  ep_characters <- extract_info("(?<=\\*\\[\\[).+(?=]])")
  
  loop <- tibble(title = ep_title,
                 season = ep_season,
                 episode = ep_episode,
                 overall = ep_overall,
                 date = ep_aired,
                 writers = list(ep_writers),
                 director = ep_director,
                 title_card = list(ep_title_card),
                 first = list(ep_first_responder),
                 backup = list(ep_backup_responder),
                 caller = list(ep_pawpatrol_caller),
                 needs = list(ep_needs_rescuing),
                 characters = list(ep_characters))
  
  full_data <- full_data %>% bind_rows(loop)
  
  
}

saveRDS(full_data, "data_out/paw_patrol_db.rds")
