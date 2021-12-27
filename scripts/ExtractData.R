library(pacman)
p_load(tidyverse, janitor, rvest, jsonlite)

url_json <-c("https://pawpatrol.fandom.com/api.php?action=parse&page=Pups_Make_a_Splash&prop=wikitext&format=json")

episode_data <- url_json %>%
  fromJSON() %>% 
  # read_html(url_json) %>%
  as_tibble() %>%
  filter(str_detect(parse, "list")) %>% 
  mutate(title = str_extract(parse, ""title\:(")"),
         season = str_trim(str_extract(parse, "\\h\\d"), side = "left"),
         episode = str_remove(str_extract(parse, "number=\\d{1,2}(A|B)"), "number=")
         ) %>%
  print()

url_json %>% print()




