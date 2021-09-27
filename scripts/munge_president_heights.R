library(readr)
library(dplyr)
library(here)

raw <- read_delim(here("data/president_heights_raw.tsv"), 
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE)
president_heights <-  raw %>% 
  select(number = `#`, name = President, height_cm = `Height (cm)`) %>% 
  mutate(height_cm = as.numeric(str_extract(height_cm, "\\d+")))


president_heights %>% 
  mutate(number = str_split(number, " & ")) %>% 
  unnest(number) %>% 
  mutate_at("number", as.integer) %>% 
  write_csv(here("data/president_heights.csv"))
