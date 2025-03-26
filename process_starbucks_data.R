# code from week 11 lab

library(tidyverse)
library(plotly)
library(widgetframe)
library(tidytext)

### load Starbucks and state-level data ###
sb_locs <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/starbucks/starbucks-locations.csv")
sb_nutr <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/starbucks/starbucks-menu-nutrition.csv")
usa_pop <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/starbucks/us_state_pop.csv")
usa_states <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/starbucks/states.csv")


### Merge data ###
sb_usa <- sb_locs |> filter(Country=="US")

sb_locs_state <- sb_usa |>
  group_by(`State/Province`) |>
  rename(Abbreviation = `State/Province`) |>  # Rename to match state data
  summarize(n_stores = n())


# need state abbreviations
usa_pop_abbr <- usa_pop |>
  left_join(usa_states, by = c("state" = "State"))

sb_locs_state <- sb_locs_state |>
  left_join(usa_pop_abbr, by = "Abbreviation")



### Get topwords from menu items ###

topwords <- sb_nutr |>
  unnest_tokens(word, Item, token="words") |>
  group_by(word) |>
  summarise(word_frequency = n()) |>
  arrange(across(word_frequency, desc)) |>
  head(10)

sb_nutr_top10 <<- sb_nutr |>
  filter(str_detect(tolower(Item), paste(topwords, collapse = "|"))) |>
  mutate(top_word = str_extract(tolower(Item), paste(topwords, collapse = "|")))
