#### Preamble ####
# Purpose: Cleans the UN World Population Prospects 2024 demographic indicators 
#         and filters country-level data for 2023, keeping:
#          - Birth rate 
#          - Life expectancy 
#          - Mortality rate 
# Pre-requisites: WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.xlsx available locally

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(readxl)
library(arrow)

#### Load the data ####
raw_data <- read_excel(
  path  = "submissions/Data/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.xlsx",
  sheet = "Estimates",
  skip  = 16
)

#### Prepare cleaned base table ####
wpp_clean <-
  raw_data |>
  janitor::clean_names() |>
  rename(
    country       = region_subregion_country_or_area,
    location_code = location_code,
    parent_code   = parent_code
  )

# lookup table for hierarchy
lookup_tbl <-
  wpp_clean |>
  distinct(location_code, parent_code, country)

#### helper: get ancestor names ####
get_ancestors <- function(code, lookup_tbl) {
  out <- character(0)
  current <- code
  
  while (!is.na(current) && current != 0) {
    idx <- match(current, lookup_tbl$location_code)
    if (is.na(idx)) break
    
    parent <- lookup_tbl$parent_code[idx]
    if (is.na(parent) || parent == 0) break
    
    parent_name <- lookup_tbl$country[match(parent, lookup_tbl$location_code)]
    if (!is.na(parent_name)) out <- c(out, parent_name)
    
    current <- parent
  }
  
  unique(out)
}

#### Clean + create dev_group ####
cleaned_data <-
  wpp_clean |>
  filter(type == "Country/Area", year == 2023) |>
  mutate(
    birth_rate = as.numeric(crude_birth_rate_births_per_1_000_population),
    life_exp   = as.numeric(life_expectancy_at_birth_both_sexes_years),
    mortality  = as.numeric(crude_death_rate_deaths_per_1_000_population),
    ancestors  = map(location_code, ~ get_ancestors(.x, lookup_tbl)),
    dev_group  = case_when(
      country == "Japan" ~ "More developed",
      map_lgl(
        ancestors,
        ~ any(.x %in% c(
          "Europe",
          "Northern America",
          "Australia and New Zealand"
        ))
      ) ~ "More developed",
      TRUE ~ "Less developed"
    )
  ) |>
  select(
    country,
    year,
    birth_rate,
    life_exp,
    mortality,
    dev_group
  ) |>
  drop_na()

#### Save data ####
write_csv(cleaned_data, "submissions/Data/wpp_country_indicators_2023.csv")

write_parquet(cleaned_data,
              "submissions/Data/wpp_country_indicators_2023.parquet")
