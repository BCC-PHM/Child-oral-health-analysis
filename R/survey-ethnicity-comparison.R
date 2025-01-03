# Compare survey ethnicities to census

source("R/config.R")
library(dplyr)
library(BSol.mapR)
library(ggplot2)

######################################################################## 
######                  Load and process data                     ######
########################################################################

# Only load vaccine data if it's not already loaded
if(!exists("oh_data_raw")) {
  oh_data_raw <- read.csv(
    file.path(
      data_path, 
      "processed-oral-health-data.csv"
    )
  ) 
}

census_data <- read.csv(
  "data/Birmingham-census-age-ethnicity.csv"
) %>%
  filter(
    Age..86.categories. %in% c("Aged 5 years", "Aged 6 years"),
    Ethnic.group..20.categories. != "Does not apply"
  ) %>%
  mutate(
    Ethnicity = stringr::str_split_i(Ethnic.group..20.categories., ": ", i=2),
    Ethnicity = case_when(
      Ethnicity == "English, Welsh, Scottish, Northern Irish or British" ~ "White British",
      Ethnicity == "African" ~ "Black African",
      Ethnicity == "Caribbean" ~ "Black Caribbean",
      Ethnicity == "Other Asian" ~ "Asian other",
      Ethnicity == "Other Black" ~ "Black other",
      Ethnicity == "Other Mixed or Multiple ethnic groups" ~ "Mixed other",
      Ethnicity == "White and Asian" ~ "White Asian",
      Ethnicity == "White and Black African" ~ "White Black African",      
      Ethnicity == "White and Black Caribbean" ~ "White Black Caribbean",    
      Ethnicity == "Other White" ~ "White other",
      Ethnicity == "Any other ethnic group" ~ "Any other",
      TRUE ~ Ethnicity
    ),
    N = Observation
  ) %>%
  group_by(
    Ethnicity
  ) %>%
  summarise(
    N = sum(Observation)
  )

eth_counts <- oh_data_raw %>%
  count(Lower.Ethnic.code) %>%
  mutate(
    Ethnicity = gsub(
      "\\w\\d - ", "", Lower.Ethnic.code
      ),
    Ethnicity = case_when(
      Ethnicity == "British" ~ "White British",
      TRUE ~ Ethnicity
    )
  ) %>%
  select(Ethnicity, n)

all_data <- eth_counts %>%
  full_join(
    census_data,
    by = join_by("Ethnicity")
  ) %>%
  mutate(
    n = tidyr::replace_na(n, 0),
    sample_perc = 100 * n / sum(n),
    pop_perc = 100 * N / sum(N),
    difference = sample_perc - pop_perc,
    group = case_when(
      difference > 0 ~ "Overrepresented",
      difference < 0 ~ "Underrepresented"
    )
  )
  