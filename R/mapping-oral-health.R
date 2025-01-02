source("R/config.R")
library(dplyr)
library(BSol.mapR)

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

# Make constituency names compatible with BSol.mapR
oh_data <- oh_data_raw %>%
  mutate(
    Constituency = gsub(
      "Birmingham, ", "",
      `Parliamentary.Constituency.Name`
    )
  ) %>%
  rename(Ward = `Ward.Name`)

######################################################################## 
######             Plot number surveyed in each ward              ######
########################################################################

# Count records in each ward
ward_counts <- oh_data %>% 
  count(Ward)

map <- plot_map(
  ward_counts,
  value_header = "n",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Number of Children Surveyed",
  fill_missing = 0,
  style = "cont"
         )
map
save_map(map, "output/maps/children-surveyed-ward.png")

######################################################################## 
######             Plot number surveyed in each ward              ######
########################################################################

# Count records in each ward
const_counts <- oh_data %>% 
  count(Constituency)

map <- plot_map(
  const_counts,
  value_header = "n",
  map_type = "Constituency",
  area_name = "Birmingham",
  map_title = "Number of Children Surveyed",
  fill_missing = 0,
  style = "cont"
)
map

save_map(map, "output/maps/children-surveyed-const.png")

######################################################################## 
######         Plot outcome percentages by Constituency           ######
########################################################################

outcomes = list(
  "Plaque" = "% with Plaque\n(<1/3 Labial Surfaces)",
  "Enamel_Caries" = "% with 1 or more\nEnamel Caries",
  "Incisor_Caries" ="% with Incisor\nCaries",
  "PUFA_signs" = "% with 1 or more\nPUFA signs",
  "any_dental_issue" = "% with 1 or more\ndental issue"
)

const_outcomes <- oh_data %>%
  group_by(Constituency) %>%
  summarise(
    Plaque = 100 * mean(Plaque == "True"),
    Enamel_Caries = 100 * mean(Enamel_Caries == "True"),
    Incisor_Caries = 100 * mean(Incisor_Caries == "True"),
    PUFA_signs = 100 * mean(PUFA_signs == "True"),
    any_dental_issue = 100 * mean(any_dental_issue == "True"),
  )

for (outcome_i in names(outcomes)) {
  map_i <- plot_map(
    const_outcomes,
    value_header = outcome_i,
    map_type = "Constituency",
    area_name = "Birmingham",
    map_title = outcomes[[outcome_i]],
    style = "cont"
  )
  save_name <- file.path(
    "output",
    "maps",
    paste0(outcome_i, ".png")
  )
  save_map(map_i, save_name)
}