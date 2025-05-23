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
      "processed-oral-health-data-2024.csv"
    )
  ) 
}

# Make constituency names compatible with BSol.mapR
oh_data <- oh_data_raw %>%
  mutate(
    Constituency24 = gsub(
      "Birmingham, ", "",
      `Parliamentary.Constituency.Name`
    ), 
    Constituency24 = stringr::str_replace(
      Constituency24, 
      "Birmingham ", 
      "")
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
save_map(map, "output/maps/2024/children-surveyed-ward.png")

######################################################################## 
######             Plot number surveyed in each ward              ######
########################################################################

# Count records in each ward
const_counts <- oh_data %>%
  count(Constituency24)

map <- plot_map(
  const_counts,
  value_header = "n",
  map_type = "Constituency24",
  area_name = "Birmingham",
  map_title = "Number of Children Surveyed",
  fill_missing = 0,
  style = "cont"
)
map

save_map(map, "output/maps/2024/children-surveyed-const.png")

######################################################################## 
######         Plot outcome percentages by Constituency           ######
########################################################################

const_outcomes <- oh_data %>%
  group_by(Constituency24) %>%
  summarise(
    num_children = n(),
    Plaque = 100 * mean(Plaque == "True", na.rm = T),
    Enamel_Caries = 100 * mean(Enamel_Caries == "True", na.rm = T),
    Incisor_Caries = 100 * mean(Incisor_Caries == "True", na.rm = T),
    PUFA_signs = 100 * mean(PUFA_signs == "True", na.rm = T),
    Decayed_teeth = 100 * mean(Decayed_teeth == "True", na.rm = T),
    Missing_teeth = 100 * mean(Missing_teeth == "True", na.rm = T),
    Filled_teeth = 100 * mean(Filled_teeth == "True", na.rm = T),
    Missing_filled_decayed_teeth = 100 * mean(Missing_filled_decayed_teeth == "True", na.rm = T),
    Mean_DMTF_per_child = mean(total_dmtf, na.rm = T)
  )

outcomes = list(
  "Plaque" = "% with Plaque",
  "Enamel_Caries" = "% with 1 or more Enamel Caries",
  "Incisor_Caries" ="% with Incisor Caries",
  "PUFA_signs" = "% with 1 or more PUFA signs",
  "Decayed_teeth" = "% with 1 or more decayed teeth",
  "Missing_teeth" = "% with 1 or more missing teeth",
  "Filled_teeth" = "% with 1 or more filled teeth",
  "Missing_filled_decayed_teeth" =  "% with one or more decayed/missing/filled teeth",
  "Mean_DMTF_per_child" = "Mean DMFT per child"
)

# Suppress Edgbaston since only one child
#const_outcomes[names(outcomes)][const_outcomes$Constituency24 == "Edgbaston",] = NA

for (outcome_i in names(outcomes)) {
  map_i <- plot_map(
    const_outcomes,
    value_header = outcome_i,
    map_type = "Constituency24",
    area_name = "Birmingham",
    locality_lines = T,
    locality_names = T,
    map_title = outcomes[[outcome_i]],
    style = "cont"
  )
  
  save_name <- file.path(
    "output",
    "maps/2024/",
    paste0(outcome_i, ".png")
  )
  
  save_map(map_i, save_name)
}