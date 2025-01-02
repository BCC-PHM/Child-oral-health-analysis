source("R/config.R")
library(readxl)
library(dplyr)
library(BSol.mapR)

######################################################################## 
######                  Load and process data                     ######
########################################################################

# Only load vaccine data if it's not already loaded
if(!exists("oh_data_raw")) {
  oh_data_raw <- read_excel(
    file.path(
      data_path, 
      "Oral Health Epi Survey.xlsx"
    )
  ) 
}

# Make constituency names compatible with BSol.mapR
oh_data <- oh_data_raw %>%
  mutate(
    Constituency = gsub(
      "Birmingham, ", "",
      `Parliamentary Constituency Name`
    )
  ) %>%
  rename(Ward = `Ward Name`)

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
