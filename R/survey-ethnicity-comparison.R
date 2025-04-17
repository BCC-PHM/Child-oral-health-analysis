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
      "processed-oral-health-data-2024.csv"
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
      Ethnicity == "Other Mixed or Multiple ethnic groups" ~ "Mixed other",
      Ethnicity == "Irish" ~ "Other White",
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
      Ethnicity == "British / English / Welsh / Scottish / Northern Irish" ~ "White British",
      Ethnicity == "African" ~ "African",
      Ethnicity == "Caribbean" ~ "Caribbean",
      Ethnicity == "Asian other including not provided" ~ "Other Asian",
      Ethnicity == "Black other including not provided" ~ "Other Black",
      Ethnicity == "Mixed other including not provided" ~ "Mixed other",
      Ethnicity == "White Asian" ~ "White and Asian",
      Ethnicity == "White Black African" ~ "White and Black African",      
      Ethnicity == "White Black Caribbean" ~ "White and Black Caribbean",    
      Ethnicity == "White other including not provided" ~ "Other White",
      Ethnicity == "Any other including not provided" ~ "Any other ethnic group",
      Ethnicity == "Irish" ~ "Other White",
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
    BroadEthnicity = case_when(
      Ethnicity %in% c("Pakistani", "Indian", 
                       "Bangladeshi", "Other Asian",
                       "Chinese") ~ "Asian",
      Ethnicity %in% c("African", "Caribbean", 
                       "Other Black") ~ "Black",
      Ethnicity %in% c("White and Black Caribbean", 
                       "White and Black African", 
                       "White and Asian",
                       "Mixed other") ~ "Mixed",
      Ethnicity %in% c("White British", "Irish", 
                       "Other White",
                       "Gypsy or Irish Traveller",
                       "Roma") ~ "White",
      Ethnicity %in% c("Any other ethnic group", "Arab") ~ "Other",
      Ethnicity %in% c("Not stated") ~ "Unknown"
    ),
    n = tidyr::replace_na(n, 0),
    sample_perc = n / sum(n),
    pop_perc = N / sum(N, na.rm = T),
    difference = sample_perc - pop_perc,
    n_SE = tidyr::replace_na(
      sqrt(n * (sum(n) - n)/sum(n)^3),
      0
      ),
    N_SE =  sqrt(N * (sum(N, na.rm = T) - N)/sum(N, na.rm = T)^3),
    diff_SE = sqrt(n_SE^2 + N_SE^2),
    # Create annotation text with aligned layout
    plt_text = case_when(
      n < 10 ~ paste0("n:", n, "     | N:", N),
      n < 100 ~ paste0("n:", n, "   | N:", N),
      TRUE ~ paste0("n:", n, " | N:", N),
      ),
    diff_95CIUpper = difference + 1.96 * diff_SE,
    diff_95CILower = difference - 1.96 * diff_SE,
    
    group = case_when(
      difference > 0 & diff_95CILower>0 ~ "Overrepresented",
      difference < 0 & diff_95CIUpper< 0~ "Underrepresented",
      TRUE ~ "No significant difference"
    ),
  ) %>%
  arrange(BroadEthnicity, Ethnicity) %>%
  filter(
    !is.na(pop_perc)
  )

eth_order <- rev(unique(all_data$Ethnicity))
all_data$Ethnicity <- factor(
  all_data$Ethnicity,
  levels = eth_order
)

all_data$group <- factor(
  all_data$group,
  levels = c("Underrepresented","No significant difference", "Overrepresented")
)

ggplot(all_data, aes(y = Ethnicity, x = difference, fill = group)) +
  geom_col() +
  geom_errorbar(aes(xmin=diff_95CILower, xmax=diff_95CIUpper),width=.3) +
  theme_bw() + 
  labs(
    y = "",
    x = "Sample Percentage - Population Percentage",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    plot.margin = unit(c(1,6,1,1), "lines"),
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_cartesian(clip = 'off') +
  geom_text(
    aes(label = plt_text),
    x = 0.17,
    vjust = 0.5,
    hjust = 0,
    size = 4) +
  scale_fill_manual(values = c("#77BF42", "gray", "#D30F7B")) +
  scale_x_continuous(
    labels = scales::percent,
    lim = c(-0.15, 0.15))
  
ggsave("output/ethnic_representation_2024.png",
       width = 7, height = 5, dpi = 300)