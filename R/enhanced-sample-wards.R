# Enhanced sample ward comparison
source("R/config.R")
library(dplyr)
library(ggplot2)
library(readxl)
library(BSol.mapR)

enhanced_wards <- c(
  "Alum Rock", "Aston", "Balsall Heath West", "Birchfield", "Bordesley Green", 
  "Castle Vale", "Garretts Green", "Glebe Farm & Tile Cross", "Gravelly Hill",
  "Handsworth", "Heartlands", "King's Norton South", "Kingstanding",
  "Lozells", "Shard End", "Sparkbrook & Balsall Heath East", "Tyseley & Hay Mills"
)

# If not already loaded, then load the oral health data
if(!exists("oh_data_raw")) {
  oh_data_raw <- read.csv(
    file.path(
      data_path, 
      "processed-oral-health-data-2024.csv"
    )
  ) 
}

# List of 15 pupil + wards
wards_15_plus <- oh_data_raw %>% 
  rename(Ward = Ward.Name) %>%
  count(Ward) %>%
  filter(n >= 15) %>%
  pull(Ward)

enchanced_ward_data <- oh_data_raw %>%
  rename(Ward = Ward.Name) %>%
  mutate(
    Ward = case_when(
      Ward %in% wards_15_plus ~ Ward,
      TRUE ~ "* All other wards *"
    )
  ) %>%
  group_by(
    Ward
  ) %>%
  summarise(
    dmft_gtr_0 = mean(dmft > 0),
    dmft_n = sum(dmft),
    N = n(),
    IMD_score = mean(IMD..2019..Score)
  ) %>%
  mutate(
    Z = qnorm(0.975),

    # average DMFT per child (DMFT rate per 1 child)
    dmft_rate = dmft_n / N,
    a_prime_rate = dmft_n + 1,
    dmft_rate_Lower = dmft_n * (1 - 1/(9*dmft_n) - Z/3 * sqrt(1/a_prime_rate))**3/N,
    dmft_rate_Upper = a_prime_rate * (1 - 1/(9*a_prime_rate) + Z/3 * sqrt(1/a_prime_rate))**3/N,
    
    # proportion of children with DMFT > 1
    dmft_perc = dmft_gtr_0,
    dmft_perc_Lower = (dmft_gtr_0 + Z^2/(2*N) - Z * sqrt((dmft_gtr_0*(1-dmft_gtr_0)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
    dmft_perc_Upper = (dmft_gtr_0 + Z^2/(2*N) + Z * sqrt((dmft_gtr_0*(1-dmft_gtr_0)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
    
    #Ward = stringr::str_wrap(Ward, width = 10),
    enhanced_sampling = case_when(
      Ward %in% enhanced_wards ~ "Enhanced Sampling",
      TRUE ~ NA
    )
  ) %>%
  arrange(
    IMD_score
  )

large_sample_wards <- enchanced_ward_data %>%
  filter(Ward != "* All other wards *") %>%
  pull(Ward)

brum_averages <- enchanced_ward_data %>%
  select(c(Ward, dmft_rate, dmft_perc)) %>%
  # Join to 5 year old populations
  full_join(
    read_excel("data/five-year-olds-by-ward.xlsx") %>%
      # Group wards will small samples
      mutate(
        Ward = case_when(
          Ward %in% large_sample_wards ~ Ward,
          TRUE ~ "* All other wards *"
        )
      ) %>%
      group_by(Ward) %>%
      summarise(
        Observation = sum(Observation)
      ),
    by = join_by("Ward")
  ) %>%
  ungroup() %>%
  # Calculate rescaled dmft rates and percentages
  summarise(
    dmft_rate = sum(dmft_rate * Observation) / sum(Observation),
    dmft_perc = sum(dmft_perc * Observation) / sum(Observation)
  )

av_dmft_perc <- brum_averages %>%
  pull(
    dmft_perc
  )

av_dmft_rate <- brum_averages %>%
  pull(
    dmft_rate
  )

enchanced_ward_data <- enchanced_ward_data %>%
  mutate(
    dmft_rate_difference = case_when(
      dmft_rate_Lower > av_dmft_rate ~ "Above Average",
      dmft_rate_Upper < av_dmft_rate ~ "Below Average",
      TRUE ~ "No significant difference"
    ),
    dmft_perc_difference = case_when(
      dmft_perc_Lower > av_dmft_perc ~ "Above Average",
      dmft_perc_Upper < av_dmft_perc ~ "Below Average",
      TRUE ~ "No significant difference"
    )
  )

# Order Wards by deprivation level
enchanced_ward_data$Ward = factor(
  enchanced_ward_data$Ward,
  levels = enchanced_ward_data$Ward
)


# Order dmft rate differences
enchanced_ward_data$dmft_rate_difference = factor(
  enchanced_ward_data$dmft_rate_difference,
  levels = c("Below Average", "No significant difference", "Above Average")
)

# Order perc rate differences
enchanced_ward_data$dmft_perc_difference = factor(
  enchanced_ward_data$dmft_perc_difference,
  levels = c("Below Average", "No significant difference", "Above Average")
)

### DMFT Rate ###

ggplot(enchanced_ward_data, aes(y = Ward, x = dmft_rate, fill = dmft_rate_difference)) +
  geom_col() + 
  theme_bw() +
  geom_errorbar(aes(xmin = dmft_rate_Lower, xmax = dmft_rate_Upper), width = 0.4)+
  geom_vline(aes(xintercept = av_dmft_rate), 
             linewidth = 1.1, 
             linetype = "dashed",
             ) +
  labs(
    fill = "",
    x = "Average dmft per child",
    y = "",
    color = ""
  ) +
  scale_fill_manual(
    values = c("#3737E1", "darkgray", "lightblue")
  ) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    legend.title.align = 0.5,
    legend.direction="horizontal",
    legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA), #transparent legend panel
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
    ) +
  geom_text(label = "Most Deprived", 
            x = 3, 
            y = 39.5,
            size = 5) +
  geom_text(label = "Least Deprived", 
            x = 3, 
            y = 1.5,
            size = 5) 

ggsave("output/dmft_rate_2024.png",
       width = 8, height = 10)

# Print wards with significantly higher DMFT rate
print("Significantly higher DMFT rate in the following wards:")
enchanced_ward_data %>% 
  filter(dmft_rate_Lower > av_dmft_rate) %>%
  select(c(Ward, dmft_rate )) %>%
  arrange(desc(dmft_rate))

### DMFT > 0 Prevalence ###

ggplot(enchanced_ward_data, aes(y = Ward, x = dmft_perc, fill = dmft_perc_difference)) +
  geom_col() + 
  theme_bw() +
  geom_errorbar(aes(xmin = dmft_perc_Lower, xmax = dmft_perc_Upper), width = 0.4)+
  geom_vline(aes(xintercept = av_dmft_perc), 
             linewidth = 1.1, 
             linetype = "dashed",
  ) +
  labs(
    fill = "",
    x = "Percentage of children with dmft > 0",
    y = "",
    color = ""
  ) +
  scale_fill_manual(
    values = c("#3737E1", "darkgray", "lightblue")
  ) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    legend.title.align = 0.5,
    legend.direction="horizontal",
    legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA), #transparent legend panel
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  xlim(0,1) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0,0)
  ) +
  geom_text(label = "Most Deprived", 
            x = 0.85, 
            y = 39.5,
            size = 5) +
  geom_text(label = "Least Deprived", 
            x = 0.85, 
            y = 1.5,
            size = 5) 
  
ggsave("output/dmft_perc_2024.png",
       width = 8, height = 10)

# Print wards with significantly higher DMFT rate
print("Significantly DMFT prevalence in the following wards:")
enchanced_ward_data %>% 
  filter(dmft_perc_Lower > av_dmft_perc) %>%
  select(c(Ward, dmft_perc )) %>%
  arrange(desc(dmft_perc))

### Ward map

enchanced_map_vals <- oh_data_raw %>%
  rename(Ward = Ward.Name) %>%
  group_by(
    Ward
  ) %>%
  summarise(
    dmft = sum(dmft),
    N = n()
  ) %>%
  mutate(
    dmft_per_child = case_when(
      N >= 15 ~ dmft / N,
      TRUE ~ NA
    )
  )

map <- plot_map(
  enchanced_map_vals,
  value_header = "dmft_per_child",
  map_type = "Ward",
  area_name = "Birmingham",
  fill_title = "Average DMFT per child (2024)",
  style = "cont"
)
map
save_map(
  map,
  save_name = "output/dmft-map-2024-v2.png"
)

######################################################################
#            DMFT prevalence by IMD (National and LA)                #
######################################################################

dmft_plot_LA <- oh_data_raw %>%
  group_by(IMD19_LA_Quintile) %>%
  summarise(
    dmft_prev = mean(dmft > 0),
    N = n()
  ) %>%
  mutate(
    Z = qnorm(0.975),
    dmft_perc_Lower = (dmft_prev + Z^2/(2*N) - Z * sqrt((dmft_prev*(1-dmft_prev)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
    dmft_perc_Upper = (dmft_prev + Z^2/(2*N) + Z * sqrt((dmft_prev*(1-dmft_prev)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
  ) %>%
  ggplot(
    aes(x = IMD19_LA_Quintile, y = dmft_prev)
  ) +
  theme_bw() +
  geom_col(fill = "#3888C1") +
  geom_errorbar(
    aes(
      ymin = dmft_perc_Lower, 
      ymax = dmft_perc_Upper
      ), 
    width = 0.4) +
  scale_y_continuous(
    limits = c(0, 0.5),
    expand = c(0, 0),
    labels = scales::percent
  ) +
  scale_x_continuous(
    breaks = c(1,2,3,4,5),
    labels = c("1\nMost deprived", "2", "3", "4", "5\n Least deprived")
  ) +
  labs(
    x = "IMD Quintile (Birmingham scale)",
    y = "Prevalence of DMFT"
  )

dmft_plot_LA
ggsave("output/dmft-imd-la-2024.png", dmft_plot_LA,
       width = 5, height = 3)


dmft_plot_nat <- oh_data_raw %>%
  group_by(IMD19_National_Quintile) %>%
  summarise(
    dmft_prev = mean(dmft > 0),
    N = n()
  ) %>%
  mutate(
    Z = qnorm(0.975),
    dmft_perc_Lower = (dmft_prev + Z^2/(2*N) - Z * sqrt((dmft_prev*(1-dmft_prev)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
    dmft_perc_Upper = (dmft_prev + Z^2/(2*N) + Z * sqrt((dmft_prev*(1-dmft_prev)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
  ) %>%
  ggplot(
    aes(x = IMD19_National_Quintile, y = dmft_prev)
  ) +
  theme_bw() +
  geom_col(fill = "#3888C1") +
  geom_errorbar(
    aes(
      ymin = dmft_perc_Lower, 
      ymax = dmft_perc_Upper
    ), 
    width = 0.4) +
  scale_y_continuous(
    limits = c(0, 0.6),
    expand = c(0, 0),
    labels = scales::percent
  ) +
  scale_x_continuous(
    breaks = c(1,2,3,4,5),
    labels = c("1\nMost deprived", "2", "3", "4", "5\n Least deprived")
  ) +
  labs(
    x = "IMD Quintile (National scale)",
    y = "Prevalence of DMFT"
  )

dmft_plot_nat
ggsave("output/dmft-imd-nat-2024.png", dmft_plot_nat,
       width = 5, height = 3)