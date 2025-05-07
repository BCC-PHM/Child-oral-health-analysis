# Enhanced sample ward comparison
source("R/config.R")
library(dplyr)
library(ggplot2)
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

enchanced_ward <- oh_data_raw %>%
  rename(Ward = Ward.Name) %>%
  mutate(
    Ward = case_when(
      Ward %in% wards_15_plus ~ Ward,
      TRUE ~ "All other wards"
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
    dmft_perc = 100 * dmft_gtr_0,
    dmft_perc_Lower = 100 * (dmft_gtr_0 + Z^2/(2*N) - Z * sqrt((dmft_gtr_0*(1-dmft_gtr_0)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
    dmft_perc_Upper = 100 * (dmft_gtr_0 + Z^2/(2*N) + Z * sqrt((dmft_gtr_0*(1-dmft_gtr_0)/N) + Z^2/(4*N^2))) / (1 + Z^2/N),
    
    #Ward = stringr::str_wrap(Ward, width = 10),
    enhanced_sampling = case_when(
      Ward %in% enhanced_wards ~ "Enhanced Sampling",
      TRUE ~ NA
    )
  ) %>%
  arrange(
    IMD_score
  )

# Order Wards by deprivation level
enchanced_ward$Ward = factor(
  enchanced_ward$Ward,
  levels = enchanced_ward$Ward
)

# Birmingham average
brum_average <- oh_data_raw %>%
  summarise(
    dmft_n = sum(dmft),
    dmft_perc = 100 * mean(dmft>0),
    N = n()
  ) %>%
  mutate(
    brum_av_dmft = dmft_n / N
  ) 

av_dmft_rate <- brum_average %>%
  pull(
    brum_av_dmft
  )

av_dmft_perc <- brum_average %>%
  pull(
    dmft_perc
  )

### DMFT Rate ###

ggplot(enchanced_ward, aes(y = Ward, x = dmft_rate, fill = IMD_score)) +
  geom_col() + 
  theme_bw() +
  geom_errorbar(aes(xmin = dmft_rate_Lower, xmax = dmft_rate_Upper), width = 0.4)+
  geom_vline(aes(xintercept = av_dmft_rate,
                 color = "Birmingham Average"), 
             linewidth = 1.1, 
             linetype = "dashed",
             ) +
  labs(
    fill = "Average IMD Score",
    x = "Average DMFT per child",
    y = "",
    color = ""
  ) +
  #geom_text(label = "Birmingham Average", x = 15.5, y = 1.3, color = "darkorange") +
  scale_fill_continuous(
    limits = c(20,60),
    breaks = c(20, 30, 40, 50, 60),
    labels = c("20\nLeast deprived", "30", "40", "50", "60\nMost deprived")
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.78, .1),
    legend.title.position = "top",
    legend.title.align = 0.5,
    legend.direction="horizontal",
    legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA), #transparent legend panel
    legend.margin=margin(c(0,5,-20,5)),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
    ) +
  guides(
    fill = guide_colorbar(order = 1),
    color = guide_legend(order = 2)
  )

ggsave("output/dmft_rate_2024.png",
       width = 8, height = 10)


### DMFT > 0 Prevalence ###

ggplot(enchanced_ward, aes(y = Ward, x = dmft_perc, fill = IMD_score)) +
  geom_col() + 
  theme_bw() +
  geom_errorbar(aes(xmin = dmft_perc_Lower, xmax = dmft_perc_Upper), width = 0.4)+
  geom_vline(aes(xintercept = av_dmft_perc,
                 color = "Birmingham Average"), 
             linewidth = 1.1, 
             linetype = "dashed",
  ) +
  labs(
    fill = "Average IMD Score",
    x = "Percentage of children with DMFT > 0",
    y = "",
    color = ""
  ) +
  #geom_text(label = "Birmingham Average", x = 15.5, y = 1.3, color = "darkorange") +
  scale_fill_continuous(
    limits = c(20,60),
    breaks = c(20, 30, 40, 50, 60),
    labels = c("20\nLeast deprived", "30", "40", "50", "60\nMost deprived")
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.78, .1),
    legend.title.position = "top",
    legend.title.align = 0.5,
    legend.direction="horizontal",
    legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA), #transparent legend panel
    legend.margin=margin(c(0,5,-20,5)),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    color = guide_legend(order = 2)
  ) +
  xlim(0,100)
  

ggsave("output/dmft_perc_2024.png",
       width = 8, height = 10)


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