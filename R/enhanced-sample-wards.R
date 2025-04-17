# Enhanced sample ward comparison
source("R/config.R")
library(dplyr)
library(ggplot2)

enhanced_wards <- c(
  "Alum Rock", "Aston", "Balsall Heath West", "Birchfield", "Bordesley Green", 
  "Castle Vale", "Garretts Green", "Glebe Farm & Tile Cross", "Gravelly Hill",
  "Handsworth", "Heartlands", "King's Norton South", "Kingstanding",
  "Lozells", "Shard End", "Sparkbrook & Balsall Heath East", "Tyseley & Hay Mills"
)

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
    dmft = sum(dmft),
    N = n(),
    IMD_score = mean(IMD..2019..Score)
  ) %>%
  mutate(
    p_hat = dmft / N,
    dmft_per_child = p_hat,
    a_prime = dmft + 1,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = dmft * (1 - 1/(9*dmft) - Z/3 * sqrt(1/a_prime))**3/N,
    UpperCI95 = a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/N,
    #Ward = stringr::str_wrap(Ward, width = 10),
    enhanced_sampling = case_when(
      Ward %in% enhanced_wards ~ "Enhanced Sampling",
      TRUE ~ NA
    )
  ) %>%
  arrange(
    desc(IMD_score)
  )

# Order Wards by deprivation level
enchanced_ward$Ward = factor(
  enchanced_ward$Ward,
  levels = enchanced_ward$Ward
)

# Birmingham average
brum_average <- oh_data_raw %>%
  summarise(
    dmft = sum(dmft),
    N = n()
  ) %>%
  mutate(
    brum_av_dmft = dmft / N
  ) %>%
  pull(
    brum_av_dmft
  )

ggplot(enchanced_ward, aes(y = Ward, x = dmft_per_child, fill = IMD_score)) +
  geom_col() + 
  theme_bw() +
  geom_errorbar(aes(xmin = LowerCI95, xmax = UpperCI95), width = 0.4)+
  geom_vline(aes(xintercept = brum_average,
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
    limits = c(20,60)
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.83, .95),
    legend.title.position = "top",
    legend.title.align = 0.5,
    legend.direction="horizontal",
    legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA), #transparent legend panel
    legend.margin=margin(c(0,5,-20,5))
    ) +
  guides(
    fill = guide_colorbar(order = 1),
    color = guide_legend(order = 2)
  )

ggsave("output/enhanced_wards.png",
       width = 7, height = 10)