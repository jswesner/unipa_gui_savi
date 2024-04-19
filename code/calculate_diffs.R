library(tidyverse)
library(janitor)
library(lubridate)
library(brms)
library(readxl)
library(tidybayes)

# load data
data_hobo_cleaned = readRDS(file = "data/data_hobo_cleaned.rds")

# load model
gam_hobo = readRDS("models/gam_hobo_new.rds")

# set grid
time_grid = tibble(julian_s = seq(from = min(data_hobo_cleaned$julian_s),
                                to = max(data_hobo_cleaned$julian_s),
                                length.out = 10)) %>% 
  expand_grid(month_f = unique(data_hobo_cleaned$month_f)) %>% 
  left_join(data_hobo_cleaned %>% ungroup %>% distinct(month_f, mean_julian, sd_julian))

# get posteriors over grid
hobo_posts = time_grid %>% 
  add_epred_draws(gam_hobo_new) %>% 
  mutate(julian_s = (julian_s*sd_julian) + mean_julian)

# get posterior of differences across each time/treatment (with help from ChatGTP)
hobo_post_diffs = hobo_posts%>%
  arrange(month_f, julian_s, .draw) %>%
  group_by(month_f, .draw) %>%
  mutate(difference = .epred - lag(.epred),
         abs_difference = abs(difference))

plot_diffs = hobo_post_diffs %>% 
  ggplot(aes(x = julian_s, y = difference)) + 
  stat_halfeye(aes(group = julian_s)) + 
  facet_wrap(~month_f, scales = "free_x") +
  geom_line(data = . %>% filter(.draw <= 100),
            aes(group = .draw), alpha = 0.2) +
  labs(x = "Time (minutes since start)",
       y = "Change in temperature over interval") +
  geom_hline(yintercept = 0)

plot_diffs

ggsave(plot_diffs, file = "plots/plot_diffs.jpg")



hobo_post_diffs %>% 
  group_by(julian_s, month_f) %>% 
  median_qi(difference) %>% print(n = Inf)


