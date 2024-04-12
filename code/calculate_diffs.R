library(tidyverse)
library(janitor)
library(lubridate)
library(brms)
library(readxl)
library(tidybayes)

# load data
data_hobo_cleaned = read_csv(file = "data/data_hobo_cleaned.csv")

# load model
gam_hobo = readRDS("models/gam_hobo.rds")

# set grid
time_grid = tibble(time_s = seq(from = min(data_hobo_cleaned$time_s),
                                to = max(data_hobo_cleaned$time_s),
                                length.out = 10)) %>% 
  expand_grid(name = unique(data_hobo_cleaned$name))

# get posteriors over grid
hobo_posts = time_grid %>% 
  add_epred_draws(gam_hobo)

# get posterior of differences across each time/treatment (with help from ChatGTP)
hobo_post_diffs = hobo_posts%>%
  arrange(name, time_s, .draw) %>%
  group_by(name, .draw) %>%
  mutate(difference = .epred - lag(.epred),
         abs_difference = abs(difference)) 

# add original times and plot
mean_time = unique(data_hobo_cleaned$mean_time)
sd_time = unique(data_hobo_cleaned$sd_time)

plot_diffs = hobo_post_diffs %>% 
  mutate(time = (time_s*sd_time) + mean_time) %>% 
  ggplot(aes(x = time, y = difference)) + 
  stat_halfeye(aes(group = time_s)) + 
  facet_wrap(~name) +
  labs(x = "Time (minutes since start)",
       y = "Change in temperature over interval") +
  ylim(-20, 20) +
  geom_hline(yintercept = 0)

ggsave(plot_diffs, file = "plots/plot_diffs.jpg")
