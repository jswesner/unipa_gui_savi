library(tidyverse)
library(janitor)
library(lubridate)
library(brms)
library(readxl)
library(tidybayes)

data_Hobo_T_C <- read_excel("data_per_savi/data_Hobo_T°C_final.xlsx") %>% 
  clean_names() %>% 
  arrange(date_hour) %>% 
  mutate(time = row_number(),
         mean_time = mean(time),
         sd_time = sd(time),
         time_s = (time - mean_time)/sd_time,
         minute = minute(date_hour),
         hour = hour(date_hour),
         hour_minute = paste0(hour, "_", minute),
         month = month(date_hour),
         day = day(date_hour),
         hour_day = paste0(day, "_", hour),
         month_day = paste0(month, "_", day),
         julian = julian(date_hour)) %>% 
  group_by(hour_minute) %>% 
  mutate(hour_minute_id = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(mean_hour_minute = mean(hour_minute_id),
         sd_hour_minute = sd(hour_minute_id),
         hour_minute_s = (hour_minute_id - mean_hour_minute)/sd_hour_minute) %>% 
  pivot_longer(cols = starts_with("t_")) %>%
  mutate(name_day = paste0(name, "_", day)) %>% 
  mutate(value_s = value/mean(value, na.rm = T)) %>% 
  filter(month != 8) %>% 
  group_by(month) %>% 
  mutate(mean_julian = mean(julian),
         sd_julian = sd(julian),
         julian_s = scale(julian),
         month_f = as.factor(month))

saveRDS(data_Hobo_T_C, file = "data/data_hobo_cleaned.rds")

test %>% 
  ggplot(aes(x = julian_s, y = value_s, color = as.factor(month))) +
  geom_point() +
  # facet_wrap(~month, scales = "free") +
  NULL
  


# GAM = generalized additive model
test = data_Hobo_T_C %>% sample_n(size = 400)

gam_hobo = brm(value ~ s(time_s, by = name_day),
               data = test,
               iter = 1000, chains = 2,
               prior = c(prior(normal(1, 0.5), class = "Intercept")))

saveRDS(gam_hobo, file = "models/gam_hobo.rds")

gam_hobo = readRDS(file = "models/gam_hobo.rds")


gam_hobo_new = brm(value_s ~ s(julian_s, by = month_f),
                   data = test,
                   iter = 1000, chains = 1,
                   prior = c(prior(normal(1, 0.5), class = "Intercept")))



cond_plot = plot(conditional_effects(gam_hobo_new), points = T)


hobo_epred = tibble(julian_s = seq(min(test$julian_s), 
                                   max(test$julian_s),
                                   length.out = 50)) %>% 
  expand_grid(month_f = unique(test$month_f)) %>% 
  add_epred_draws(gam_hobo_new, re_formula = NULL)


hobo_epred %>% 
  ggplot(aes(x = julian_s, y = .epred, fill = month_f)) + 
  stat_lineribbon()




