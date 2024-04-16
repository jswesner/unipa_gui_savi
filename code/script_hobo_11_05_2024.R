library(tidyverse)
library(janitor)
library(lubridate)
library(brms)
library(readxl)

data_Hobo_T_C <- read_excel("data_per_savi/data Hobo T°C.xlsx") %>% 
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
         month_day = paste0(month, "_", day)) %>% 
  group_by(hour_minute) %>% 
  mutate(hour_minute_id = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(mean_hour_minute = mean(hour_minute_id),
         sd_hour_minute = sd(hour_minute_id),
         hour_minute_s = (hour_minute_id - mean_hour_minute)/sd_hour_minute) %>% 
  pivot_longer(cols = starts_with("t_")) %>%
  mutate(name_day = paste0(name, "_", day)) %>% 
  mutate(value_s = value/mean(value, na.rm = T))

write_csv(data_Hobo_T_C, file = "data/data_hobo_cleaned.csv")

data_Hobo_T_C %>% 
  ggplot(aes(x = hour_minute_s, y = value_s)) +
  geom_point(aes(color = as.factor(day))) +
  geom_line(aes(group = day)) +
  facet_grid(vars(name))


# GAM = generalized additive model
test = data_Hobo_T_C %>% sample_n(size = 200)

gam_hobo = brm(value ~ s(time_s, by = name_day),
               data = test,
               iter = 1000, chains = 2,
               prior = c(prior(normal(1, 0.5), class = "Intercept")))

saveRDS(gam_hobo, file = "models/gam_hobo.rds")


cond_plot = plot(conditional_effects(gam_hobo), points = T)

cond_plot$`time_s:name_day` + 
  facet_wrap(~name_day, scales = "free")
