#Library necessary packages
library(tidyverse)
library(ggplot2)

#Read in statcast master CSV
statcast_master <- read_csv("savant_pitch_level.csv")

#Reading in predicted RP ERA (for current SP) dataset based on our model
#Filter to only have 2023 pitchers, and an ERA improvement greater than 0.5
modeled_SP_23_good <- read_csv("predicted_SP_ERA.csv") %>% 
  filter(Season == "2023") %>%
  filter(ERA_dif > 0.5)

#Candidates for profiles set as a vector
SP_23_candidates <- as.vector(modeled_SP_23_good$ID)

#Filter statcast master dataframe to only include 2023 candidate pitchers
SP_23_statcast <- statcast_master %>% 
  filter(pitcher %in% SP_23_candidates & game_date > "2023-01-01")

#Create dataframe that only has actual events from SP_23_stacast
SP_23_events <- SP_23_statcast %>% drop_na(events)

#Visualization for Gonsolin's Pitch Number against Run Expectancy for 2023
SP_23_events %>% filter(player_name == "Gonsolin, Tony") %>%
  ggplot(aes(x = pitch_number_appearance, y = delta_run_exp)) + 
  geom_point() + geom_smooth() + 
  labs(title = "Tony Gonsolin Pitch Number against Run Expectancy 2023") + 
  theme_bw()

#Visualization for Crawford's Pitch Number against Run Expectancy for 2023
SP_23_events %>% filter(player_name == "Crawford, Kutter") %>%
  ggplot(aes(x = pitch_number_appearance, y = delta_run_exp)) + 
  geom_point() + geom_smooth() + 
  labs(title = "Kutter Crawford Pitch Number against Run Expectancy 2023") + 
  theme_bw()

#Visualization for Crawford's Pitch Number against Fastball Velocity for 2023
SP_23_events %>% filter(player_name == "Crawford, Kutter") %>% 
  filter(pitch_type == "FF" | pitch_type == "SI") %>%
  ggplot(aes(x = pitch_number_appearance, y = release_speed)) + 
  geom_point() + geom_smooth() + 
  labs(title = "Kutter Crawford Pitch Number against FB velo") + 
  theme_bw()