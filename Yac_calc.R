library(tidyverse)

sup = read_csv('supplementary_data.csv')

yac = sup %>% 
  select(game_id,play_id,pass_length,pass_result,pre_penalty_yards_gained,play_description) %>% 
  filter(pass_result == "C", !str_detect(play_description, "FUMBLES")) %>% 
  mutate(yac = pre_penalty_yards_gained - pass_length)
