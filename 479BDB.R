library(arrow)
library(ggplot2)
library(dplyr)


targetdata <- read_parquet("sumer_coverages_player_play.parquet")

supdata <- read.csv("114239_nfl_competition_files_published_analytics_final/supplementary_data.csv")
week1 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w01.csv")
week1out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w01.csv")
week2out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w02.csv")
week3out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w03.csv")
week4out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w04.csv")
week5out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w05.csv")
week6out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w06.csv")
week7out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w07.csv")
week8out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w08.csv")
week9out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w09.csv")
week10out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w10.csv")
week11out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w11.csv")
week12out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w12.csv")
week13out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w13.csv")
week14out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w14.csv")
week15out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w15.csv")
week16out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w16.csv")
week17out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w17.csv")
week18out <- read.csv("114239_nfl_competition_files_published_analytics_final/train/output_2023_w18.csv")


week2 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w02.csv")
week3 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w03.csv")
week4 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w04.csv")
week5 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w05.csv")
week6 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w06.csv")
week7 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w07.csv")
week8 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w08.csv")
week9 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w09.csv")
week10 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w10.csv")
week11 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w11.csv")
week12 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w12.csv")
week13 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w13.csv")
week14 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w14.csv")
week15 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w15.csv")
week16 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w16.csv")
week17 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w17.csv")
week18 <- read.csv("114239_nfl_competition_files_published_analytics_final/train/input_2023_w18.csv")


data <- rbind(week1, week2,week3,week4, week5, week6, week7, week8, week9,week10, week11, week12, 
              week13, week14, week15, week16, week17, week18)
outputs <- rbind(week1out,week2out, week3out, week4out, week5out, week6out, week7out, week8out,
                 week9out, week10out, week11out, week12out, week13out, week14out, week15out, week16out,
                 week17out, week18out)


write.csv(targetdata, "SumerSupplementData.csv", row.names = FALSE)

playerinfo <- data %>%
  distinct(game_id, play_id, nfl_id, .keep_all = TRUE) %>%
  select(game_id, play_id, nfl_id, player_name, player_position, player_to_predict, player_side)




outputs <- outputs %>%
  left_join(playerinfo, by = c("game_id", "play_id", "nfl_id"))

outputsinfo <- outputs %>%
  group_by(game_id,play_id, frame_id) %>%
  mutate(
    defense = (ifelse(player_side == "Defense", 1, 0))
  ) %>%
  summarise(
    totalDef = sum(defense)
  ) %>%
  filter(totalDef == 0)

outputplays <- outputs %>%
  distinct(game_id, play_id, .keep_all = TRUE)

outputsDef <- outputs %>%
  anti_join(outputsinfo, by = c("game_id", "play_id", "frame_id"))

week1targs <- week1 %>%
  left_join(targetdata, by = c("game_id", "play_id", "nfl_id"))

week1targs <- week1targs %>%
  filter(player_role == "Targeted Receiver" |  targeted_defender == TRUE)


targs <- data %>%
  left_join(targetdata, by = c("game_id", "play_id", "nfl_id"))

receivers479 <- targs %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, x, y, player_position, ball_land_x, ball_land_y)

# Get defenders at the same frame
defenders479 <- targs %>%
  filter(targeted_defender == TRUE) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = y, defenderX = x, player_position)

joined479 <- inner_join(receivers479, defenders479, by = c("game_id", "play_id", "frame_id"))


outputs479 <- outputs %>%
  semi_join(joined479, by = c("game_id", "play_id"))

receiversoutputs479 <- outputs479 %>%
  filter(player_position %in% c("RB", "WR", "TE")) %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, x, y, player_position)

# Get defenders at the same frame
defendersoutputs479 <- outputs479 %>%
  filter(player_position %in% c("CB", "SS","FS", "DB", "ILB", "LB", "MLB")) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = y, defenderX = x, player_position)

joinedoutputs479 <- inner_join(receiversoutputs479, defendersoutputs479, by = c("game_id", "play_id", "frame_id"))


joinedoutputs479 <- joinedoutputs479 %>%
  left_join(targetdata, by = c("game_id", "play_id", "defenderId" = "nfl_id"))

joinedoutputs479 <- joinedoutputs479 %>%
  filter(targeted_defender == "TRUE")


final_frame_df <- joinedoutputs479 %>%
  group_by(game_id, play_id) %>%
  slice_max(order_by = frame_id, n = 1, with_ties = FALSE) %>%
  ungroup()

distances <- final_frame_df %>%
  left_join(supdata, by = c("game_id", "play_id"))

colnames(supdata)

distances <- distances %>%
  filter(!grepl("fumble", play_description, ignore.case = TRUE)) %>%
  filter(pass_result == "C", is.na(penalty_yards))%>%
  mutate(
    yac = yards_gained - pass_length,
    final_distance = sqrt((x - defenderX)^2 +
                            (y - defenderY)^2)
  ) %>%
  filter(yac > 0, final_distance < 40, pass_length <= 10)


observe <- distances %>%
  select(player_name.x, player_name.y, x, y, defenderX, defenderY, yac, final_distance, )

model <- lm(yac ~ final_distance, data = distances)
summary(model)

library(ggplot2)

ggplot(distances, aes(final_distance, yac)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Defender Distance at Final Frame and YAC",
    x = "Final Defender–Receiver Distance",
    y = "Yards After Catch"
  )


# distances between player and ball

outputs_updated_frames = outputs %>% mutate(frame_id = frame_id+100)

output_players = outputs_updated_frames %>% select(game_id,play_id,nfl_id) %>% distinct()

data_only_in_out = data %>% 
  semi_join(output_players, by = c("game_id","play_id","nfl_id"))


joined_distances = rbind(select(data_only_in_out,colnames(outputs_updated_frames)),outputs_updated_frames)

ball_land = data %>% 
  select(game_id,play_id,ball_land_x,ball_land_y) %>% 
  distinct()

joined_distances = joined_distances %>% 
  left_join(ball_land,by = c("game_id","play_id")) %>% 
  mutate(distance_ball = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2)) 

target_defender = joined_distances %>% 
  filter(player_side == "Defense") %>% 
  group_by(game_id, play_id,nfl_id) %>%
  slice_max(order_by = frame_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  group_by(game_id, play_id) %>% 
  slice_max(order_by = -distance_ball, n=1,with_ties = FALSE) %>% 
  ungroup() %>% 
  select(game_id,play_id,nfl_id) %>% 
  mutate(dist = TRUE)

player_names = data_only_in_out %>% 
  select(nfl_id,player_name,player_position) %>% 
  distinct()

select_distances = joined_distances %>% 
  filter(player_side == "Defense") %>% 
  group_by(game_id, play_id,nfl_id) %>%
  slice_max(order_by = frame_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(game_id,play_id,nfl_id, distance_ball)


sumer_targ_def = sumer_sup %>% 
  filter(targeted_defender) %>% 
  select(game_id,play_id,nfl_id) %>% 
  mutate(sumer = TRUE)

targeted_distance_vs_sumer = target_defender %>% 
  full_join(sumer_targ_def, by = c("game_id","play_id","nfl_id")) %>%
  left_join(player_names, by = "nfl_id") %>% 
  left_join(select_distances, by = c("game_id","play_id","nfl_id")) %>% 
  mutate(sumer = replace_na(sumer, FALSE), dist = replace_na(dist,FALSE)) %>% 
  arrange(game_id, play_id, nfl_id)

summarise(targeted_distance_vs_sumer, sumer = mean(sumer), dist = mean(dist))

#distance to receiver

joinedoutputs479 <- joinedoutputs479 %>%
  left_join(balllands, by = c("game_id", "play_id"))
joinedoutputs479 <- joinedoutputs479 %>%
  group_by(game_id, play_id) %>%
  mutate(
    distanceReceiver = sqrt((defenderX - x)^2 + (defenderY - y)^2),
    distanceBall = sqrt((defenderX - ball_land_x)^2 + (defenderY - ball_land_y)^2)
  )
#instantaneous velocity towards receiver vs towards ball at each frame calculated in ROC's
#feel free to double check this calculation
ROCs <- joinedoutputs479 %>%
  group_by(game_id, play_id, defenderId) %>%
  mutate(
    close_rec_inst = (distanceReceiver - lead(distanceReceiver)) / 0.1,
    close_ball_inst = (distanceBall - lead(distanceBall)) / 0.1,
    
    close_rec_inst = ifelse(is.infinite(close_rec_inst) | is.nan(close_rec_inst), NA, close_rec_inst),
    close_ball_inst = ifelse(is.infinite(close_ball_inst) | is.nan(close_ball_inst), NA, close_ball_inst)
  )

ROCjoined <- ROCs %>%
  left_join(distances, by = c("game_id", "play_id"))


