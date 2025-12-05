library(tidyverse)
library(xgboost)
library(ggthemes)
library(ggtext)
library(extrafont)

# View first 501 fonts

one_play_in = data %>% 
  filter(game_id == 2023102201, play_id == 485) %>% 
  select(colnames(outputs))
frames = max(one_play_in$f)
one_play_out = outputs %>% 
  filter(game_id == 2023102201, play_id == 485) %>% 
  mutate(frame_id = frame_id+frames)
one_play = rbind(one_play_in,one_play_out) %>% 
  mutate(x = x, y = y)

sup = supdata %>% 
  filter(game_id == 2023102201, play_id == 485)

ball = data %>% 
  filter(game_id == 2023102201, play_id == 485) %>% 
  select(ball_land_x, ball_land_y) %>% 
  distinct()

players_out = one_play_out %>% 
  select(nfl_id) %>% 
  distinct()

players_in = one_play %>% 
  filter(!nfl_id %in% players_out) %>% 
  select(nfl_id) %>% 
  distinct()

one_p_players = one_play %>% 
  semi_join(players_out, by = "nfl_id")

library(gganimate)
library(sportyR)
library(ggtext)



filled_in = one_play %>% 
  mutate(alpha = 1) %>% 
  group_by(nfl_id) %>%
  complete(frame_id = full_seq(1:41, 1)) %>%     # ensure every id has frame_id 1–41
  mutate(
    alpha = if_else(is.na(alpha), 0, alpha)  # missing (new) rows become 0.5
  ) %>%
  filter(nfl_id == 55998 | nfl_id == 47862 ) %>%
  fill(everything(), .direction = "down") %>%     # fill missing rows using last known data
  ungroup()

play_data <- ROCjoined %>%
  filter(game_id == 2023102201, play_id == 485) %>%
  left_join(dirDiffs, by = c("game_id", "play_id"))

play_data <- play_data %>% arrange(frame_id.x)

play_data <- play_data %>%
  arrange(frame_id.x) %>%
  mutate(
    dx = c(0, diff(defenderX.x)),
    dy = c(0, diff(defenderY.x)),
    distance = sqrt(dx^2 + dy^2)
  )

avg_speed <- mean(play_data$distance[-1], na.rm = TRUE) # skip first row (0)


vec_x <- play_data$ball_land_x - play_data$defenderX.x[1]
vec_y <- play_data$ball_land_y - play_data$defenderY.x[1]
total_dist <- sqrt(vec_x^2 + vec_y^2)

# Number of frames needed to match actual speed
n_frames_needed <- ceiling(total_dist / avg_speed)

# Recreate straight-line path with realistic speed
straight_line_scaled <- data.frame(
  frame_id = seq(min(play_data$frame_id.x), length.out = n_frames_needed),
  defenderX_alt = play_data$defenderX.x[1] + seq(0, 1, length.out = n_frames_needed) * vec_x,
  defenderY_alt = play_data$defenderY.x[1] + seq(0, 1, length.out = n_frames_needed) * vec_y
) 

straight_line_scaled <- straight_line_scaled %>%
  filter(frame_id <= 10)

AltCheck <- play_data %>%
  left_join(straight_line_scaled, by = c( "frame_id.x" = "frame_id"))

AltCheck <- AltCheck %>%
  group_by(game_id, play_id) %>%
  mutate(
    distanceReceiverAlt = sqrt((defenderX_alt - x.x)^2 + (defenderY_alt - y.x)^2),
    final_distanceAlt = sqrt((defenderX_alt[10] - x.x[10])^2 + (defenderY_alt[10] - y.x[10])^2),
    distanceBallAlt = sqrt((defenderX_alt - ball_land_x)^2 + (defenderY_alt - ball_land_y)^2)
  )
#instantaneous velocity towards receiver vs towards ball at each frame calculated in ROC's
#feel free to double check this calculation
AltCheck <- AltCheck %>%
  group_by(game_id, play_id, defenderId.x) %>%
  mutate(
    close_rec_instAlt = (distanceReceiverAlt - lead(distanceReceiverAlt)) / 0.1,
    close_ball_instAlt = (distanceBallAlt - lead(distanceBallAlt)) / 0.1,
    
    close_rec_instAlt = ifelse(is.infinite(close_rec_inst) | is.nan(close_rec_inst), NA, close_rec_inst),
    close_ball_instAlt = ifelse(is.infinite(close_ball_inst) | is.nan(close_ball_inst), NA, close_ball_inst)
  ) %>%
  ungroup()

OutputCheck <- AltCheck %>%
  filter(frame_id.x >= 6, frame_id.x <= 9)

dmat_actual <- xgb.DMatrix(
  data = as.matrix(OutputCheck %>% select(close_rec_inst = close_rec_instAlt, close_ball_inst = close_ball_instAlt, final_distance = final_distanceAlt, distanceReceiver = distanceReceiverAlt, lastdirDiff))
)
features <- c("close_rec_inst", "close_ball_inst", "final_distance", "distanceReceiver", "lastdirDiff")


pred_alt <- predict(model, dmat_actual)

names(AltCheck %>% select(close_rec_inst, close_ball_inst, lastdirDiff, final_distance, distanceReceiver))

print(pred_alt)

OutputCheck <- cbind(OutputCheck, pred_alt)
GraphCheck <- cbind(OutputCheck, check$pred_prob)

library(ggplot2)

prob_plot <- GraphCheck %>%
  select(frame_id.x, pred_prob = `check$pred_prob`, pred_alt) %>%
  tidyr::pivot_longer(
    cols = c(pred_prob, pred_alt),
    names_to = "path",
    values_to = "prob"
  ) %>%
  mutate(
    path = factor(path,
                  levels = c("pred_prob", "pred_alt"),
                  labels = c("Actual Path", "Alternative Path"))
  )

ggplot(prob_plot, aes(x = frame_id.x, y = prob, color = path)) +
  geom_line(size = 1.15) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Actual Path" = "#d62728" ,
                                "Alternative Path" = "#1f77b4")) +
  labs(
    title = "Probability of YAC >= 4 After Throw",
    subtitle = "Actual (Red) vs DB Goes To Ball (Blue)",
    x = "Frame",
    y = "Predicted Probability (YAC ≥ 4)",
    color = "Path"
  ) +
  theme_fivethirtyeight(base_size = 13)

p <- ggplot() +
annotate("text", 
         x = seq(40, 70, 10),
         y = 10,
         label = 10 * c(3,4,5,4),
         color = "#bebebe",
         family = "Chivo",
         size = 4) +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 40,
           label = 10 * c(3,4,5,4),
           color = "#bebebe",
           family = "Chivo",
           angle = 180,
           size = 4) +
  
  annotate("text",
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 0,
           label = "—",
           angle = 90,
           color = "#bebebe") +
  
  annotate("text",
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 160/3,
           label = "—",
           angle = 90,
           color = "#bebebe") +
geom_vline(xintercept = seq(35, 75, 5), color = "#bebebe") +
geom_point(data = filled_in,
           aes(x, y, color = player_side),
           size = 6,
           alpha = 0.95) +
    geom_point(data = filled_in %>% filter(nfl_id == 55998),
             aes(x, y),
             size = 7.5,
             color = "#FFD700",      # bright gold for visibility
             alpha = 1) +
geom_path(data = AltCheck %>% filter(defenderId.x == 47862),
          aes(defenderX.x, defenderY.x),
          color = "red",
          size = 1.2) +
geom_path(data = AltCheck,
            aes(defenderX_alt, defenderY_alt),
            color = "blue",
            linetype = "dashed",
            size = 1.1) +
geom_point(data = ball,
           aes(ball_land_x, ball_land_y),
           shape = "x",
           size = 4,
           color = "brown") +
labs(
  title = "<span style='color:#A5ACAF;'>**Las Vegas Raiders**</span> vs. 
             <span style='color:#C83803;'>**Chicago Bears**</span>, 2023 NFL Week 7",
  subtitle = sup$play_description
) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
    plot.title = ggtext::element_markdown(hjust = 0.5, size = 14),
    text = element_text(family = "", color = "#26282A"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_color_manual(values = c(
    Defense = "#C83803",
    Offense = "#A5ACAF"
  )) +
transition_time(frame_id) +
  ease_aes("linear") +
  coord_cartesian(
    xlim = c(35, 75),
    ylim = c(0, 160/3),
    expand = FALSE,
    clip = "off"
  )
anim <- animate(
  p,
  fps = 10,
  width = 900,
  height = 400,
  duration = max(filled_in$frame_id) / 10  # ensures 1 frame per actual frame_id
)

anim_save("Play.gif", animation = anim)
anim





