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

library(gganimate)
library(sportyR)
library(ggtext)

geom_football("nfl")

ggplot(one_play) +
  geom_point(aes(x,y, color = player_side))

geom_football("nfl")+
  geom_point(data = one_play, aes(x,y, color = player_side),size = 3)+
  #geom_label(data = one_play, aes(x,y, label = nfl_id))+
  transition_time(frame_id) +
  anim_save("plot.gif")




ggplot()  +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 10,
           color = "#bebebe",
           family = "Chivo",
           label = 10 * c(3,4,5,4)) +
  annotate("text", 
           x = seq(40, 70, 10),
           y = 40,
           color = "#bebebe",
           family = "Chivo",
           label = 10 * c(3,4,5,4),
           angle = 180) +
  annotate("text", 
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 0,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 160 / 3,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 23.36667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(35, 75, 1), seq(35, 75, 5)),
           y = 29.96667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  geom_vline(xintercept = seq(35, 75, 5), color = "#bebebe") +
  geom_point(data = one_play, aes(x,y,color = player_side))+
  theme_minimal() +
  labs(title = "<span style = 'color:#A5ACAF;'>**Las Vegas Raiders**</span> vs. <span style = 'color:#C83803;'>**Chicago Bears**</span>, 2023 NFL Week 7",
       subtitle = sup$play_description) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 12),
        text = element_text(family = "Chivo", color = "#26282A"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  scale_color_manual(values = c(Defense = "#C83803", Offense = "#A5ACAF"))+
  transition_time(frame_id)+
  annotate("segment", 
         x = 15,
         xend = 65,
         y = c(-Inf, Inf),
         yend = c(-Inf, Inf),
         color = "#bebebe") +
    scale_size_identity() +
    scale_fill_identity() +
    transition_time(frameId) +
    ease_aes("linear") +
    coord_cartesian(xlim = c(35, 75), ylim = c(0, 160 / 3), expand = FALSE)+
  anim_save("Play.gif")
