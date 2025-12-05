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

