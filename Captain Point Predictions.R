library(tidyverse)
library(fplscrapR)
theme_set(theme_light())

df<-get_player_details()

player_points <- df %>%
  group_by(playername, round) %>%
  select(playername, round, total_points, minutes)

last4stats <- player_points %>%
  group_by(playername) %>%
  filter(minutes > 0) %>%
  top_n(4, round) %>%
  group_by(playername) %>%
  mutate(sd.samp = sd(total_points),
         sd.pop = sd.samp*sqrt(length(total_points)-1)/length(total_points),
         me = 1.644*(sd.samp/sqrt(38)),
         lower = mean(total_points - me),
         upper = mean(total_points + me),
         mean = mean(total_points)) %>%
  summarise(lower = mean(lower), upper = mean(upper), mean = mean(mean), minutes = mean(minutes))

last8stats <- player_points %>%
  group_by(playername) %>%
  filter(minutes > 0) %>%
  top_n(8, round) %>%
  group_by(playername) %>%
  mutate(sd.samp = sd(total_points),
         sd.pop = sd.samp*sqrt(length(total_points)-1)/length(total_points),
         me = 1.644*(sd.samp/sqrt(38)),
         lower = mean(total_points - me),
         upper = mean(total_points + me),
         mean = mean(total_points)) %>%
  summarise(lower = mean(lower), upper = mean(upper), mean = mean(mean), minutes = mean(minutes))

last4stats %>%
  top_n(15, mean) %>%
  ggplot(aes(x = reorder(playername, mean), y = mean)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, colour = minutes), width=.5, size = 1) +
  geom_point(colour = "black") +
  coord_flip() +
  expand_limits(y = 0) +
  labs(title = "Point Prediction Range - Last 4 Matches",
       caption = "Top 15 Players by Mean Score over last 4 matches",
       y = "Mean Points Last 4",
       x = "Player",
       colour = "Avg. Starting Mins") +
  scale_colour_viridis_c(direction = -1) +
  ggsave(path = "Plots", filename = paste0("last_four_points_", format(Sys.time(),"%Y-%m-%d"), ".png"), type = "cairo-png", dpi = 300, height = 5, width = 9)

last8stats %>%
  top_n(15, mean) %>%
  ggplot(aes(x = reorder(playername, mean), y = mean)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, colour = minutes),width=.5, size = 1) +
  geom_point(colour = "black") +
  coord_flip() +
  expand_limits(y = 0) +
  scale_colour_viridis_c(direction = -1) +
  labs(title = "Point Prediction Range - Last 8 Matches",
       caption = "Top 15 Players by Mean Score over last 8 matches",
       y = "Mean Points Last 8",
       x = "Player",
       colour = "Avg. Starting Mins") +
  ggsave(path = "Plots", filename = paste0("last_eight_points_", format(Sys.time(),"%Y-%m-%d"), ".png"), type = "cairo-png", dpi = 300, height = 5, width = 9)
