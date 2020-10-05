library(tidyverse)
library(understatr)
library(ggsoccer)
library(hrbrthemes)
library(RColorBrewer)
library(patchwork)

match_list <- as.list(c(14465:14474))

star_player <- "James Rodríguez" 
player <- get_team_players_stats(team_name = "Everton", year = 2020)

shots <- NULL;
for (i in match_list) {
  tmp <- get_match_shots(i)
  shots <- rbind(shots, tmp)
}

stats <- NULL;
for (i in match_list) {
  tmp <- get_match_stats(i)
  stats <- rbind(stats, tmp)
}

rm(tmp)

shotmap <- shots %>%
  filter(player == star_player) %>%
  mutate(X = X*100, Y = Y*100) %>%
  ggplot() +
  annotate_pitch(colour = "grey80",
                 fill = "#252a32") +
  geom_point(aes(x = X, y = Y, size = xG, colour = result),alpha = 0.9) +
  labs(title = "Shot Map",
       colour = "Shot Outcome") +
  scale_colour_manual(values = c("#274488", "#FFFFFF", "firebrick", "grey40")) +
  theme_ft_rc() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  scale_size_continuous(range = c(3,10)) +
  scale_y_reverse() +
  coord_flip(xlim = c(65,105))

minutes <- as.data.frame(1:90) %>%
  rename(minute = "1:90") %>%
  mutate(cumulative_xg = ifelse(minute == 1, 0, NA))

cumulative_xg <- shots %>%
  filter(player == star_player) %>%
  mutate(cumulative_xg = cumsum(xG)) %>%
  select(minute, result, cumulative_xg) %>%
  merge(minutes, all = TRUE) %>%
  fill(cumulative_xg) %>%
  ggplot(aes(x = minute, y = cumulative_xg)) +
  geom_line(size = 1.6, colour = "grey70") +
  geom_point(aes(x = minute, y = cumulative_xg, colour = result), size = 4) +
  theme_ft_rc() +
  scale_x_continuous(limits = c(0,90)) +
  scale_colour_manual(values = c("#274488", "#FFFFFF", "firebrick", "orange"),
                      breaks = c("BlockedShot", "Goal", "SavedShot", "MissedShots")) +
  labs(title = "Cumulative xG",
       x = "Minute of Match",
       y = "Cumulative Expected Goals",
       colour = "Shot Outcome")

scatter_shots <- stats %>%
  mutate(shots = as.numeric(as.character(shots)), xG = as.numeric(as.character(xG))) %>%
  ggplot() +
  geom_point(aes(x = shots, y = xG)) +
  geom_point(data = stats %>%
               filter(player == star_player) %>%
               mutate(shots = as.numeric(as.character(shots)),
                      xG = as.numeric(as.character(xG))),
             aes(x = shots, y = xG),
             colour = "firebrick",
             size = 5) +
  labs(title = "Shots and xG compared to other players this weekend",
       x = "Shots",
       y = "Expected Goals (xG)") +
  theme_ft_rc()

team_stats <- player %>%
  select(player_name, shots, key_passes) %>%
  filter(shots + key_passes > 0) %>%
  top_n(8, shots+key_passes) %>%
  rename("Key Passes" = key_passes,
         "Shots" = shots) %>%
  pivot_longer(cols = c("Shots", "Key Passes")) %>%
  ggplot(aes(x = value, y = reorder(player_name, value))) +
  geom_col(aes(fill = name), position = position_dodge()) +
  theme_ft_rc() +
  scale_fill_manual(values = c("#274488", "#FFFFFF", "firebrick")) +
  labs(title = "Everton season stats",
       x = "",
       y = "Player",
       fill = "Type")

  final_plot <- (cumulative_xg + shotmap) / (scatter_shots + team_stats) +
  plot_annotation(title = paste(star_player, " Match Review - Everton vs Brighton - 3rd October 2020"),
                  caption = "Twitter | @henrywrover2\n GitHub | henrywrover\n 5th October 2020") &
  theme(plot.background = element_rect(fill = "#252a32"),
        plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  colour = "grey90"),
        plot.caption = element_text(size = 6,
                                    colour = "grey90")) 
  
  setwd("~/R/FPL 20-21/Star Player")
  ggsave(final_plot, filename = paste(star_player, " - ", Sys.Date(), ".png", sep = ""), dpi = 300, height = 9, width = 14, type = "cairo-png")
  