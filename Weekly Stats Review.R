library(understatr)
library(tidyverse)
library(ggrepel)
theme_set(theme_minimal())

### Getting the data

match_list1 <- as.list(14086:14087)
match_list2 <- as.list(14090:14095)

y <- NULL;
for (i in match_list1) {
  tmp <- get_match_stats(i)
  y <- rbind(y, tmp)
}

x <- NULL;
for (i in match_list2) {
  tmp <- get_match_stats(i)
  x <- rbind(x, tmp)
}

stats <- rbind(x, y)

y <- NULL;
for (i in match_list1) {
  tmp <- get_match_shots(i)
  y <- rbind(y, tmp)
}

x <- NULL;
for (i in match_list2) {
  tmp <- get_match_shots(i)
  x <- rbind(x, tmp)
}

shots <- rbind(x, y)

key_passes <- shots %>%
  filter(!is.na(player_assisted)) %>%
  group_by(player_assisted) %>%
  tally(sort = TRUE)

rm(match_list1, match_list2, tmp, x, y, i)

setwd("~/R/FPL-20-21/plots")
dir.create(paste(Sys.Date(), sep = "_"))
setwd(paste("~/R/FPL-20-21/plots/",Sys.Date(), sep = ""))

### Who had the highest xG?

stats %>%
  arrange(-xG) %>%
  select(player, xG) %>%
  top_n(10, xG)

### Who had the most shots?

shots %>%
  group_by(player) %>%
  summarise(shots = n()) %>%
  arrange(-shots) %>%
  top_n(10)

### Who assisted the most shots?

shots %>%
  filter(!is.na(player_assisted)) %>%
  group_by(player_assisted) %>%
  tally(sort = TRUE) %>%
  top_n(10)

### Who had the highest xA?

stats %>%
  arrange(-xA) %>%
  top_n(10, xA) %>%
  select(player, xA)

### Creating some plots to show xA and xG

### xA / Key Passes

merge(shots, stats, by = "player") %>%
  filter(!is.na(player_assisted)) %>%
  group_by(player) %>%
  summarise(key_passes = n(), xA = sum(xA, na.rm = TRUE)) %>%
  ggplot(aes(x = key_passes, y = xA)) +
  geom_point() +
  labs(x = "Passes leading to a shot",
       y = "Expected Assists (xA)",
       title = "Player creative contributions",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  geom_label_repel(aes(label = ifelse(key_passes >= 4 | xA >= 0.75 , player, ""))) +
  ggsave(filename = "key_passes_and_xa.png",
         height = 6,
         width = 6,
         type = "cairo-png")

### xG and shots

ggplot(stats, aes(x = shots, y = xG)) +
  geom_point() +
  labs(x = "Shots",
       y = "Expected Goals (xG)",
       title = "Player shots vs Expected Goals",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  geom_label_repel(aes(label = ifelse(shots >= 5 | xG > 0.75, player, ""))) +
  ggsave(filename = "shots_and_xg.png",
         type = "cairo-png",
         height = 6,
         width = 6)

### Creating a player and team dataframe

players <- shots %>%
  mutate(team = ifelse(h_a == "h", h_team, a_team)) %>%
  select(player, team) %>%
  distinct()

shots2 <- merge(players, shots, all.x = TRUE, by = "player")

stats2 <- merge(players, stats, all.x = TRUE, by = "player")

### Which team shot the most and had the most xG?

### xG delta

stats2 %>%
  group_by(team) %>%
  summarise(xg = sum(xG), goals = sum(goals), delta = goals-xg) %>%
  ggplot(aes(x = reorder(team, delta), y = delta)) +
  geom_col(aes(fill = delta > 0)) +
  scale_fill_manual(values = c("firebrick", "forestgreen"), c(T,F)) +
  coord_flip() +
  labs(title = "Premier League Team xG Delta GW1",
       y = "xG Delta",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  theme(axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  ggsave(filename = "Team xG Delta.png",
         type = "cairo-png",
         height = 6,
         width = 6) a

### xG and shots

shots2 %>%
  group_by(team) %>%
  summarise(shots = n(), total_xg = sum(xG)) %>%
  ggplot(aes(x = shots, y = total_xg)) +
  geom_point() +
  geom_text(aes(label = team),
            vjust = 1.5,
            position = position_jitter()) +
  labs(title = "PL team by xG and total shots taken",
       x = "Total Shots",
       y = "Expected Goals (xG)",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  ggsave(filename = "Shots vs xG Teams.png",
         type = "cairo-png",
         height = 6,
         width = 6)