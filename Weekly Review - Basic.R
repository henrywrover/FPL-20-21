library(understatr)
library(tidyverse)
library(ggrepel)
library(hrbrthemes)
theme_set(theme_minimal())

### Getting the data

  ### Use this if just one sequence of matches (i.e. unbroken)
  
match_list <- as.list(14106:14115)

stats <- NULL;
for (i in match_list) {
  tmp <- get_match_stats(i)
  stats <- rbind(stats, tmp)
}

shots <- NULL;
for (i in match_list) {
  tmp <- get_match_shots(i)
  shots <- rbind(shots, tmp)
}

rm(tmp, i, match_list)

setwd("~/R/FPL 20-21/Weekly Review")
dir.create(paste(Sys.Date()))
setwd(paste("~/R/FPL 20-21/Weekly Review/", Sys.Date(), sep = ""))

### Non-penalty xG and shots

shots %>%
  filter(situation != "Penalty") %>%
  group_by(player) %>%
  summarise(xG = round(sum(xG), 2),
            shots = n()) %>%
  arrange(-xG) %>%
  top_n(10, xG) %>%
  as.data.frame() %>%
  write_csv("shots_xg_table.csv")
  
### xA and Key Passes

stats %>%
  mutate(xA = as.numeric(as.character(xA))) %>%
  top_n(10, xA) %>%
  select(player, xA, key_passes) %>%
  mutate(xA = round(xA, 2)) %>%
  arrange(-xA) %>%
  as.data.frame() %>%
  write_csv("key_passes_xa_table.csv")

### Creating some plots to show xA and xG

### xA / Key Passes

stats %>%
  mutate(xA = as.numeric(as.character(xA)),
         key_passes = as.numeric(as.character(key_passes)),
         player = as.character(player)) %>%
  ggplot(aes(x = key_passes, y = xA)) +
  geom_point() +
  labs(x = "Key Passes",
       y = "Expected Assists (xA)",
       title = "Player creative contributions",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  theme_ft_rc() +
  geom_text_repel(aes(label = ifelse(key_passes >= 4 | xA >= 0.75 , player, "")),
                  colour = "grey50") +
  ggsave(filename = "key_passes_and_xa.png",
         height = 6,
         width = 6,
         type = "cairo-png")

### xG and shots

shots %>%
  filter(situation != "Penalty") %>%
  group_by(player) %>%
  summarise(xG = round(sum(xG), 2),
            shots = n()) %>%
  mutate(xG = as.numeric(as.character(xG)),
         shots = as.numeric(shots),
         player = as.character(player)) %>%
  ggplot(aes(x = shots, y = xG)) +
  geom_point() +
  labs(x = "Total Shots",
       y = "Expected Goals (xG)",
       title = "Player shots vs Expected Goals",
       subtitle = "xG Value does not include penalties",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  theme_ft_rc() +
  geom_text_repel(aes(label = ifelse(xG > 0.75 | shots > 5, player, "")),
                  colour = "grey50") +
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

### xG and shots

shots2 %>%
  group_by(team) %>%
  filter(situation != "Penalty") %>%
  summarise(shots = n(), total_xg = sum(xG)) %>%
  ggplot(aes(x = shots, y = total_xg)) +
  geom_point() +
  geom_text(aes(label = team),
            vjust = 1.5,
            position = position_jitter()) +
  theme_ft_rc() +
  labs(title = "PL team by xG and total shots taken",
       subtitle = "xG Value does not include penalties",
       x = "Total Shots",
       y = "Expected Goals (xG)",
       caption = paste("Twitter: @henrywrover2\n", Sys.Date())) +
  ggsave(filename = "Shots vs xG Teams.png",
         type = "cairo-png",
         height = 6,
         width = 6)
