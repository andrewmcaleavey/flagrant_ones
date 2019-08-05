# Getting data 

library(tidyverse)
library(rvest)
library(googlesheets)

andy_URL <- "https://docs.google.com/spreadsheets/d/1dNJYYRy0BG3c725nN--BZkuU3uklDned7hTylkrWoBM/edit#gid=0"
andy_key <- extract_key_from_url(andy_URL)
andy_data <- andy_URL %>%
  gs_url()%>% 
  gs_read(ws = "2019 Draft", 
          col_names = TRUE, 
          stringsAsFactors = FALSE) # doesn't seem to work

team_names <- unlist(unname(andy_data[, 1]))

players <- data.frame(t(andy_data[, 2:11])) %>% 
  # set_names(unlist(team_names)) %>% 
  mutate_all(.funs = as.character) %>%  # this makes the factors characters again.
  gather(value = "Player", 
         key = "Team") %>% 
  mutate(Team = rep(team_names, each = 10)) %>%   # adds team names
  dplyr::select(Player, Team)

# NOTE: need to harmonize the player names to see if anyone *cough andy* misspelled any player names
source("andy_cannot_spell.R")
players$Player <-  andy_cannot_spell(players$Player)

# Get data from Basketball Reference
# Will update for 2019-2020
dat0 <- "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html" %>%
  read_html() %>% 
  html_table() %>% 
  getElement(1) 

# Modify by mostly filtering out undrafted players
dat <- dat0[, c("Player", "Pos", "MP", "WS")] %>% 
  right_join(players, by = "Player") %>%  # this takes only players in the Secret NBA
  # Drops the rookies though, since they don't show up in the data yet
  group_by(Player) %>% 
  mutate(MP = as.numeric(MP), 
         WS = as.numeric(WS)) %>% 
  replace_na(list(Player = "Empty Spot", 
                  Pos = "Future King",
                  MP = 0, 
                  WS = 0)) %>% 
  filter(MP == max(MP)) %>%   # takes just the row with the most minutes per player
  ungroup() %>% 
  transmute(
    Player = Player,
    Team = Team,
    Position = Pos, 
    MP = MP,
    WS = WS
  ) %>% 
  arrange(Team, desc(WS))
