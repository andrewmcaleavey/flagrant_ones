library(shiny)
library(DT)
library(tidyverse)
library(rvest)
library(yaml)
library(googlesheets)

dat <- NULL # initialize

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

# need to correct typos
source("andy_cannot_spell.R")
players$Player <-  andy_cannot_spell(players$Player)

# this is just a backup data set, maintained by Andrew McAleavey (and not intended to last)
# totally not necessary
andrew_URL <- "https://docs.google.com/spreadsheets/d/1dLKu1jKvbvWEt_nSG2kP22A-kfQuV_TGYz-19lbJhFI/edit?usp=sharing"
andrew_data <- andrew_URL %>%
  gs_url()
gs_ws_ls(andrew_data)
andrew_data <- andrew_URL %>%
  gs_url() %>% 
  gs_read(ws = "Sheet1", 
          col_names = TRUE, 
          stringsAsFactors = FALSE) 

#legacy:
# pinchy_crabs <- read_yaml("teams.yml")$pinchy_crabs
# bats <- read_yaml("teams.yml")$bats

# Get data from Basketball Reference
# Will update for 2019-2020
dat0 <- "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html" %>%
  read_html() %>% 
  html_table() %>% 
  getElement(1) 

# Modify by mostly filtering out undrafted players
dat <- dat0[, c("Player", "Pos", "MP", "WS")] %>% 
  right_join(players, by = "Player") %>%  # this takes only players in the Secret NBA
  # NOTE: need to harmonize the player names to see if anyone *cough andy* misspelled any player names
  group_by(Player) %>% 
  mutate(MP = as.numeric(MP), 
         WS = as.numeric(WS)) %>% 
  filter(MP == max(MP)) %>%   # takes just the first row for each player, which has all their positions
  ungroup() %>% 
  transmute(
    Player = Player,
    Team = Team,
    Position = Pos, 
    MP = MP,
    WS = WS
  ) %>% 
  arrange(Team, desc(WS))
# need a way to track player teams. Why am I blanking? 

############
# HERE'S THE APP CODE:
############

ui <- fluidPage(
  
  fluidRow(
    column(6,
           uiOutput("pinchy")
    ),
    column(6,
           uiOutput("bats")
    ), 
    column(6,
           uiOutput("shirts")
    )
  )
  
)

server <- function(input, output) {
  
  output$pinchy <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Baltimore Pinchy Crabs (Sean)") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Baltimore Pinchy Crabs"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE) * 48, 3)
          )
        ),
        renderDataTable(
          datatable(
            tmp,
            rownames = FALSE,
            options = list(
              paging = FALSE,
              searching = FALSE,
              bInfo = FALSE
            )
          )
        )
      )
    }
  })
  
  output$bats <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Austin Ballin' Bats (Carl)") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Austin Ballin' Bats"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE) * 48, 3)
          )
        ),
        renderDataTable(
          datatable(
            tmp,
            rownames = FALSE,
            options = list(
              paging = FALSE,
              searching = FALSE,
              bInfo = FALSE
            )
          )
        )
      )
    }
  })
  
  output$shirts <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Pittsburgh Shirts (Hayes)") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Pittsburgh Shirts"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE) * 48, 3)
          )
        ),
        renderDataTable(
          datatable(
            tmp,
            rownames = FALSE,
            options = list(
              paging = FALSE,
              searching = FALSE,
              bInfo = FALSE
            )
          )
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
