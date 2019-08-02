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

team_names <- unname(andy_data[, 1])

rosters <- data.frame(t(andy_data[, 2:11])) %>% 
  # set_names(unlist(team_names)) %>% 
  mutate_all(.funs = as.character)  # this makes the factors characters again.

# need to correct typos
source("andy_cannot_spell.R")
rosters <- data.frame(lapply(rosters, andy_cannot_spell))

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


pinchy_crabs <- read_yaml("teams.yml")$pinchy_crabs
bats <- read_yaml("teams.yml")$bats

dat0 <- "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html" %>%
  read_html() %>% 
  html_table() %>% 
  getElement(1) 

dat <- dat0[, c("Player", "Pos", "MP", "WS")] %>% 
  filter(Player %in% unlist(rosters)) %>%  # this takes only players in the Secret NBA
  # NOTE: need to harmonize the player names to see if anyone *cough andy* misspelled any player names
  group_by(Player) %>% 
  arrange(desc(MP)) %>%
  slice(1) %>%  # takes just the first row for each player, which has all their positions
  ungroup() %>% 
  transmute(
    Player = Player,
    Position = Pos,
    `Minutes Played` = as.numeric(MP),
    `Win Shares` = as.numeric(WS)
  )
# need a way to track player teams. Why am I blanking? 

ui <- fluidPage(
  
  fluidRow(
    column(6,
      uiOutput("pinchy")
    ),
    column(6,
      uiOutput("bats")
    )
  )
   
)

server <- function(input, output) {
   
  output$pinchy <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Pinchy Crabs") %>% 
        select(-Team) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Baltimore Pinchy Crabs"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`) / sum(tmp$`Minutes Played`) * 48, 3)
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
        filter(Team == "Ballin' Bats") %>% 
        select(-Team) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Austin Ballin' Bats"),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`),
            ", per 48 Minutes: ",
            round(sum(tmp$`Win Shares`) / sum(tmp$`Minutes Played`) * 48, 3)
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
