library(tidyverse)
library(shiny)
library(DT)

# this script gets the data:
source("retrieveData.R")

############
# HERE'S THE APP CODE:
############
# UI code first
############

ui <- fluidPage(
  titlePanel("Secret NBA"),
  
  fluidRow(
    column(6,
           uiOutput("team1")
    ),
    column(6,
           uiOutput("team2")
    ), 
    column(6,
           uiOutput("team3")
    ),
    column(6,
           uiOutput("team4")
    ),
    column(6,
           uiOutput("team5")
    ),
    column(6,
           uiOutput("team6")
    ),
    column(6,
           uiOutput("team7")
    ),
    column(6,
           uiOutput("team8")
    ),
    column(6,
           uiOutput("team9")
    ),
    column(6,
           uiOutput("team10")
    ),
    column(6,
           uiOutput("team11")
    ),
    column(6,
           uiOutput("team12")
    ),
    column(6,
           uiOutput("team13")
    ), 
    column(6,
           uiOutput("team14")
    )
  )
  
)

#########
# Server code
#########

server <- function(input, output) {
  
  output$team1 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[1]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[1]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team2 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[2]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[2]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team3 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[3]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[3]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team4 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[4]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[4]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team5 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[5]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[5]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team6 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[6]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[6]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team7 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[7]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[7]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team8 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[8]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[8]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team9 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[9]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[9]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team10 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[10]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[10]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team11 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[11]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[11]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team12 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[12]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[12]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team13 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[13]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[13]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
  
  output$team14 <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == team_names[14]) %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3(team_names[14]),
        h4(
          paste0(
            "Win Shares: ", sum(tmp$`Win Shares`, na.rm = TRUE),
            ", per 48 Minutes: ",
            round((sum(tmp$`Win Shares`, na.rm = TRUE) / sum(tmp$`Minutes Played`, na.rm = TRUE)) * 48, 3)
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
