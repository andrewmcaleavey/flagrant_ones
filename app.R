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
           uiOutput("pinchy")
    ),
    column(6,
           uiOutput("bats")
    ), 
    column(6,
           uiOutput("shirts")
    ),
    column(6,
           uiOutput("PBR")
    ),
    column(6,
           uiOutput("Leopards")
    ),
    column(6,
           uiOutput("Kennedys")
    ),
    column(6,
           uiOutput("Spongebobs")
    ),
    column(6,
           uiOutput("Tables")
    ),
    column(6,
           uiOutput("Bozos")
    ),
    column(6,
           uiOutput("Carters")
    ),
    column(6,
           uiOutput("Megs")
    ),
    column(6,
           uiOutput("Frances")
    ),
    column(6,
           uiOutput("Lawyers")
    )
  )
  
)

#########
# Server code
#########

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
  
  output$PBR <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "St. Louis PBR Tallboys") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("St. Louis PBR Tallboys"),
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
  
  output$Leopards <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Kauai Leopards") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Kauai Leopards"),
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
  
  output$Kennedys <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Kennebunkport Kennedys") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Kennebunkport Kennedys"),
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
  
  output$Spongebobs <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "Bikini Bottom Spongebobs") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("Bikini Bottom Spongebobs"),
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
  
  output$Tables <- renderUI({
    if (!is.null(dat)) {
      tmp <- dat %>% 
        filter(Team == "My House Tables") %>% 
        mutate(`Win Shares` = WS, 
               `Minutes Played` = MP) %>% 
        select(-Team, -WS, -MP) %>% 
        arrange(desc(`Win Shares`))
      
      tagList(
        h3("My House Tables"),
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
  
  output$Bozos <- renderUI({
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
  
  output$Carters <- renderUI({
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
  
  output$Megs <- renderUI({
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
  
  output$Frances <- renderUI({
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
  
  output$Lawyers <- renderUI({
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
}

shinyApp(ui = ui, server = server)
