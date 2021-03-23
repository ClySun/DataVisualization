#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("shinydashboard")
library(shinydashboard)
library(tidyverse)
library(forcats)
library(reshape2)
library(dendextend)

library(gapminder)

library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(ggmap)
library(visNetwork)
library(igraph)

# setwd("~/Downloads/archive (1)/")
# game_details<-read.csv("~/Downloads/games_details.csv")
# write.csv(game_details[1:150000,], "~/Downloads/archive (1)/game_details1.csv", row.names = FALSE)
# write.csv(game_details[150001:300000,], "~/Downloads/archive (1)/game_details2.csv", row.names = FALSE)
# write.csv(game_details[300001:450000,], "~/Downloads/archive (1)/game_details3.csv", row.names = FALSE)
# write.csv(game_details[450001:nrow(game_details),], "~/Downloads/archive (1)/game_details4.csv", row.names = FALSE)
df1 <- read_csv("https://raw.githubusercontent.com/pwu97/nba-dataset/master/game_details1.csv")
df2 <- read_csv("https://raw.githubusercontent.com/pwu97/nba-dataset/master/game_details2.csv")
df3 <- read_csv("https://raw.githubusercontent.com/pwu97/nba-dataset/master/game_details3.csv")
df4 <- read_csv("https://raw.githubusercontent.com/pwu97/nba-dataset/master/game_details4.csv")
game_details <- rbind(df1, df2, df3, df4)

game <- read_csv("https://raw.githubusercontent.com/pwu97/nba-dataset/master/games.csv")
games_teams<-game_details %>%
  inner_join(game, by = "GAME_ID")
grouped_teams<-games_teams%>%
  group_by(SEASON, TEAM_ABBREVIATION)%>%
  summarize("TOTAL_FG3A" = sum(FG3A, na.rm = T), "TOTAL_FG2A" = 
              sum(FGA- FG3A, na.rm = T))
cor <- read_csv("https://raw.githubusercontent.com/pwu97/nba-dataset/master/uscities.csv")
cor <- data.frame(cor$city, cor$state_id)
cor <- cor[!duplicated(cor[, "cor.city"]),]
points_teams<-games_teams%>%
  group_by(SEASON, TEAM_ABBREVIATION)%>%
  summarize("Total_Points" = sum(PTS, na.rm = T))

games_map <- games_teams %>% group_by(year = substring(GAME_DATE_EST,1,4),TEAM_CITY)%>%
  summarise(n = n())%>%merge(.,cor, by.x = "TEAM_CITY", by.y = "cor.city", all.x = TRUE) %>% 
  group_by(cor.state_id, year) %>% summarize(Games_Number = sum(n))
#Cleveland Cavaliers, Miami Heat, and Los Angeles Lakers.
#MIA, LAL, CLE
#all for network building
games_network <- data.frame(games_teams$TEAM_ID, games_teams$PLAYER_NAME)
networks<- games_network %>% as.matrix(.)%>%graph_from_edgelist()

#Cleveland Cavaliers, Miami Heat, and Los Angeles Lakers.
#MIA, LAL, CLE
#mia <- games_teams$PLAYER_NAME[games_teams$TEAM_ABBREVIATION=="MIA"]
#lal <- games_teams$PLAYER_NAME[games_teams$TEAM_ABBREVIATION=="LAL"]
#cle <- games_teams$PLAYER_NAME[games_teams$TEAM_ABBREVIATION=="CLE"]
games_attr <- data.frame(name = game_details$PLAYER_NAME, FG = game_details$FG_PCT)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Graphics Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chloropleth Map", tabName = "1", icon = icon("dashboard")),
      menuItem("Network Diagram", tabName = "2", icon = icon("dashboard")),
      menuItem("PTS by Team and Season", tabName = "3", icon = icon("dashboard")),
      menuItem("Clustering NBA Plus-Minus", tabName = "4", icon = icon("dashboard")),
      menuItem("AST vs. TO by Position", tabName = "5", icon = icon("dashboard")),
      menuItem("2-pointers vs. 3-pointers", tabName = "6", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "1",
              fluidPage(
                
                bootstrapPage(
                  
                    plotlyOutput(outputId = "plot1")
                    
                )
              )      
      ),
      tabItem(tabName = "2", 
              fluidPage(
                fluidRow(
                  column(
                    width = 4,
                    selectInput("perf", "Player Field Goal Percentage Performance Level :",
                                c("low","medium","good","excellent")),
                    #selectInput("teams","Three Teams that LeBron James Played At:",
                                #c("CLE","MIA","LAL"))
                  ),
                  column(
                    width = 12,
                    HTML("<h2> LeBron James' Network Throughout Years")),
                  column(
                    width = 12,
                    visNetworkOutput("plot2", height = "400px")),
                  column(
                    width = 12,
                    HTML("<br> The graph shows all NBA players who have been on the same team with LeBron James at one point.\n
                         Every connection represents two players being on the same team. \n
                         Average Field Goals Percentage\n
                         low : 0-0.3 medium : 0.3-0.5 good: 0.5-0.7 excellent 0.7-1\n
                         LeBrone James' field goal performance was in 'good' category"))
                )
              )
      ),
      tabItem(tabName = "3", 
                  
                  selectizeInput(inputId = "team", 
                                 label = "Choose Teams", 
                                 choices = levels(grouped_teams$TEAM_ABBREVIATION) , 
                                 selected = c("ATL", "GSW", "DEN"), 
                                 multiple = TRUE,
                                 options = NULL),
                  
                  dygraphOutput(outputId = "plot3", height = "300px")
                  
      ),
      tabItem(tabName = "4", 
              fluidPage(
                
                bootstrapPage(
                  
                  selectizeInput(inputId = "prop_players_4", 
                                 label = "Proportion of Players Sampled", 
                                 choices = seq(0.01, 1, 0.01), 
                                 selected = 0.8, 
                                 options = NULL),
                  
                  plotOutput(outputId = "plot4")
                  
                )
              )  
      ),
      tabItem(tabName = "5", 
              fluidPage(
                
                bootstrapPage(
                  
                  selectizeInput(inputId = "prop_players_5", 
                                 label = "Proportion of Players Sampled", 
                                 choices = seq(0.01, 1, 0.01), 
                                 selected = 0.1, 
                                 options = NULL),
                  
                  plotlyOutput(outputId = "plot5")
                  
                )
              )  
      ),
      tabItem(tabName = "6", 
              plotlyOutput(outputId = "plot6")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$plot1 <- renderPlotly({
    g <- plot_geo(games_map, 
                  locationmode = "USA-states",
                  frame = ~year) %>%
      add_trace(locations = ~cor.state_id,
                z = ~Games_Number,
                zmin = 100,
                zmax = max(games_map['Games_Number']),
                color = ~Games_Number,
                colorscale = "Electric") %>% 
      layout(geo = list(scope = 'usa'),
             title = "Number of NBA Games in Each State\n2003 - 2019") %>%
      colorbar(title = "Number of Games") %>%
      config(displayModeBar = FALSE)
    return(g)
    
  })
  
  output$plot2 <- renderVisNetwork({
    V(networks)$type <- ifelse(V(networks)$name %in% games_network[,"games_teams.PLAYER_NAME"], TRUE, FALSE)
    networks_player <-  bipartite.projection(networks, multiplicity = TRUE, remove.type = FALSE) $proj2

    rec <- c()
    for (i in 1:vcount(networks_player)){
      perf <- filter(games_attr, games_attr$name == as.character(V(networks_player)[i]$name))
      perf <- mean(as.numeric(perf$FG),na.rm=TRUE)
      rec <- c(rec,perf)
      
    }
    
    #V(networks_player)$team <- ifelse(V(networks_player)$name %in% lal, "LAL",ifelse(V(networks_player)$name %in% cle, 
    #                                                                                 "CLE",ifelse(V(networks_player)$name %in% mia, "MIA", "NA")))
    
    V(networks_player)$FG_PCT <- rec
    V(networks_player)$perf<-as.character(cut(V(networks_player)$FG_PCT, 
                                              breaks=c(0, 0.3, 0.5, 0.7, 1), 
                                              labels=c("low", "medium", "good", "excellent")))
    
    node_of_interest = "LeBron James"
    selnodes <- V(networks_player)[which(name == node_of_interest)]
    selegoV <- ego(networks_player, order= 1, nodes = selnodes, mode = "all", mindist = 0)
    selegoG <- induced_subgraph(networks_player,unlist(selegoV))
    final <- induced.subgraph(selegoG, which(V(selegoG)$perf == input$perf))
    visIgraph(final) 
    #%>%
    #visLegend(main = "LeBron James' Network",position = "left",zoom = FALSE)
    #prof said how to set network color: set_edge_attr; eliminate duplicated connections

    
  })
    
    output$plot3 <- renderDygraph({
      react_nba <- reactive({
        data <- points_teams %>% 
          dplyr::filter(TEAM_ABBREVIATION %in%  input$team) %>% # select countries
          dcast(SEASON ~ TEAM_ABBREVIATION) # switch data structure (for insert into dygraph)
        return(data)
      })
      
      time_dygraph <- dygraph(data = react_nba(), main = "Total Points Scored by Team by Season",
                              xlab = "Season") %>%
        dyLegend(labelsSeparateLines = T, width = 80) %>%
        dyRangeSelector(height = 20) %>%
        dyAxis("y",
               label = "Total Points",
               valueRange = c(0, 22000))

      return(time_dygraph)
      
    })
    
    output$plot4 <- renderPlot({
      set.seed(7)
      quant <- game_details %>%
        select(PLUS_MINUS, REB:PTS) %>%
        na.omit() %>%
        sample_frac(as.numeric(input$prop_players_4)) %>%
        mutate(PLUS_MINUS_ROUND = round(PLUS_MINUS %/% 10) * 10) %>%
        group_by(PLUS_MINUS_ROUND) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        ungroup() %>%
        select(PLUS_MINUS_ROUND, REB:PTS)

      dend <- quant %>%  scale %>%
        dist %>% hclust %>% as.dendrogram

      res <- dend %>% set("labels", quant$PLUS_MINUS_ROUND) %>%
        set("labels_col", value = c("red", "forestgreen"), k=2) %>%
        set("branches_k_color", value = c("red", "forestgreen"), k=2) %>%
        plot(main = "Clustering NBA Plus-Minus by Traditional Box Score Stats",
             xlab = "Height", ylab = "Rounded NBA Plus-Minus", horiz = TRUE)
      
      return(res)
    })
    
    output$plot5 <- renderPlotly({
      set.seed(7)
      res <- game_details %>%
        filter(!is.na(AST)) %>%
        filter(!is.na(TO)) %>%
        filter(!is.na(START_POSITION)) %>%
        group_by(PLAYER_NAME, START_POSITION) %>%
        summarize(AST = mean(AST),
                  TO = mean(TO)) %>%
        ungroup() %>%
        filter(START_POSITION %in% c("C", "F", "G")) %>%
        sample_frac(as.numeric(input$prop_players_5)) %>%
        ggplot(aes(x = AST, y = TO, color = START_POSITION, label = PLAYER_NAME)) +
        geom_text(check_overlap = TRUE, vjust = 1, hjust = 1, size = 3) +
        geom_smooth(se = FALSE) +
        theme_bw() +
        labs(label = "Name",
             color = "Position",
             x = "Assists (Pass that leads to score) Average",
             y = "Turnover (Losing possession of ball) Average",
             title = "Assist to Turnover Ratio by Position")
      
      return(ggplotly(res))
    })
    
    output$plot6 <- renderPlotly({
      p <- ggplot(grouped_teams, 
                  aes(y = TOTAL_FG2A, x = TOTAL_FG3A,
                      label =TEAM_ABBREVIATION)) +
        geom_text(aes(frame = SEASON)) + 
        labs(x = "Total Three Pointers Attempted",
             y = "Total Two Pointers Attempted",
             title = "Total 2-Pointers Attempted vs Total 3-Pointers Attempted per Season by Team")+
        scale_y_continuous(limits = c(0,8000))+
        scale_x_continuous(limits = c(0,5000))
      
      
      p_plotly <- ggplotly(p,tooltip = "text")
      
      return(p_plotly)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
shiny::runApp(system.file("shiny", package = "visNetwork"))

