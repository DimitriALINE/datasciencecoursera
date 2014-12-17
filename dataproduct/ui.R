library(shiny)
library(ggplot2)
library("SportsAnalytics")
NBA <- fetch_NBAPlayerStatistics(season = "13-14")
NBA$Team <- as.vector(NBA$Team)
NBA$Position <- as.vector(NBA$Position)
NBA[,6] <- NBA[,6]/NBA[,5]
NBA[,7] <- NBA[,7]/NBA[,5]
NBA[,8] <- NBA[,8]/NBA[,5]
NBA[,9] <- NBA[,9]/NBA[,5]
NBA[,10] <- NBA[,10]/NBA[,5]
NBA[,11] <- NBA[,11]/NBA[,5]
NBA[,12] <- NBA[,12]/NBA[,5]
NBA[,14] <- NBA[,14]/NBA[,5]
NBA[,15] <- NBA[,15]/NBA[,5]
NBA[,16] <- NBA[,16]/NBA[,5]
NBA[,17] <- NBA[,17]/NBA[,5]
NBA[,18] <- NBA[,18]/NBA[,5]
NBA[,21] <- NBA[,21]/NBA[,5]


shinyUI(pageWithSidebar(
  titlePanel(" 2013-2014 Nba regular season individual statistics"),
  sidebarPanel(
            selectInput("Team",h5("Choose a Team"),sort(NBA$Team)),
            
            selectInput("Position",h5("Choose a Position"),sort(NBA$Position)),
            br(),
            submitButton("Display player choice"),
            
            br(),
            br(),
           
            uiOutput("listplayer"),
           
            radioButtons("graph",h5("Choose a plot"),choices = c("General Stats","Shooting Stats")),
            br(),
            submitButton("Display player Stats"),
            br(),
            wellPanel(
            helpText(  
            p("The database comes from the SportsAnalytics package."),
            a("SportsAnalytics information here.", href="http://cran.r-project.org/web/packages/SportsAnalytics/SportsAnalytics.pdf"))),
            width = 3),
         
          
  mainPanel(      p("All statistics are in average per game except for GamesPlayed."),
                  tableOutput("table1"),
                  tableOutput("table2"),
                  tableOutput("table3"),
                  plotOutput("plot",width =720),
                  width = 9
                  
  ))
  )

