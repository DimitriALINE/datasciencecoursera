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


listplayer <- function(team,position){
  team <- as.character(team)
  position <- as.character(position)
  player <- NBA[(NBA$Team == team) & (NBA$Position == position),]  
  player <- player$Name  
  player  
}

tablestat1 <- function(player){
player <- as.character(player)  
playerstats <- NBA[(NBA$Name == player),c(2,5,6,7,8) ]
names(playerstats)[3] <- "Minutes"
playerstats
}

tablestat2 <- function(player){
  player <- as.character(player)  
  playerstats <- NBA[(NBA$Name == player),c(2,9,10,11,12) ]
  playerstats
}

tablestat3 <- function(player){
  player <- as.character(player)  
  playerstats <- NBA[(NBA$Name == player),c(2,14,15,16,17,18,21) ]
  names(playerstats)[2] <- "Rebounds"
  names(playerstats)[7] <- "Points"
  playerstats
}


shinyServer(
  function(input, output) {
    
    output$listplayer <- renderUI({
                              player <- listplayer(input$Team,input$Position)
                              selectInput("Player",h5("Choose a Player"),player) 
                                   })
    ##Table
    output$table1 <- renderTable({tablestat1(input$Player)},include.rownames =FALSE)
    output$table2 <- renderTable({tablestat2(input$Player)},include.rownames =FALSE)
    output$table3 <- renderTable({tablestat3(input$Player)},include.rownames =FALSE)
     
    ##Graphics
    output$plot <- renderPlot({
     if  (is.na(tablestat3(input$Player)[1,7]) == FALSE) {
     datap<- c(tablestat3(input$Player)[1,7],tablestat3(input$Player)[1,3],tablestat3(input$Player)[1,2],tablestat3(input$Player)[1,4],tablestat3(input$Player)[1,6],tablestat3(input$Player)[1,5])
     datap<- as.numeric(datap)
     datap2<- c(tablestat1(input$Player)[1,4],tablestat2(input$Player)[1,2],tablestat2(input$Player)[1,4],(tablestat1(input$Player)[1,5]-tablestat1(input$Player)[1,4]),(tablestat2(input$Player)[1,3]-tablestat2(input$Player)[1,2]),(tablestat2(input$Player)[1,5])-tablestat2(input$Player)[1,4]) 
     datap3<- c("Made","Made","Made","Missed","Missed","Missed")
       
     df<- data.frame(cat =  factor(c("Points","Assists","Rebounds","Steals","Blocks","Turnover"),
       levels=c("Points","Assists","Rebounds","Steals","Blocks","Turnover")),stats=datap)
      
     df2 <- data.frame(cat =  factor(c("FieldGoal","Threes","FreeThrows"),
       levels=c("FieldGoal","Threes","FreeThrows")),stats=datap2,Attempted = datap3)
     
   
      g <- ggplot(data=df,aes(x = cat , y = stats,fill=cat)) + 
       geom_bar(stat = "identity") +  ylim(c(0,32.02)) + 
      ylab("")+xlab("")+scale_fill_hue(name="") + 
      ggtitle("General Stats")
     
     
      h <-ggplot(data=df2,aes(x=cat,y=stats,fill=Attempted))+ geom_bar(stat="identity") +
       ggtitle("Shooting Stats") +ylab("")+xlab("") +ylim(c(0,21.4))
      if (input$graph == "General Stats"){g}
       else{h}
     } else{}
   })
    
  
  }
) 

