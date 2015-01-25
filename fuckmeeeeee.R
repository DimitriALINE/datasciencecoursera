storm <- read.table("repdata-data-StormData.csv.bz2", sep = ",", header=TRUE)

library(dplyr)
storm <- select(stormbas,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,REMARKS)

storm$EVTYPE <- tolower(storm$EVTYPE)
library(stringr)
storm$EVTYPE <-str_trim(storm$EVTYPE)


x <- grep(("landslide|mudslide|mud|volcan|avalanche"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "landslide, avalanche or volcanic ash"}

x <- grep(("surf|tsunami|seiche|rip|surge|tide"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "sea wave and current"}

x <- grep(("drought|heat|dry|fire|dust|warmth|warm|record"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "heat and dryness"}

x <- grep(("winter storm|snow|blizzard|ice storm|sleet"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "winter storm and snow"}

x <- grep(("cold|ice|freez|frozen|frost|winter weather|chill|wintry|icy|glaze|low temperature|fog"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "cold and other winter weather"}

x <- grep(("flood|fld"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "flood"}

x <- grep(("rain|precipitation|precip"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "rain"}

x <- grep(("hail"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "hail"}

x <- grep(("tornado|waterspout|funnel"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "tornado"}

x <- grep(("hurricane|tropical|typhoon"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "hurricane"}

x <- grep(("thunderstorm|tstm|lightning"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "thunderstorm"}

x <- grep(("wind|winds"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "other wind storm"}

storm$EVTYPE <- as.factor(storm$EVTYPE)

storm <- filter(storm,EVTYPE == "other wind storm" | EVTYPE == "thunderstorm" | EVTYPE == "hurricane" | EVTYPE == "tornado" | EVTYPE == "hail" | EVTYPE == "rain"| EVTYPE == "flood" | EVTYPE == "cold and other winter weather" | EVTYPE == "winter storm and snow" | EVTYPE == "heat and dryness" | EVTYPE == "sea wave and current"| EVTYPE == "landslide, avalanche or volcanic ash")
sort(table(storm$EVTYPE))

stormhum <- filter(storm,FATALITIES != 0 | INJURIES != 0)
stormdmg <- filter(storm,PROPDMG != 0 | CROPDMG != 0)
stormhum <- select(stormhum,EVTYPE,FATALITIES,INJURIES,REMARKS)
stormdmg<- select(stormdmg,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,REMARKS)

stormdmg$PROPDMGEXP <- tolower(stormdmg$PROPDMGEXP)
stormdmg$CROPDMGEXP <- tolower(stormdmg$CROPDMGEXP)
n <- length(stormdmg$PROPDMG)
table(stormdmg$PROPDMG)
stormext <- filter(stormdmg,PROPDMG > 999)
table(stormdmg$CROPDMG)
for (i in 1:n) {
                if (stormdmg$PROPDMG[i] > 999) {stormdmg$PROPDMG[i] <- stormdmg$PROPDMG[i] / 1000}
}
for (i in 1:n) {
  if (stormdmg$CROPDMG[i] > 999) {stormdmg$CROPDMG[i] <- stormdmg$CROPDMG[i] / 1000}
}

x <- grep(("k"),stormdmg$CROPDMGEXP)
for (i in x){stormdmg$CROPDMG[i] <- stormdmg$CROPDMG[i] * 1000}

x <- grep(("m"),stormdmg$CROPDMGEXP)
for (i in x){stormdmg$CROPDMG[i] <- stormdmg$CROPDMG[i] * 1000000}

x <- grep(("b"),stormdmg$CROPDMGEXP)
for (i in x){stormdmg$CROPDMG[i] <- stormdmg$CROPDMG[i] * 1000000000}

x <- grep(("k"),stormdmg$PROPDMGEXP)
for (i in x) {stormdmg$PROPDMG[i] <- stormdmg$PROPDMG[i] * 1000}

x <- grep(("m"),stormdmg$PROPDMGEXP)
for (i in x){stormdmg$PROPDMG[i] <- stormdmg$PROPDMG[i] * 1000000}

x <- grep(("b"),stormdmg$PROPDMGEXP)
for (i in x){stormdmg$PROPDMG[i] <- stormdmg$PROPDMG[i] * 1000000000}

table(stormdmg$PROPDMG)

stormext <- filter(stormdmg,PROPDMG > 10000000000)
View(stormext)
stormdmg$PROPDMG[stormdmg$PROPDMG == 1.150e+11] <- 115000000
table(stormdmg$CROPDMG)
stormext <- filter(stormdmg,CROPDMG > 500000000)
View(stormext)

stormhum <- summarize(group_by(stormhum,EVTYPE),sum(INJURIES),sum(FATALITIES))

stormdmg <- summarize(group_by(stormdmg,EVTYPE),sum(PROPDMG),sum(CROPDMG))

colnames(stormhum) <- c("EVENTS","INJURIES","FATALITIES")
colnames(stormdmg) <- c("EVENTS","PropertyDamage","CropDamage")
stormdmg <- mutate(stormdmg,PropertyDamage = PropertyDamage / 1000000, CropDamage = CropDamage / 1000000)
x <- summarize(group_by(storm,EVTYPE),n())
stormresult1 <- cbind(stormhum,stormdmg$PropertyDamage,stormdmg$CropDamage,x[2])
colnames(stormresult1) <- c("Weather Event","Injuries","Fatalities","PropertyDamage","CropDamage","NumberEvents")



stormresult2 <- stormresult1

stormresult2 <- mutate(stormresult1,Injuries = Injuries/sum(Injuries)*100,NumberEvents = NumberEvents/sum(NumberEvents)*100,Fatalities = Fatalities/sum(Fatalities)*100,PropertyDamage = PropertyDamage/sum(PropertyDamage)*100,CropDamage = CropDamage/sum(CropDamage)*100)
stormresult1[4] <- round(stormresult1[4],2)
stormresult1[5] <- round(stormresult1[5],2)

stormresult2[2] <- round(stormresult2[2],2)
stormresult2[3] <- round(stormresult2[3],2)
stormresult2[4] <- round(stormresult2[4],2)
stormresult2[5] <- round(stormresult2[5],2)
stormresult2[6] <- round(stormresult2[6],2)
colnames(stormresult1) <- c("WeatherEvent","Injuries","Fatalities","PropertyDamage","CropDamage","NumberEvents")
colnames(stormresult2) <- c("WeatherEvent","Injuries","Fatalities","PropertyDamage","CropDamage","NumberEvents")

library("ggplot2")
library("grid")
library("gridExtra")
library("RColorBrewer")
pal <- brewer.pal(12,"Set3")


p1 = ggplot(stormresult2, aes( x= factor(1), y = NumberEvents, fill = factor(WeatherEvent))) + geom_bar(width = 1,stat="identity",colour = "white") + 
  coord_polar(theta = "y") + scale_fill_manual(values = pal) + labs(fill="Weather Event") + xlab("") + ylab("Number of Events") + 
  theme(axis.ticks=element_blank(),axis.text.y=element_blank()) + scale_y_continuous(breaks=cumsum(stormresult2$NumberEvents) - stormresult2$NumberEvents/2,labels=stormresult1$NumberEvents)
p2 = ggplot(stormresult2, aes( x= factor(1), y = Injuries, fill = factor(WeatherEvent))) + geom_bar(width = 1,stat="identity",colour = "white") + 
  coord_polar(theta = "y") + scale_fill_manual(values = pal) + labs(fill="Weather Event") + xlab("") + ylab("Injuries") + 
  theme(axis.ticks=element_blank(),axis.text.y=element_blank()) + scale_y_continuous(breaks=cumsum(stormresult2$Injuries) - stormresult2$Injuries/2,labels=stormresult1$Injuries)
p3 = ggplot(stormresult2, aes( x= factor(1), y = Fatalities, fill = factor(WeatherEvent))) + geom_bar(width = 1,stat="identity",colour = "white") + 
  coord_polar(theta = "y") + scale_fill_manual(values = pal) + labs(fill="Weather Event") + xlab("") + ylab("Fatalities") + 
  theme(axis.ticks=element_blank(),axis.text.y=element_blank()) + scale_y_continuous(breaks=cumsum(stormresult2$Fatalities) - stormresult2$Fatalities/2,labels=stormresult1$Fatalities)
p4 = ggplot(stormresult2, aes( x= factor(1), y = PropertyDamage, fill = factor(WeatherEvent))) + geom_bar(width = 1,stat="identity",colour = "white") + 
  coord_polar(theta = "y") + scale_fill_manual(values = pal) + labs(fill="Weather Event") + xlab("") + ylab("Property Damage (M$)") + 
  theme(axis.ticks=element_blank(),axis.text.y=element_blank()) + scale_y_continuous(breaks=cumsum(stormresult2$PropertyDamage) - stormresult2$PropertyDamage/2,labels=stormresult1$PropertyDamage)
p5 = ggplot(stormresult2, aes( x= factor(1), y = CropDamage, fill = factor(WeatherEvent))) + geom_bar(width = 1,stat="identity",colour = "white") + 
  coord_polar(theta = "y") + scale_fill_manual(values = pal) + labs(fill="Weather Event") + xlab("") + ylab("Crop Damage (M$)") + 
  theme(axis.ticks=element_blank(),axis.text.y=element_blank()) + scale_y_continuous(breaks=cumsum(stormresult2$CropDamage) - stormresult2$CropDamage/2,labels=stormresult1$CropDamage)

p2 = grid.arrange(p2, p3, ncol = 2, main = "Human Damage")

p3 = grid.arrange(p4, p5, ncol = 2, main = "Material Damage")

p1
p2
p3
stormresult1
stormresult2