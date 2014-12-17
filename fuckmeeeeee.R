storm <- read.table("repdata-data-StormData.csv.bz2", sep = ",", header=TRUE)
str(storm)
sum(is.na(storm))

library(dplyr)
storm <- select(storm,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
sort(table(storm$EVTYPE),decreasing = TRUE)

storm$EVTYPE <- tolower(storm$EVTYPE)
library(stringr)
storm$EVTYPE <-str_trim(storm$EVTYPE)

x <- grep(("avalanche"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "avalanche"}

x <- grep(("volcan"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "volcanic ash"}

x <- grep(("surf|tsunami|seiche|rip|surge"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "sea wave and current"}

x <- grep(("drought|heat|dry|fire|dust devil"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "heat and dryness"}

x <- grep(("winter storm|snow|blizzard|ice storm|sleet|fog"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "winter storm and snow"}

x <- grep(("cold|ice|freez|frozen|frost|winter weather|chill"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "winter weather"}

x <- grep(("flood|rain"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "flood and heavy rain"}

x <- grep(("hail"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "hail"}

x <- grep(("tornado|waterspout|funnel cloud"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "tornado"}

x <- grep(("hurricane|tropical"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "hurricane"}

x <- grep(("thunderstorm|tstm|lightning"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "thunderstorm"}

x <- grep(("wind|storm"),storm$EVTYPE)
for (i in x) {storm$EVTYPE[i] <- "other wind storm"}


storm$EVTYPE <- as.factor(storm$EVTYPE)




levels(stormdmg$EVTYPE)
sum(is.na(storm))

levels(storm$EVTYPE)
storm$EVTYPE <- as.factor(tolower(storm$EVTYPE))
library(stringr)
storm$EVTYPE <- as.factor(str_trim(storm$EVTYPE))
