---
title       : NBA Individual Statistics App
subtitle    : 
author      : Dimitri Aline
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Two steps

# 1. Choose Player

 Easy to choose a player by selecting their team and position.
 
# 2. Show individual Stats

 Show Complete Stats in a table

 Illustrate stats with graphics
  1. General Stats
  2. Shooting Stats

--- 

## Choose Player

The app uses the following function named listplayer.
This function give the complete list of the players with a given Team and a given Position. 
The Team and Position are the two arguments of this function.


```r
listplayer <- function(team,position){
  team <- as.character(team)
  position <- as.character(position)
  player <- NBA[(NBA$Team == team) & (NBA$Position == position),]  
  player <- player$Name  
  player  
}
```
Here, you can see an exemple with the team Boston and the position Point Guard.

```r
listplayer("BOS","PG")
```

```
## [1] "Jerryd Bayless" "Avery Bradley"  "Phil Pressey"   "Rajon Rondo"
```

---

## Show individual Stats

Here you can see an exemple of the general stat plot with the player Rajon Rondo.

![plot of chunk unnamed-chunk-4](assets/fig/unnamed-chunk-4-1.png) 

---

## Improvements in future

Possibility to choose the season

Include team stats

Include average stats





