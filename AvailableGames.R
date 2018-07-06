library(XML)
library(httr)
library(RCurl)
library(dplyr)
library(stringr)
library(lubridate)

setwd("ModernBoxScore/")

getMonthsByYear <- function(year){
     monthslevelpage <- read_html(paste0("http://gd2.mlb.com/components/game/mlb/year_", year)) %>% as_list()
     months <- monthslevelpage$body$ul %>% as.data.frame() %>% t() %>% as.data.frame() %>%
          filter(substr(V1, 2, 6) == "month") %>% mutate(month = str_sub(V1, 2, -1)) %>% select(month) %>% unlist(use.names = F)
}

getDaysByMonth <- function(year, month){
     dayslevelpage <- read_html(paste0("http://gd2.mlb.com/components/game/mlb/year_", year, "/", month)) %>% as_list()
     days <- dayslevelpage$body$ul %>% as.data.frame() %>% t() %>% as.data.frame() %>%
          filter(substr(V1, 2, 4) == "day")%>% mutate(day = str_sub(V1, 2, -1)) %>% select(day) %>% unlist(use.names = F)
}

getGamesByDay <- function(year, month, day){
     gameslevelpage <- read_html(paste0("http://gd2.mlb.com/components/game/mlb/year_", year, "/", month, day)) %>% as_list()
     games <- gameslevelpage$body$ul %>% as.data.frame() %>% t() %>% as.data.frame() %>%
          filter(substr(V1, 2, 4) == "gid")%>% mutate(gid = str_sub(V1, 2, -1)) %>% select(gid) %>% unlist(use.names = F)
}

doesGameLogExist <- function(year, month, day, gid){
     gid <- gsub(gid, pattern = " ", replacement = "%20")
     gameinfopage <- read_html(paste0("http://gd2.mlb.com/components/game/mlb/year_", year, "/", month, day, gid)) %>% as_list()
     gameinfo <- gameinfopage$body$ul %>% as.data.frame() %>% t() %>% as.data.frame() %>%
          filter(V1 == " game_events.xml")
     if(nrow(gameinfo)){
          return(TRUE)
     } else {
          return(FALSE)
     }
}

games.available <- data.frame()

years <- c(2017:2016)

for(year in years){
     months <- getMonthsByYear(year)
     if(!length(months)){
          next
     }
     for(month in months){
          days <- getDaysByMonth(year, month)
          if(!length(days)){
               next
          }
          for(day in days){
               games <- getGamesByDay(year, month, day)
               if(!length(games)){
                    next
               }
               for(gid in games){
                    if(!length(games)){
                         next
                    }
                    if(doesGameLogExist(year, month, day, gid)){
                         game <- data.frame(year = year, month = month, day = day, gid = gid)
                         print(gid)
                         if(!nrow(games.available)){
                              
                              games.available <- game
                         } else {
                              games.available <- bind_rows(games.available, game)
                         }
                    }
               }
          }
     }
}

if(F){
     games.available %>% filter(year == 2017) %>% write.csv(file = "2017.csv")
     games.available %>% filter(year == 2016) %>% write.csv(file = "2016.csv")
}
