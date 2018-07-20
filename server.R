library(XML)
library(httr)
library(RCurl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)
library(tidyr)

shinyServer(function(input, output, session) {
     
     source("getcolor.R")
     
     getWinExp <- reactive({
          read_excel("WPA/WinExpRaw.xlsx") %>%
               gather("diff", "winexp", which(LETTERS == "E"):(which(LETTERS == "I") + 26)) %>%
               mutate(home_away = as.numeric(diff)) %>%
               select(-diff) %>%
               dplyr::rename(inn.winexp = inn) %>%
               arrange(inn.winexp, half, o, basesit, home_away)
     })
     
     fixDes <- function(des){
          for(q in 1:3){
               des <- str_replace_all(des, pattern = "  ", replacement = " ")
          }
          
          des <- str_replace_all(des, pattern = " Jr\\. ", replacement = " ")
          des <- str_replace_all(des, pattern = "J\\. D\\.", replacement = "J\\.D\\.")
          des <- str_replace_all(des, pattern = "J\\. J\\.", replacement = "J\\.J\\.")
          des <- str_replace_all(des, pattern = "J\\. P\\.", replacement = "J\\.P\\.")
          des <- str_replace_all(des, pattern = "J\\. T\\.", replacement = "J\\.T\\.")
          des <- str_replace_all(des, pattern = "A\\. J\\.", replacement = "A\\.J\\.")
          des <- str_replace_all(des, pattern = "John Ryan", replacement = "JohnRyan")
          des <- str_replace_all(des, pattern = "Michael A. ", replacement = "Michael ")
          
          over <- str_locate(des, pattern = "was overturned: ")[2]
          if(!is.na(over)){
               des <- str_sub(des, (over + 1), -1)
          }
          return(des)
     }
     
     fixAbrev <- function(abr){
          if(abr == "nyy") return("nya")
          if(abr == "nym") return("nyn")
          if(abr == "wsh") return("was")
          if(abr == "chc") return("chn")
          if(abr == "kc") return("kca")
          if(abr == "sd") return("sdn")
          if(abr == "chw") return("cha")
          if(abr == "stl") return("sln")
          if(abr == "tb") return("tba")
          if(abr == "sf") return("sfn")
          if(abr == "laa") return("ana")
          if(abr == "lad") return("lan")
          else return(abr)
     }
     
     basesitOf <- function(b1, b2, b3){
          b1 <- ifelse(b1 == "", NA, b1)
          b2 <- ifelse(b2 == "", NA, b1)
          b3 <- ifelse(b3 == "", NA, b1)
          basesit <- ifelse(is.na(b1) & is.na(b2) & is.na(b3), 1, NA) #bases empty
          basesit <- ifelse(!is.na(b1) & is.na(b2) & is.na(b3), 2, basesit) #runner on first
          basesit <- ifelse(is.na(b1) & !is.na(b2) & is.na(b3), 3, basesit) #runner on second
          basesit <- ifelse(!is.na(b1) & !is.na(b2) & is.na(b3), 4, basesit) #runner on first and second
          basesit <- ifelse(is.na(b1) & is.na(b2) & !is.na(b3), 5, basesit) #runner on third
          basesit <- ifelse(!is.na(b1) & is.na(b2) & !is.na(b3), 6, basesit) #runner on first and third
          basesit <- ifelse(is.na(b1) & !is.na(b2) & !is.na(b3), 7, basesit) #runner on second and third
          basesit <- ifelse(!is.na(b1) & !is.na(b2) & !is.na(b3), 8, basesit) #bases loaded
          basesit
     }
     
     getYear <- reactive({
          str_sub(input$fulldate, 1, 4)
     })
     
     getMonth <- reactive({
          str_sub(input$fulldate, 6, 7)
     })
     
     getDay <- reactive({
          str_sub(input$fulldate, 9, 10)
     })
     
     getGames <- reactive({
          date.url <- paste0("http://www.espn.com/mlb/schedule/_/date/", getYear(), getMonth(), getDay())
          games <- readHTMLTable(date.url)[[1]]
          games$away <- str_sub(games$V1, -3, -1) %>% trimws()
          games$home <- str_sub(games$V2, -3, -1) %>% trimws()
          games$game <- 1
          if(nrow(games) > 1){
               for(i in 2:nrow(games)){
                    prevgames <- games[1:(i - 1),]
                    if(games$home[i] %in% prevgames$home){
                         games$game[i] <- 2
                    }
               }
          }
          
          
          games$winscore <- NA
          games$losescore <- NA
          games$winteam <- NA
          
          for(g in 1:nrow(games)){
               tolook <- gsub("\\s*\\([^\\)]+\\)", "", games$V3[g])
               scores <- str_split(tolook, pattern = ",")[[1]]
               games$winscore[g] <- scores[1] %>% str_sub(-2, -1) %>% trimws() %>% as.numeric()
               games$losescore[g] <- scores[2] %>% str_sub(-2, -1) %>% trimws() %>% as.numeric()
               winner <- scores[1] %>% str_sub(1,3) %>% trimws() %>% toupper() %>% fixAbrev()
               print(winner)
               games$winteam[g] <- ifelse(winner == games$home[g], "home", "away")
          }
          
          print(games)
          
          return(games)
     })
     
     getHomeTeam <- reactive({
          req(getGames(), input$game)
          getGames() %>% filter(V3 == input$game) %>% select(home) %>% unlist(use.names = F)
     })
     
     getAwayTeam <- reactive({
          req(getGames(), input$game)
          getGames() %>% filter(V3 == input$game) %>% select(away) %>% unlist(use.names = F)
     })
     
     getGameNumber <- reactive({
          req(getGames(), input$game)
          getGames() %>% filter(V3 == input$game) %>% select(game) %>% unlist(use.names = F)
     })
     
     awayWin <- reactive({
          req(getGames(), input$game)
          winner <- getGames() %>% filter(V3 == input$game) %>% select(winteam) %>% unlist(use.names = F)
          return(winner == "away")
     })
     
     getGameURL <- reactive({
          req(getHomeTeam(), getAwayTeam(), getGameNumber())
          awayteam <- getAwayTeam() %>% tolower() %>% fixAbrev()
          hometeam <- getHomeTeam() %>% tolower() %>% fixAbrev()
          gamenum <- getGameNumber()
          
          game.url <- paste0("http://gd2.mlb.com/components/game/mlb/year_", getYear(), 
                             "/month_", getMonth(),
                             "/day_", getDay(), 
                             "/gid_", getYear(), 
                             "_", getMonth(), 
                             "_", getDay(), 
                             "_", awayteam, 
                             "mlb_", hometeam, 
                             "mlb_", gamenum, 
                             "/game_events.xml")
          print(game.url)
          return(game.url)
     })
     
     output$gamedrop <- renderUI({
          choices <- getGames()$V3 
          choices <- choices[which(!choices %in% c("Postponed", "LIVE"))]
          selectInput("game", "Game", choices = choices)
     })
     
     output$plot <- renderPlot({
          req(getGameURL())
          
          game.xml <- getURL(getGameURL())
          game.list <- xmlToList(game.xml)
          
          innings.list <- game.list[which(names(game.list) == "inning")]
          
          inning <- 3
          half <- "bottom"
          event <- 1
          
          atbats.df <- data.frame()
          events.df <- data.frame()
          print("test1")
          for(inning in 1:length(innings.list)){
               for(half in c("top", "bottom")){
                    if(!length(innings.list[[inning]][[half]])) next
                    for(event in 1:length(innings.list[[inning]][[half]])){
                         if(names(innings.list[[inning]][[half]][event])=="atbat"){
                              df <- try(unlist(innings.list[[inning]][[half]][[event]]$.attrs) %>% as.data.frame() %>% t() %>% as.data.frame(stringsAsFactors = F), silent = T)
                              if(class(df) == "try-error"){
                                   next
                              }
                         } else if(names(innings.list[[inning]][[half]][event])=="action"){
                              df <- unlist(innings.list[[inning]][[half]][[event]]) %>% as.data.frame() %>% t() %>% as.data.frame(stringsAsFactors = F)
                         }
                         
                         df$inn <- inning
                         df$half <- half
                         df$event <- event
                         df$des <- fixDes(df$des)
                         
                         words <- str_split(df$des[1], pattern = " ")[[1]]
                         words <- words[which(words != "")]
                         words <- gsub(words, pattern = ".", replace = "", fixed = T)
                         
                         df$name <- paste0(words[6], ", ", words[5])
                         
                         if(names(innings.list[[inning]][[half]][event])=="atbat"){
                              if(str_detect(df$des[1], pattern = "intentionally walks")){
                                   df$des[1] <- paste(words[5], words[6], "intentionally walks.")
                              }
                              if(!nrow(atbats.df)){
                                   atbats.df <- df
                              } else {
                                   atbats.df <- bind_rows(atbats.df, df)
                              }
                         } else if(names(innings.list[[inning]][[half]][event])=="action"){
                              if(!nrow(events.df)){
                                   events.df <- df
                              } else {
                                   events.df <- bind_rows(events.df, df)
                              }
                         }
                         
                    }
               }
          }
          print("test2")
          events.df$o <- as.numeric(events.df$o)
          
          atbats.df <- atbats.df %>% arrange(as.numeric(event_num)) %>% mutate(prevOut = lag(o)) %>% mutate(turn = o != prevOut)
          atbats.df$turn <- ifelse(is.na(atbats.df$turn), TRUE, atbats.df$turn)
          atbats.df$o <- as.numeric(atbats.df$o)
          atbats.df$o <- ifelse(atbats.df$turn & atbats.df$o > 0, atbats.df$o - 1 , atbats.df$o)
          atbats.df <- atbats.df %>% arrange(as.numeric(event_num))
          splits <- atbats.df %>% group_by(half, inn, o) %>% count()
          
          atbats.df$basesit <- basesitOf(atbats.df$b1, atbats.df$b2, atbats.df$b3)
          atbats.df$inn.winexp <- ifelse(atbats.df$inn > 9, 9, atbats.df$inn)
          atbats.df$home_away <- as.numeric(atbats.df$home_team_runs) - as.numeric(atbats.df$away_team_runs)
          atbats.df$home_away <- ifelse(atbats.df$home_away > 15, 15, atbats.df$home_away)
          atbats.df$home_away <- ifelse(atbats.df$home_away < -15, -15, atbats.df$home_away)
          
          atbats.df <- atbats.df %>%
               left_join(getWinExp())
          
          if(awayWin()){
               atbats.df$winexp <- 1 - atbats.df$winexp
          }
          
          
          new <- atbats.df[0,]
          
          
          for(i in 1:nrow(splits)){
               current <- atbats.df %>% filter(half == splits$half[i], inn == splits$inn[i], o == splits$o[i])
               current$withinout <- c(1:splits$n[i])
               current$ratio <- 1 / splits$n[i]
               new <- rbind(new, current)
          }
          
          xnudge <- 0
          barwidth <- 10
          ynudge <- 5 + barwidth / 2
          
          
          bottom <- new %>% filter(half == "bottom")
          bottom$batorder <- rep_len(c(1:9), nrow(bottom))
          bottom <- bottom %>% mutate(prevpitcher = lag(pitcher)) %>%
               mutate(newpitcher = !pitcher == prevpitcher)
          
          top <- new %>% filter(half == "top")
          top$batorder <- rep_len(c(1:9), nrow(top))
          top <- top %>% mutate(prevpitcher = lag(pitcher)) %>%
               mutate(newpitcher = !pitcher == prevpitcher)
          
          
          
          atbats.df <- top %>%
               bind_rows(bottom) %>%
               mutate(xdirection = ifelse(o == 0, -1, 1)) %>%
               mutate(xdirection = ifelse(half == "bottom", xdirection * -1, xdirection)) %>%
               mutate(ydirection = ifelse(half == "bottom", -1, 1))
          
          vertbars <- atbats.df %>% 
               filter(o %in% c(0,2)) %>%
               mutate(fullbarlength = ynudge + inn * 10) %>%
               mutate(singlebarlength = fullbarlength * ratio) %>%
               mutate(x1 = xdirection * (xnudge + inn * barwidth), x2 =  x1 + barwidth * xdirection)
          
          vertbarsup <- vertbars %>%
               filter(o == 0) %>%
               mutate(y1 = (withinout - 1) * singlebarlength * ydirection, y2 = y1 + singlebarlength * ydirection)
          
          vertbarsdown <- vertbars %>%
               filter(o == 2) %>%
               mutate(y1 = ydirection * (fullbarlength - (withinout - 1) * singlebarlength), y2 = y1 - singlebarlength * ydirection)
          
          horizbars <- atbats.df %>% 
               filter(o == 1) %>%
               mutate(fullbarlength = ((xnudge + inn * barwidth)) * 2) %>%
               mutate(singlebarlength = fullbarlength * ratio)  %>%
               mutate(x1 = (-1 * fullbarlength / 2 + (withinout - 1) * singlebarlength) * ydirection, x2 = x1 + singlebarlength * ydirection) %>%
               mutate(y1 = ydirection * (ynudge + inn * barwidth), y2 =  y1 + barwidth * ydirection)
          
          allbars <- horizbars %>%
               bind_rows(vertbarsup) %>%
               bind_rows(vertbarsdown) %>%
               arrange(as.numeric(event_num))
          
          singles <- allbars %>%
               filter(str_detect(des, pattern = "singles")) %>%
               mutate(x = (x1 + x2) / 2, y = (y1 + y2) / 2) 
          
          # horizdoubles <- allbars %>%
          #      filter(str_detect(des, pattern = "doubles"), o == 1 ) %>%
          #      mutate(d1x = (x1 + x2) / 2 - .1 * singlebarlength, d1y = (y1 + y2) / 2) %>%
          #      mutate(d2x = (x1 + x2) / 2 + .1 * singlebarlength, d2y = (y1 + y2) / 2)
          
          horizdoubles <- allbars %>%
               filter(str_detect(des, pattern = "doubles"), o == 1 ) %>%
               mutate(d1x = (x1 + x2) / 2, d1y = (y1 + y2) / 2 - barwidth * .15) %>%
               mutate(d2x = (x1 + x2) / 2, d2y = (y1 + y2) / 2 + barwidth * .15)
          
          # vertdoubles <- allbars %>%
          #      filter(str_detect(des, pattern = "doubles"), o %in% c(0, 2)) %>%
          #      mutate(d1x = (x1 + x2) / 2, d1y = (y1 + y2) / 2 - .1 * singlebarlength) %>%
          #      mutate(d2x = (x1 + x2) / 2, d2y = (y1 + y2) / 2 + .1 * singlebarlength)
          
          vertdoubles <- allbars %>%
               filter(str_detect(des, pattern = "doubles"), o %in% c(0, 2)) %>%
               mutate(d1x = (x1 + x2) / 2 - barwidth * .15, d1y = (y1 + y2) / 2) %>%
               mutate(d2x = (x1 + x2) / 2 + barwidth * .15, d2y = (y1 + y2) / 2)
          
          doubles <- horizdoubles %>%
               bind_rows(vertdoubles)
          
          # horiztriples <- allbars %>%
          #      filter(str_detect(des, pattern = "triples"), o == 1 ) %>%
          #      mutate(t1x = (x1 + x2) / 2 - .15 * singlebarlength, t1y = (y1 + y2) / 2) %>%
          #      mutate(t2x = (x1 + x2) / 2 + .15 * singlebarlength, t2y = (y1 + y2) / 2) %>%
          #      mutate(t3x = (x1 + x2) / 2, t3y = (y1 + y2) / 2)
          
          horiztriples <- allbars %>%
               filter(str_detect(des, pattern = "triples"), o == 1 ) %>%
               mutate(t1x = (x1 + x2) / 2 - .12 * barwidth, t1y = (y1 + y2) / 2 - barwidth * .12 * sign(y1)) %>%
               mutate(t2x = (x1 + x2) / 2 + .12 * barwidth, t2y = (y1 + y2) / 2 - barwidth * .12 * sign(y1)) %>%
               mutate(t3x = (x1 + x2) / 2, t3y = (y1 + y2) / 2 + barwidth * .12 * sign(y1))
          
          # verttriples <- allbars %>%
          #      filter(str_detect(des, pattern = "triples"), o %in% c(0, 2)) %>%
          #      mutate(t1x = (x1 + x2) / 2, t1y = (y1 + y2) / 2 - .15 * singlebarlength) %>%
          #      mutate(t2x = (x1 + x2) / 2, t2y = (y1 + y2) / 2 + .15 * singlebarlength) %>%
          #      mutate(t3x = (x1 + x2) / 2, t3y = (y1 + y2) / 2)
          
          verttriples <- allbars %>%
               filter(str_detect(des, pattern = "triples"), o %in% c(0, 2)) %>%
               mutate(t1x = (x1 + x2) / 2 - barwidth * .12 * sign(x1), t1y = (y1 + y2) / 2 - .12 * barwidth) %>%
               mutate(t2x = (x1 + x2) / 2 - barwidth * .12 * sign(x1), t2y = (y1 + y2) / 2 + .12 * barwidth) %>%
               mutate(t3x = (x1 + x2) / 2 + barwidth * .12 * sign(x1), t3y = (y1 + y2) / 2)
          
          triples <- horiztriples %>%
               bind_rows(verttriples)
          
          walks <- allbars %>%
               filter(str_detect(des, pattern = "walks")) %>%
               mutate(x = (x1 + x2) / 2, y = (y1 + y2) / 2) 
          
          homers <- allbars %>%
               filter(str_detect(des, pattern = "homers") | str_detect(des, pattern = "grand slam")) %>%
               mutate(x = (x1 + x2) / 2, y = (y1 + y2) / 2) 
          
          pitchingchangesvert <- allbars %>%
               filter(newpitcher, o %in% c(0,2)) %>%
               mutate(x = (x1 + x2) / 2, y = y1, angle = 90 * o * xdirection + 90 * (ydirection - 1))
          
          pitchingchangeshoriz <- allbars %>%
               filter(newpitcher, o == 1) %>%
               mutate(x = x1, y = (y1 + y2) / 2, angle = ydirection * 270)
          
          pitchingchanges <- pitchingchangesvert %>%
               bind_rows(pitchingchangeshoriz)
          
          strikeouts <- allbars %>%
               filter(str_detect(des, pattern = "strikes out") | str_detect(des, pattern = "called out on strikes")) %>%
               mutate(x = (x1 + x2) / 2, y = (y1 + y2) / 2)  
          
          scoringplays <- allbars %>%
               filter(str_detect(des, pattern = "scores"))
          
          events.df$des <- str_replace_all(events.df$des, pattern = "steals \\([0-9]\\)+ home", replacement = "scores")
          scoringplays.events <- events.df %>%
               filter(str_detect(des, pattern = "scores"))
          #print(scoringplays.events)
          #need to add events
          scoringplays <- scoringplays %>% bind_rows(scoringplays.events)
          
          scores <- allbars[0,]
          i <- 5
          for(i in 1:nrow(scoringplays)){
               words <- str_split(scoringplays$des[i], pattern = " ")[[1]]
               words <- words[which(words != "")]
               scorewords <- which(words %in% c("scores.", "scores"))
               names <- words[scorewords - 1]
               fnames <- words[scorewords - 2]
               possiblecounter <- 1
               possiblescored <- allbars %>%
                    filter(inn == scoringplays$inn[i], half == scoringplays$half[i], as.numeric(event_num) < as.numeric(scoringplays$event_num[i]))
               possiblescored$lastname <- lapply(1:nrow(possiblescored), function(row){
                    str_split(possiblescored$des[row], pattern = " ")[[1]][2]
               }) %>% unlist()
               namecounter <- length(names)
               pinchrunners <- events.df %>%
                    filter(inn == scoringplays$inn[i], half == scoringplays$half[i], as.numeric(event_num) < as.numeric(scoringplays$event_num[i]),
                           str_detect(des, pattern = "inch-runner"))
               if(nrow(pinchrunners)){
                    for(q in nrow(pinchrunners)){
                         words <- str_split(pinchrunners$des[q], pattern = " ")[[1]]
                         words <- words[which(words != "")]
                         new <- words[which(words == "replaces") - 1]
                         old <- str_sub(words[length(words)], 1, -2)
                         possiblescored$lastname <- str_replace_all(possiblescored$lastname, pattern = old, replacement = new)
                    }
               }
               for(j in nrow(possiblescored):1){
                    if(possiblescored$lastname[j] == names[namecounter]){
                         scores <- scores %>% bind_rows(possiblescored[j,])
                         namecounter <- namecounter - 1
                    }
                    if(namecounter == 0){
                         break
                    }
               }
               
          }
          
          
          
          scores <- scores %>% bind_rows(homers)
          scores$team <- ifelse(scores$half == "top", getAwayTeam(), getHomeTeam())
          scores$color <- NA
          scores$color2 <- NA
          for(i in 1:nrow(scores)){
               scores$color[i] <- getcolor(scores$team[i])
               scores$color2[i] <- getcolor2(scores$team[i])
          }
          
          last.ab <- allbars %>% arrange(as.numeric(event_num)) %>% slice(n())
          
          segments.q1 <- allbars %>% 
               select(inn) %>%
               distinct() %>%
               mutate(x1 = xnudge + inn * 10,
                      x2 = xnudge + inn * 10 + 5,
                      y1 = ynudge + inn * 10 + 5,
                      y2 = ynudge + inn * 10)
          
          segments.q2 <- segments.q1 %>% 
               mutate(x2.new = x1 * -1, x1.new = x2 * -1, y1.new = y2, y2.new = y1) %>% 
               select(-x1, -x2, -y1, -y2) %>%
               rename(x1 = x1.new, x2 = x2.new, y1 = y1.new, y2 = y2.new)
          
          segments.q3 <- segments.q2 %>% 
               mutate(x2.new = x1, x1.new = x2, y1.new = y2 * -1, y2.new = y1 * -1) %>% 
               select(-x1, -x2, -y1, -y2) %>%
               rename(x1 = x1.new, x2 = x2.new, y1 = y1.new, y2 = y2.new)
          
          segments.q4 <- segments.q3 %>% 
               mutate(x2.new = x1 * -1, x1.new = x2 * -1, y1.new = y2, y2.new = y1) %>% 
               select(-x1, -x2, -y1, -y2) %>%
               rename(x1 = x1.new, x2 = x2.new, y1 = y1.new, y2 = y2.new)
          
          if(last.ab$half == "top"){
               segments.q3 <- segments.q3 %>%
                    slice(-n())
               segments.q4 <- segments.q4 %>%
                    slice(-n())
          } else {
               if(last.ab$o != 2 & !str_detect(last.ab$des, pattern = "double play")){
                    segments.q3 <- segments.q3 %>%
                         slice(-n())
               }
          }
          
          segments <- segments.q1 %>%
               bind_rows(segments.q2) %>%
               bind_rows(segments.q3) %>%
               bind_rows(segments.q4)
               
          
          inningnumbers <- data.frame(inn = c(1:max(allbars$inn))) %>%
               mutate(x = -1 * (xnudge + inn * barwidth + barwidth / 2), y = 0)
          
          dotsize.big <- 4
          dotsize.small <- 3
          hrsize.big <- 7
          hrsize.small <- 6
          
          #allbars$batorder <- factor(allbars$batorder, levels = c(1:9))
          
          scores <- scores %>% arrange(color)
          
          color.vec1 <- scores %>% arrange(color) %>% select(color) %>% distinct() %>% unlist(use.names = F)
          color.vec2 <- scores %>% arrange(color2) %>% select(color2) %>% distinct() %>% unlist(use.names = F)
          color.vec <- c(color.vec1, color.vec2)
          names(color.vec) <- color.vec
          
          print(tail(allbars$winexp))
          
          ggplot() + geom_rect(data = allbars, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = winexp), color = "white", size =2) + 
               geom_curve(data = segments, aes(x = x1, xend = x2, y = y1, yend = y2), curvature = -.5, size = 2, color = "gray") + 
               geom_segment(data = allbars, aes(x= -10 * max(allbars$inn) - 10, xend = 10 * max(allbars$inn) + 10, y = 0, yend = 0), color = "white", size = 5) + 
               geom_rect(data = scores, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = winexp, color = color), size =2) + 
               #geom_rect(data = scores %>% arrange(color), aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, color = color2), linetype = "dotted", size =2, alpha = 0) + 
               geom_rect(data = scores, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, color = color2), linetype = "dashed", size = 1, alpha = 0) + 
               scale_color_manual(values = color.vec) +
               geom_point(data = singles, aes(x, y), color = "white", size = dotsize.big) + 
               geom_point(data = singles, aes(x, y), color = "black", size = dotsize.small) + 
               geom_point(data = doubles, aes(d1x, d1y), color = "white", size = dotsize.big) + 
               geom_point(data = doubles, aes(d2x, d2y), color = "white", size = dotsize.big) + 
               geom_point(data = doubles, aes(d1x, d1y), color = "black", size = dotsize.small) + 
               geom_point(data = doubles, aes(d2x, d2y), color = "black", size = dotsize.small) + 
               geom_point(data = triples, aes(t1x, t1y), color = "white", size = dotsize.big) + 
               geom_point(data = triples, aes(t2x, t2y), color = "white", size = dotsize.big) + 
               geom_point(data = triples, aes(t3x, t3y), color = "white", size = dotsize.big) + 
               geom_point(data = triples, aes(t1x, t1y), color = "black", size = dotsize.small) + 
               geom_point(data = triples, aes(t2x, t2y), color = "black", size = dotsize.small) + 
               geom_point(data = triples, aes(t3x, t3y), color = "black", size = dotsize.small) + 
               #geom_point(data = walks, aes(x, y), color = "black", size = dotsize.big) + 
               #geom_point(data = walks, aes(x, y), color = "white", size = dotsize.small) + 
               geom_point(data = walks, aes(x, y), color = "green", size = dotsize.big, shape = 18) + 
               geom_point(data = homers, aes(x, y), color = "white", size = hrsize.big) + 
               geom_point(data = homers, aes(x, y), color = "black", size = hrsize.small, shape = 18) + 
               #geom_point(data = strikeouts, aes(x, y), color = "white", size = dotsize.big) + 
               geom_point(data = strikeouts, aes(x, y), color = "red", size = dotsize.big, shape = 18) + 
               geom_text(data = pitchingchanges, aes(x, y, angle=angle), hjust=0.6, vjust=-0.4, label="Delta", parse=T, color="black", size=hrsize.big) +
               geom_text(data = pitchingchanges, aes(x, y, angle=angle), hjust=0.4, vjust=-0.35, label="Delta", parse=T, color="white", size=hrsize.big) +
               geom_text(data = inningnumbers, aes(x, y, label = inn), color="gray", size = 3) +
               
               #geom_point(data = pitchingchanges, aes(x, y), color = "black", size = dotsize.big, shape = 18) + 
               
               scale_fill_gradient(low = "#E8E8E8", high = "black", limits = c(0, 1)) +
               theme(legend.position="none", panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                     axis.ticks = element_blank(), axis.text = element_blank(), axis.title=element_blank(), 
                     panel.background=element_rect(fill="white"))
     })
})