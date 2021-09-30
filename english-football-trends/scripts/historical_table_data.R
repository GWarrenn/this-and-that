library(devtools)
library(tidyverse)
library(engsoccerdata)
library(zoo)

###################################################
##
## Load Data 
##
###################################################

df <- tbl_df(england)
df$Date <- as.Date(df$Date, format="%Y-%m-%d")   

###################################################
##
## Calculate Home/Away Wins 
##
###################################################

dfhome <- df %>% mutate(team = home,
                        opp = visitor,
                        GF=as.numeric(as.character(hgoal)),
                        GA=as.numeric(as.character(vgoal)),
                        GD = GF-GA,
                        result=ifelse(GD>0, "W", ifelse(GD<0, "L", "D")),
                        venue="home") 


dfaway <- df %>% mutate(team = visitor,
                        opp = home, 
                        GF=as.numeric(as.character(vgoal)),
                        GA=as.numeric(as.character(hgoal)),
                        GD = GF-GA,
                        result=ifelse(GD>0, "W", ifelse(GD<0, "L", "D")),
                        venue="away") 


dfboth <- rbind(dfhome,dfaway) %>% select(Date, Season, tier, team, opp, GF, GA, GD)

###################################################
##
## Determine Positions across Seasons/Tiers
##
###################################################

mydf <- dfboth %>%
  group_by(Season, tier,team) %>%
  mutate(result = ifelse(GD>0, "W", ifelse(GD<0, "L", "D")),
         pts = ifelse(GD>0, 3, ifelse(GD<0, 0, 1)),
         gameno = dense_rank(Date)) %>%
  arrange(-Season,tier,team, gameno) %>%
  mutate(Cumpts = cumsum(pts),
         CumGF = cumsum(GF),
         CumGA = cumsum(GA),
         CumGD = cumsum(GD),
         pts.pg = Cumpts/gameno,
         GF.pg = CumGF/gameno,
         GA.pg = CumGA/gameno,
         GD.pg = CumGD/gameno) %>%
  select(Season, team, tier, gameno, Cumpts, CumGF, CumGA, CumGD, pts.pg, GF.pg, GA.pg, GD.pg) 

mydf.final <- mydf %>% 
  filter(gameno == max(gameno)) %>%
  group_by(Season) %>%
  arrange(-Season,tier,-Cumpts) %>%
  mutate(england_position = row_number()) %>%
  group_by(Season,tier) %>%
  arrange(-Season,tier,-Cumpts) %>% 
  mutate(league_position = row_number())

leagues <- mydf.final %>%
  group_by(Season,tier) %>%
  mutate(total_teams_in_league = n()) %>%
  select(Season,tier,total_teams_in_league) %>%
  distinct() %>%
  group_by(Season) %>%
  mutate(adjusted_league_bottom = cumsum(total_teams_in_league))

mydf.final <- merge(mydf.final,leagues,by = c("Season","tier"))

world_wars <- data.frame("Season" = c(seq(1915, 1918),seq(1939,1945)))

mydf.final <- bind_rows(mydf.final, world_wars)

all_seasons <- mydf.final %>%
  expand(Season,team)

mydf.final <- merge(mydf.final,all_seasons,by=c("Season","team"),all=T)

###################################################
##
## Find which club had the worst/best decade performance
##
###################################################

mydf.final <- mydf.final %>% 
  group_by(team) %>% 
  arrange(team,Season) %>%
  mutate(next_year = lead(england_position),
         number_of_seasons = max(row_number()))

mydf.final$change <- mydf.final$england_position - mydf.final$next_year

roughest_decade <- mydf.final %>%
  group_by(team) %>%
  arrange(team,-Season) %>%
  filter(number_of_seasons >= 10) %>%
  mutate(moving_change = rollapply(change,10,mean,align='right',fill=change)) %>%
  select(Season,team,moving_change)

mydf.final <- merge(mydf.final,roughest_decade,by=c("Season","team"),all.x=T)

mydf.final <- mydf.final %>%
  filter(!is.na(team))

best_decade <-  mydf.final %>%
  group_by(team) %>%
  arrange(team,-Season) %>%
  filter(number_of_seasons >= 10) %>%
  mutate(avg_position = rollapply(england_position,10,mean,align='left',fill=NA)) %>%
  select(Season,team,avg_position)

mydf.final <- merge(mydf.final,best_decade,by=c("Season","team"),all.x=T)

###################################################
##
## Export Data for D3
##
###################################################

write.csv(file = "data/historical_table_data.csv",
          x = mydf.final,row.names = F)  

temp <- mydf.final %>%
  arrange(moving_change) %>%
  mutate(rank = rank(moving_change)) %>%
  filter(team == "Sunderland")
