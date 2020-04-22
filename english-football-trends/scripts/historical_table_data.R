library(devtools)
library(tidyverse)
install_github('jalapic/engsoccerdata')
library(engsoccerdata)

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
  group_by(Season,tier) %>%
  arrange(-Season,tier,-Cumpts) %>%
  mutate(position = row_number())

###################################################
##
## Export Data
##
###################################################

write.csv(file = "historical_table_data.csv",
          x = mydf.final,row.names = F)  