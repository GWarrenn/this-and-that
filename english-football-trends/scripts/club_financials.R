library(tidyverse)
library(fuzzyjoin)
library(zoo)

table_data <- read.csv("C:/Users/august.warren/Documents/GitLab/this-and-that/english-football-trends/data/historical_table_data.csv")
finance_data <- read.csv("C:/Users/august.warren/Documents/GitLab/this-and-that/english-football-trends/data/transfermarkt_data.csv")

## fuzzy join club records to finance data on club names -- not standardized

joined <- finance_data %>%
  stringdist_full_join(table_data, by = c(club = "team"), max_dist = 3) %>%
  filter(season == Season)

## convert financial data from strings to actual numbers 

joined$multiplier <- ifelse(grepl(x = joined$income,pattern = "m"),1000000,
                            ifelse(grepl(x = joined$income,pattern = "Th."),1000,NA))
  
joined$income_int <- as.numeric(gsub(pattern = "â,¬|m|Th.",replacement = "",x = joined$income)) * joined$multiplier

joined$multiplier <- ifelse(grepl(x = joined$expenditure,pattern = "m"),1000000,
                            ifelse(grepl(x = joined$expenditure,pattern = "Th."),1000,NA))

joined$expenditure_int <- as.numeric(gsub(pattern = "â,¬|m|Th.",replacement = "",x = joined$expenditure)) * joined$multiplier

joined$multiplier <- ifelse(grepl(x = joined$balance,pattern = "m"),1000000,
                            ifelse(grepl(x = joined$balance,pattern = "Th."),1000,NA))

joined$balance_int <- as.numeric(gsub(pattern = "â,¬|m|Th.",replacement = "",x = joined$balance)) * joined$multiplier

joined$balance_ratio <- joined$balance_int / joined$income_int

joined$tier <- ifelse(joined$league == "")

ggplot(joined,aes(x=england_position,y=balance_int,color=league)) +
  geom_point()

ggplot(joined,aes(x=season,y=balance_int,group=club,color=league)) +
  geom_line() + 
  geom_hline(yintercept = 0)
