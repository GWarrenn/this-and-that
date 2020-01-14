## Author: August Warren
## Description: Analysis of Fringe Sports Surve
## Date: 1/13/2020
## Status: Draft
## Specs: R version 3.4.4 (2018-03-15)

library(tidyverse)
library(googledrive)
library(reshape2)
library(scales)
library(viridis)
library(tidytext)
library(tm)

#####################################################
##
## download data from Google Sheets/Drive
##
#####################################################

drive_find(type = "spreadsheet")

sheet_id = ""

drive_download(as_id(sheet_id), type = "csv",overwrite = T)

survey_data <- read.csv("Sport or Not! (Responses).csv")

#####################################################
##
## clean data 
##
#####################################################

## get rid of question text in column/variable names

columns <- colnames(survey_data)

columns <- sub(x=columns,pattern = "Are.these.things.sports...",
               replacement = "")

columns <- gsub(x=columns,pattern = "\\.$",
               replacement = "")

colnames(survey_data) <- columns

survey_data$gender_recode <- ifelse(survey_data$To.which.gender.do.you.most.closely.identify == "Male","Male",
                                    ifelse(survey_data$To.which.gender.do.you.most.closely.identify == "Female","Female","Other"))

survey_data$race_recode <- factor(survey_data$Which.race.ethnicity.best.describes.you...Please.choose.only.one.,
                                  levels = c("White/Caucasian","Black or African American","Hispanic or Latino","Asian/Pacific Islander",
                                             "American Indian or Alaskan Native","Multiple ethnicity/Other"))

survey_data$income_recode <- factor(survey_data$What.was.your.total.household.income.before.taxes.during.the.past.12.months,
                                    levels = c("Under $50,000","$50,000 to $100,000","Over $100,000","Not sure/Refuse"))

## reshape fruit data to long for top-level aggregation

sports <- c("Chess","eSports..Videogames.","Ping.Pong..Table.Tennis.","Foosball","Skiing",
            "Snowboarding","Cycling","Bowling","Golf","Ultimate.Frisbee","Sailing",
            "Rowing..Crew.","Frisbee.Golf","Kickball","Scrabble","Cornhole","Pickleball",
            "NASCAR","Crossfit")

clean <- survey_data %>%
  select(sports)

clean$id <- seq.int(nrow(clean))

clean_l <- melt(clean,id.vars = "id")

clean_l$value_recode <- ifelse(clean_l$value == "Not a Sport - Don't Feel Strongly" | clean_l$value == "Not a Sport - Feel Strongly","Not a Sport!",
                               ifelse(clean_l$value == "Sport - Don't Feel Strongly" | clean_l$value == "Sport - Feel Strongly","Sport!",
                                      clean_l$value))

#####################################################
##
## Plot 1: Overall distributions on average
##
#####################################################

overall_stats <- clean_l %>%
  filter(value != "") %>%
  group_by(value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

overall_stats$value <- factor(overall_stats$value,levels = c("Sport - Feel Strongly","Sport - Don't Feel Strongly","Not a Sport - Don't Feel Strongly","Not a Sport - Feel Strongly","Never heard of/Don't know what this is","Don't Know/Care"))

overall_bar_plot <- ggplot(overall_stats,aes(x=value,y=freq,fill=value)) +
  geom_bar(stat= "identity",color="black") +
  geom_text(aes(x=value,y=freq,label=percent(round(freq,2))),vjust = -.5) +
  scale_fill_manual(values = c("#1a9641","#a6d96a","#fdae61","#d7191c","#D3D3D3","#D3D3D3")) +
  scale_x_discrete(labels = function(grouping) str_wrap(grouping, width = 20)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Average Sports Rankings",
       subtitle = paste("among a very non-random sample of people with opinions about sports")) +
  guides(fill=F) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=12))

ggsave(plot = overall_bar_plot, "images/1.0 Overall Ratings on Average.png", w = 10.67, h = 8,type = "cairo-png")

stats <- clean_l %>%
  filter(value != "") %>%
  group_by(variable,value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(value != "Never heard of/Don't know what this is")

## add zero percents

sports <- stats %>%
  select(variable) %>%
  distinct()

responses <- stats %>%
  ungroup() %>%
  select(value) %>%
  distinct()

all_combinations <- merge(sports,responses, by = NULL)

stats <- merge(stats,all_combinations,by = c("variable","value"),all.y = T)

stats$freq <- ifelse(is.na(stats$freq),0,stats$freq)

stats_a_tier <- stats %>%
  ungroup() %>%
  filter(value == "Sport - Feel Strongly") %>%
  rename(a_freq = freq) %>%
  select(a_freq,variable)

stats <- merge(stats,stats_a_tier) 

stats$value <- factor(stats$value,levels = c("Sport - Feel Strongly","Sport - Don't Feel Strongly",
                                             "Not a Sport - Don't Feel Strongly","Not a Sport - Feel Strongly"))

count <- nrow(survey_data)

sports_heatmap_plot <- ggplot(stats,aes(x=value,y=reorder(variable,a_freq))) +
  geom_tile(aes(fill = freq),colour = "white") +
  geom_text(aes(x=value,y=reorder(variable,a_freq),label=percent(round(freq,3)),color = as.numeric(freq) > 0.25)) +
  scale_color_manual(guide = FALSE, values = c("white", "black")) +
  scale_fill_viridis(name="",labels = scales::percent) +
  labs(title = "Overall Sports Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about what is & isn't a sport")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels = function(grouping) str_wrap(grouping, width = 20))

ggsave(plot = sports_heatmap_plot, "images/2.0 Ratings by Sport.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Correlations
##
#####################################################

sports_recode <- c("Chess_recode","eSports..Videogames._recode","Ping.Pong..Table.Tennis._recode","Foosball_recode","Skiing_recode",
                   "Snowboarding_recode","Cycling_recode","Bowling_recode","Golf_recode","Ultimate.Frisbee_recode","Sailing_recode",
                   "Rowing..Crew._recode","Frisbee.Golf_recode","Kickball_recode","Scrabble_recode","Cornhole_recode","Pickleball_recode",
                   "NASCAR_recode","Crossfit_recode")

clean_filtered <- clean %>%
  select(sports_recode)

correlations <- cor(clean_filtered,use="complete.obs")

wide_corr <- melt(correlations)

drop <- c("id")

wide_corr <- wide_corr %>%
  filter(Var1 != "id" & Var2 != "id") %>%
  mutate(Var1 = gsub(pattern = "_recode",replacement = "",x=Var1),
         Var2 = gsub(pattern = "_recode",replacement = "",x=Var2))

correlations_matrix <- ggplot(wide_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(aes(fill = value),colour = "white") +
  geom_text(aes(x=Var1,y=Var2,label=round(value,2))) +
  scale_fill_gradientn(colours = c("red","white","#1a9641"), 
                       values = rescale(c(-.3,0,.9)),
                       guide = "colorbar", limits=c(-.3,.9)) +
  labs(title = "Sports Correlation Matrix",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about what is & isn't a sport"),
       fill = "R-Squared") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.width = unit(1, "cm"))

ggsave(plot = correlations_matrix, "images/3.0 Correlation Matrix.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Demographics
##
#####################################################

demographic_plots <- function(df,demo) {
  
  demo <- df$demo
  
  print(demo)
  
  new_df <- survey_data %>%
    select(sports,demo) 
  
  return(new_df)
  
}

clean_demos <- survey_data %>%
  select(sports,
         gender_recode) 

clean_l_demos <- melt(clean_demos,id.vars = c("gender_recode"))

clean_l_demos$value_recode <- ifelse(clean_l_demos$value == "Not a Sport - Don't Feel Strongly" | clean_l_demos$value == "Not a Sport - Feel Strongly","Not a Sport!",
                               ifelse(clean_l_demos$value == "Sport - Don't Feel Strongly" | clean_l_demos$value == "Sport - Feel Strongly","Sport!",
                                      clean_l_demos$value))

demos <- clean_l_demos %>%
  ungroup() %>%
  group_by(gender_recode,variable,value_recode) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(value_recode != "Never heard of/Don't know what this is" & gender_recode != "Other" & value_recode != "")
  
demos_l <- dcast(demos,value_recode + variable ~ gender_recode, value.var = "freq")

demos_l$diff <- demos_l$Male - demos_l$Female

demos_l <- demos_l %>% rename(sport = variable)

demos_w <- melt(demos_l,id.vars = c("value_recode","sport"))

sports_heatmap_plot <- ggplot(demos_w,aes(x=variable,y=sport)) +
  geom_tile(aes(fill = value),colour = "white") +
  facet_wrap(~value_recode) +
  geom_text(aes(x=variable,y=sport,label=percent(round(value,3)),color = as.numeric(value) > 0.25)) +
  scale_color_manual(guide = FALSE, values = c("white", "black")) +
  scale_fill_viridis(name="",labels = scales::percent) +
  labs(title = "Overall Sports Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about what is & isn't a sport")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels = function(grouping) str_wrap(grouping, width = 20))

ggsave(plot = sports_heatmap_plot, "images/4.0 Sport Ratings by Gender.png", w = 10.67, h = 8,type = "cairo-png")

survey_data$sports_fans <- ifelse((survey_data$How.often.would.you.say.you.watch.televised.sports.or.sports.content.on.channels.like.ESPN == "A few times a week" |
                                    survey_data$How.often.would.you.say.you.watch.televised.sports.or.sports.content.on.channels.like.ESPN == "Daily" | 
                                    survey_data$How.often.would.you.say.you.watch.televised.sports.or.sports.content.on.channels.like.ESPN == "Once a week") &
                                    (survey_data$Which.one.of.the.following.best.describes.you == "Avid mainstream/traditional sports fan" |
                                    survey_data$Which.one.of.the.following.best.describes.you == "Casual mainstream/traditional sports fan"),"Sports Fans","Non-Sports Fans")

clean_demos <- survey_data %>%
  select(sports,sports_fans) 

clean_l_demos <- melt(clean_demos,id.vars = c("sports_fans"))

clean_l_demos$value_recode <- ifelse(clean_l_demos$value == "Not a Sport - Don't Feel Strongly",0,
                                     ifelse(clean_l_demos$value == "Not a Sport - Feel Strongly",.25,
                                            ifelse(clean_l_demos$value == "Sport - Don't Feel Strongly",.75,
                                                   ifelse(clean_l_demos$value == "Sport - Feel Strongly",1,NA))))

clean_l_demos <- clean_l_demos %>%
  filter(!is.na(value_recode))

avg_ratings_by_demo <- clean_l_demos %>%
  group_by(sports_fans,variable) %>%
  summarise(avg_rating = mean(value_recode))

avg_ratings_by_demo <- dcast(avg_ratings_by_demo,variable ~ sports_fans, value.var = "avg_rating")

avg_ratings_by_demo$diff <- avg_ratings_by_demo$`Sports Fans` - avg_ratings_by_demo$`Non-Sports Fans`

avg_ratings_by_demo <- avg_ratings_by_demo %>% rename(sport = variable)

avg_ratings_by_demo <- melt(avg_ratings_by_demo)

sports_heatmap_plot <- ggplot(avg_ratings_by_demo,aes(x=variable,y=sport)) +
  geom_tile(aes(fill = value),colour = "white") +
  geom_text(aes(x=variable,y=sport,label=round(value,3),color = as.numeric(value) > 0.25)) +
  scale_color_manual(guide = FALSE, values = c("white", "black")) +
  scale_fill_viridis(name="",labels = scales::percent) +
  labs(title = "Overall Sports Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about what is & isn't a sport")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))

ggsave(plot = sports_heatmap_plot, "sports_heatmap_plot.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Regressions
##
#####################################################

clean <- survey_data %>%
  select(sports,gender_recode,
         income_recode,
         `What.is.your.age`,
         race_recode) %>%
  rename(age_recode = `What.is.your.age`)

recode_sports <- function(df,sport) {
  new_var <- paste0(sport,"_recode")
  
  df[new_var] <- ifelse(df[,sport] == "Sport - Feel Strongly",1,
                        ifelse(df[,sport] == "Sport - Don't Feel Strongly",.75,
                               ifelse(df[,sport] == "Never heard of/Don't know what this is",.5,
                                      ifelse(df[,sport] == "Not a Sport - Don't Feel Strongly",.25,
                                             ifelse(df[,sport] == "Not a Sport - Feel Strongly",0,
                                                    NA)))))
  return(as.data.frame(df))
}

sports <- c("Chess","eSports..Videogames.","Ping.Pong..Table.Tennis.","Foosball","Skiing",
            "Snowboarding","Cycling","Bowling","Golf","Ultimate.Frisbee","Sailing",
            "Rowing..Crew.","Frisbee.Golf","Kickball","Scrabble","Cornhole","Pickleball",
            "NASCAR","Crossfit")

for (f in sports) {
  clean <- recode_sports(clean,f)
}

clean$male <- ifelse(clean$gender_recode == "Male",1,0)
clean$white <- ifelse(clean$race_recode == "White/Caucasian",1,0)
clean$youth <- ifelse(clean$age_recode == "18-24" | 
                        clean$age_recode == "25-29",1,0)
clean$low_income <- ifelse(clean$income_recode == "Under $50,000",1,0)

model_results <- data.frame()

for(f in sports) {
  
  dv <- paste0(f,"_recode")
  
  clean_df <- clean %>%
    filter(clean[,f] != "")
  
  model <- glm(get(dv) ~ male + white + youth + low_income, family = "binomial",data=clean_df)
  
  model_df <- as.data.frame(summary.glm(model)$coefficients,row.names = F)
  model_df$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))
  model_df$sport <- f
  model_df$odds <- exp(model_df$Estimate)
  
  model_results <- rbind(model_results,model_df)
  
}


#####################################################
##
## Open-ends: Text Analysis
##
#####################################################

text <- as.character(survey_data$In.a.few.words..what.makes.a.sport.a.sport.in.your.opinion)

filler_words <- c(" and "," the "," a "," be "," is "," to "," it ")

text = removeWords(as.character(survey_data$In.a.few.words..what.makes.a.sport.a.sport.in.your.opinion),
                               stopwords("english"))

bigrams <- survey_data %>%
  unnest_tokens(bigram,text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))
  