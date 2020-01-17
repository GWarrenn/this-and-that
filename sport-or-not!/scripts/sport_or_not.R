## Author: August Warren
## Description: Analysis of Fringe Sports Survey
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
library(ggnewscale)

#####################################################
##
## download data from Google Sheets/Drive
##
#####################################################

setwd("./GitLab/this-and-that/sport-or-not!/")

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

survey_data$race_recode <- ifelse(survey_data$Which.race.ethnicity.best.describes.you...Please.choose.only.one. == "White/Caucasian","White","POC")

survey_data$income_recode <- factor(survey_data$What.was.your.total.household.income.before.taxes.during.the.past.12.months,
                                    levels = c("Under $50,000","$50,000 to $100,000","Over $100,000","Not sure/Refuse"))

survey_data$sports_fans <- ifelse((survey_data$How.often.would.you.say.you.watch.televised.sports.or.sports.content.on.channels.like.ESPN == "A few times a week" |
                                     survey_data$How.often.would.you.say.you.watch.televised.sports.or.sports.content.on.channels.like.ESPN == "Daily" | 
                                     survey_data$How.often.would.you.say.you.watch.televised.sports.or.sports.content.on.channels.like.ESPN == "Once a week") &
                                    (survey_data$Which.one.of.the.following.best.describes.you == "Avid mainstream/traditional sports fan" |
                                       survey_data$Which.one.of.the.following.best.describes.you == "Casual mainstream/traditional sports fan"),"Sports Fans","Non-Sports Fans")


survey_data$pe_recode <- ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school <= 2,"Unfavorable",
                               ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school >= 4,"Favorable",NA))

## reshape sports data to long for top-level aggregation

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

clean_l$variable <- trimws(gsub(x = clean_l$variable,pattern = "\\.",replacement=" "))
clean_l$variable <- gsub(x = clean_l$variable,pattern = "  ",replacement=" ")

clean_l$variable <- ifelse(clean_l$variable == "eSports Videogames","eSports/Videogames",
                           ifelse(clean_l$variable == "Ping Pong Table Tennis","Ping Pong/Table Tennis",clean_l$variable))


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

#####################################################
##
## Plot 2: Overall distributions by Sport
##
#####################################################

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
## Plot 2A: Overall distributions by Sport (alternate)
##
#####################################################

overall_stats <- clean_l %>%
  filter(value != "") %>%
  group_by(variable,value_recode) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

overall_sports_w <- dcast(overall_stats,variable ~ value_recode, value.var = "freq")

overall_sports_w$ruling <- overall_sports_w$`Sport!` - overall_sports_w$`Not a Sport!` 

stats_strong <- stats %>%
  filter(value == "Sport - Feel Strongly" | value == "Not a Sport - Feel Strongly") %>%
  select(variable,value,freq) %>%
  rename(strong_freq = freq)

stats_strong <- dcast(stats_strong,variable ~ value, value.var = "strong_freq")

stats_strong$strong_freq <- stats_strong$`Sport - Feel Strongly` - stats_strong$`Not a Sport - Feel Strongly`

overall_sports_w <- merge(overall_sports_w,stats_strong,by="variable")

sports_bar_plot <- ggplot(overall_sports_w,aes(x=reorder(variable,ruling),y=ruling,fill=strong_freq)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(x=variable,y=ruling,label=percent(round(ruling,2)))) +
  coord_flip() +
  scale_fill_distiller(palette = "Spectral",direction = 1,labels=scales::percent) +
  labs(title = "Overall Sports Rankings - Difference Between Total Sport & Not Sport",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about what is & isn't a sport")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm")) 

ggsave(plot = sports_bar_plot, "images/2.0A Ratings by Sport.png", w = 10.67, h = 8,type = "cairo-png")


#####################################################
##
## Correlations
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

sports_recode <- c("Chess_recode","eSports..Videogames._recode","Ping.Pong..Table.Tennis._recode","Foosball_recode","Skiing_recode",
                   "Snowboarding_recode","Cycling_recode","Bowling_recode","Golf_recode","Ultimate.Frisbee_recode","Sailing_recode",
                   "Rowing..Crew._recode","Frisbee.Golf_recode","Kickball_recode","Scrabble_recode","Cornhole_recode","Pickleball_recode",
                   "NASCAR_recode","Crossfit_recode")

clean_filtered <- clean %>%
  select(sports_recode)

correlations <- cor(clean_filtered,use="complete.obs")

wide_corr <- melt(correlations)

wide_corr <- wide_corr %>%
  filter(Var1 != "id" & Var2 != "id") %>%
  mutate(Var1 = gsub(pattern = "_recode",replacement = "",x=Var1),
         Var2 = gsub(pattern = "_recode",replacement = "",x=Var2))

wide_corr$Var1 <- trimws(gsub(x = wide_corr$Var1,pattern = "\\.",replacement=" "))
wide_corr$Var1 <- gsub(x = wide_corr$Var1,pattern = "  ",replacement=" ")

wide_corr$Var1 <- ifelse(wide_corr$Var1 == "eSports Videogames","eSports/Videogames",
                           ifelse(wide_corr$Var1 == "Ping Pong Table Tennis","Ping Pong/Table Tennis",wide_corr$Var1))

wide_corr$Var2 <- trimws(gsub(x = wide_corr$Var2,pattern = "\\.",replacement=" "))
wide_corr$Var2 <- gsub(x = wide_corr$Var2,pattern = "  ",replacement=" ")

wide_corr$Var2 <- ifelse(wide_corr$Var2 == "eSports Videogames","eSports/Videogames",
                         ifelse(wide_corr$Var2 == "Ping Pong Table Tennis","Ping Pong/Table Tennis",wide_corr$Var2))


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

## create generalizable funciton to handle all demographic aggregations and plotting

demographic_plots <- function(df,demo,label) {
    
    #########################################
    ##
    ## Plot Average Sport Scores
    ##
    #########################################
    
    new_df <- survey_data %>%
      select(sports,demo) 
    
    new_df_l <- melt(new_df,id.vars = demo)
    
    new_df_l$value_recode <- ifelse(new_df_l$value == "Not a Sport - Don't Feel Strongly" | new_df_l$value == "Not a Sport - Feel Strongly","Not a Sport!",
                                         ifelse(new_df_l$value == "Sport - Don't Feel Strongly" | new_df_l$value == "Sport - Feel Strongly","Sport!",
                                                new_df_l$value))
    demos <- new_df_l %>%
      ungroup() %>%
      group_by(new_df_l[,demo],variable,value_recode) %>%
      summarise(n=n()) %>%
      mutate(freq=n/sum(n)) %>%
      filter(value_recode != "Never heard of/Don't know what this is" & 
               `new_df_l[, demo]` != "Other" & value_recode != "" &
               `new_df_l[, demo]` != "Not sure/Refuse") %>%
      select(`new_df_l[, demo]`,variable,value_recode,freq)
    
    demos_all <- new_df_l %>%
      ungroup() %>%
      group_by(new_df_l[,demo],value_recode) %>%
      summarise(n=n()) %>%
      mutate(freq=n/sum(n)) %>%
      filter(value_recode != "Never heard of/Don't know what this is" & 
               `new_df_l[, demo]` != "Other" & value_recode != "" &
               `new_df_l[, demo]` != "Not sure/Refuse") %>%
      select(`new_df_l[, demo]`,value_recode,freq) %>%
      mutate(variable = "All Sports (Mean)")
    
    demos <- rbind(demos_all,demos)
    
    demos_l <- dcast(demos,variable + value_recode  ~ `new_df_l[, demo]`, value.var = c("freq"))
    
    ## for specific "binary" demos: calculate differences
    
    if(demo == "gender_recode" | demo == "sports_fans" | demo == "race_recode" | demo == "pe_recode"){ 
    
      demos_l$zdiff <- demos_l[,3] - demos_l[,4]

    }
    
    demos_l <- demos_l %>% rename(sport = variable)
    
    stats_a_tier <- demos_l %>%
      ungroup() %>%
      filter(value_recode == "Sport!") %>%
      rename(sport_freq = 3) %>%
      select(sport_freq,sport) %>%
      mutate(sport_freq = if_else(sport == "All Sports (Mean)",1,sport_freq))
    
    demos_l <- merge(demos_l,stats_a_tier) 
    
    demos_w <- melt(demos_l,id.vars = c("value_recode","sport","sport_freq"))
    
    sports_heatmap_plot <- ggplot(demos_w,aes(x=variable,y=reorder(sport,sport_freq))) +
      geom_tile(data=filter(demos_w,variable != 'zdiff'),aes(fill = value),colour = "white") +
      scale_fill_viridis(name="",labels = scales::percent) +
      facet_wrap(~value_recode) +
      new_scale_fill() +
      geom_tile(data = filter(demos_w, variable == 'zdiff'), 
                 aes(fill = value)) + 
      scale_fill_distiller(palette ="Spectral",direction = 1,guide = F) +
      geom_text(aes(x=variable,y=sport,label=percent(round(value,3)),color = (as.numeric(value) > 0.25) | demos_w$variable == 'zdiff')) +
      scale_color_manual(guide = FALSE, values = c("white", "black")) +
      labs(title = "Overall Sports Rankings",
           subtitle = paste("among a very non-random sample of 113 people with opinions about what is & isn't a sport")) +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text = element_text(size=12),
            legend.key.width = unit(1, "cm")) +
      scale_y_discrete(expand = c(0, 0)) +
      scale_x_discrete(expand = c(0, 0),labels = function(grouping) str_wrap(grouping, width = 20))
    
    ggsave(plot = sports_heatmap_plot, paste0("images/4.0 Sport Ratings by ",label,".png"), w = 10.67, h = 8,type = "cairo-png")
  
  #############################################
  ##
  ## Plot Average Sport Scores
  ##
  #############################################  
    
  new_df_l$sport_score <- ifelse(new_df_l$value == "Not a Sport - Don't Feel Strongly",0,
                                       ifelse(new_df_l$value == "Not a Sport - Feel Strongly",.25,
                                              ifelse(new_df_l$value == "Sport - Don't Feel Strongly",.75,
                                                     ifelse(new_df_l$value == "Sport - Feel Strongly",1,NA))))
  
  new_df_l <- new_df_l %>%
    filter(!is.na(sport_score))
  
  avg_ratings_by_demo <- new_df_l %>%
    group_by(new_df_l[,demo],variable) %>%
    summarise(avg_rating = mean(sport_score))
  
  avg_ratings_by_demo <- dcast(avg_ratings_by_demo,variable ~ `new_df_l[, demo]`, value.var = "avg_rating")
  
  avg_ratings_by_demo <- avg_ratings_by_demo %>% rename(sport = variable)
  
  avg_ratings_by_demo <- melt(avg_ratings_by_demo)
  
  sports_heatmap_avgs_plot <- ggplot(avg_ratings_by_demo,aes(x=variable,y=reorder(sport,value))) +
    geom_tile(aes(fill = value),colour = "white") +
    geom_text(aes(x=variable,y=sport,label=round(value,3),color = as.numeric(value) > 0.30)) +
    scale_color_manual(guide = FALSE, values = c("white", "black")) +
    scale_fill_viridis(name="",labels = scales::percent) +
    labs(title = "Overall Sports Rankings",
         subtitle = paste("among a very non-random sample of people with opinions about what is & isn't a sport")) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size=12),
          legend.key.width = unit(1, "cm")) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0))
  
  ggsave(plot = sports_heatmap_avgs_plot, paste0("images/4.0A Sport Average Ratings by ",label,".png"), w = 10.67, h = 8,type = "cairo-png")
  
}

## Now plot all demos

demographic_plots(clean,"gender_recode","Gender")
demographic_plots(clean,"sports_fans","Sports Fans")
demographic_plots(clean,"income_recode","Income")
demographic_plots(clean,"race_recode","Race")
demographic_plots(clean,"pe_recode","PE Therm")

#####################################################
##
## Let's talk about PE...
##
#####################################################

new_df <- survey_data %>%
  select(Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school,gender_recode,sports_fans) 

new_df_l <- melt(new_df,id.vars = c("gender_recode"))

new_df_l$value <- ifelse(is.na(new_df_l$value),"Neutral",new_df_l$value)

gender_tabs <- new_df_l %>%
  group_by(gender_recode,variable,value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(gender_recode != "Other")

labs <- c("PE/Gym Class Favorability","Sports Fans")
names(labs) <- c("Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school", "sports_fans")

gender_tabs$value <- factor(gender_tabs$value,
                                    levels = c("1","2","3","4","5","Non-Sports Fans","Sports Fans"),
                                    labels = c("Very Unfavorable","Somewhat Unfavorable","Neutral","Somewhat Favorable","Very Favorable","Non-Sports Fans","Sports Fans"))

gender_sports <- ggplot(gender_tabs,aes(x=gender_recode,y=freq,fill=value,label = percent(round(freq,3)))) +
  geom_bar(stat="identity",color="black") +
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  facet_grid(~variable,scales="free",labeller = labeller(variable = labs)) +
  scale_fill_manual(values = c("#de2d26","#fee0d2","#D3D3D3","#e5f5e0","#31a354","#deebf7","#3182bd")) +
  labs(title = "Attitudes Towards Sports by Gender",
       subtitle = paste("among a very non-random sample of people with opinions about what is & isn't a sport"),
       fill ="") +
  scale_y_continuous(labels = scales::percent)  +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm"))

ggsave(plot = gender_sports, "images/Sports & Gender.png", w = 10.67, h = 8,type = "cairo-png")

## are views of PE more strongly driven by gender or sports fandom?

survey_data$pe_cont <- ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school == 1,0,
                              ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school == 2,.25,
                                     ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school == 3,.5,
                                            ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school == 4,.75,
                                                   ifelse(survey_data$Please.rate.your.opinion.towards.P.E..Gym.Class.when.you.were.in.school == 5,1,NA)))))

survey_data$male <- ifelse(survey_data$gender_recode == "Male",1,0)
survey_data$sports <- ifelse(survey_data$sports_fans == "Sports Fans",1,0)

model <- glm(data = survey_data,formula = pe_cont ~ male + sports,family = "binomial")

model_df <- as.data.frame(summary.glm(model)$coefficients,row.names = F)
model_df$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))
model_df$odds <- exp(model_df$Estimate)

df <- survey_data %>% select(male,sports,pe_cont)

cor(df,method = "pearson", use = "complete.obs")

## ...both? But more so driven by sports fandom

#####################################################
##
## Regressions
##
#####################################################

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
  