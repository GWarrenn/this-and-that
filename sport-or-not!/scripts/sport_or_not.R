library(tidyverse)
library(googledrive)
library(reshape2)
library(scales)
library(viridis)

drive_find(type = "spreadsheet")
drive_download(as_id("1QVqwzJrUngsbUNSY8gSpiMgkeYCePmxpbFUuqawofuw"), type = "csv")

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
## Plot 1: Overall distribution - everyone loves fruits
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
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Average Sports Rankings",
       subtitle = paste("among a very non-random sample of people with opinions about sports")) +
  guides(fill=F) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=12))

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
  scale_x_discrete(expand = c(0, 0))


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

ggplot(demos_w,aes(x=variable,y=sport)) +
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
  scale_x_discrete(expand = c(0, 0))
