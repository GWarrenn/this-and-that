## Author: August Warren
## Description: Definitive Gatorade Ranking
## Date: 5/18/2020
## Status: Draft
## Specs: R version 3.3.2 (2016-10-31)

library(tidyverse)
library(googledrive)
library(reshape2)
library(scales)
library(viridis)
library(tidytext)
library(tm)
library(ggnewscale)
library(stringr)
library(ggrepel)
library(reshape2)
library(factoextra)
library(ggridges)
library(htmlTable)

#####################################################
##
## Connect to data via googlesheets
##
#####################################################

setwd("this-and-that/thermometer/")

sheets <- drive_find(type = "spreadsheet")

sheet_id <- sheets$id[1]

drive_download(as_id(sheet_id), type = "csv",overwrite = T)

survey_data <- read.csv("Some Like it way Too Cold (Responses).csv")

###############################################
##
## Demographic recoding
##
###############################################

survey_data$gender_recode <- ifelse(survey_data$With.which.gender.do.you.most.closely.identify == "Male","Male",
                                    ifelse(survey_data$With.which.gender.do.you.most.closely.identify == "Female","Female","Other"))

survey_data$race_recode <- ifelse(survey_data$Which.race.s..ethnicity.ies..best.describes.you == "White/Caucasian","White","POC")

survey_data$income_recode <- factor(survey_data$What.was.your.total.household.income.before.taxes.during.the.past.12.months,
                                    levels = c("Under $50,000","$50,000 to $100,000","Over $100,000","Prefer not to disclose"))

survey_data$age_recode <- ifelse(survey_data$What.is.your.age == "40-44" | survey_data$What.is.your.age == "45-49" | survey_data$What.is.your.age == "50+","40+",
                                 ifelse(survey_data$What.is.your.age == "18-24" | survey_data$What.is.your.age == "25-29","18-29",
                                        ifelse(survey_data$What.is.your.age == "30-34"| survey_data$What.is.your.age == "35-39","30-39",
                                               as.character(survey_data$What.is.your.age))))

survey_data$legend <- ifelse(survey_data$Email.Address == "augustjwarren@gmail.com","me","everyone else")

###############################################
##
## Current therm temp
##
###############################################

have_therms <- survey_data %>%
  filter(!is.na(What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.))

ggplot(have_therms,aes(x=What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.)) +
  geom_density() +
  geom_vline(xintercept = mean(have_therms$What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.)) +
  scale_fill_viridis()

###############################################
##
## Current therm temp by gender
##
###############################################

distribution_gender <- have_therms %>%
  group_by(gender_recode) %>%
  summarise(mean_temp = mean(What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.),
            median_temp = median(What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.),
            std_dv = sd(What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.))

women_sd <- filter(distribution_gender,gender_recode=="Female") %>% ungroup() %>% summarise(sd = sd(as.numeric(mean_temp)))
men_sd <- filter(distribution_gender,gender_recode=="Male") %>% ungroup() %>% summarise(sd = sd(as.numeric(mean_temp)))
nonbinary_sd <- filter(distribution_gender,gender_recode=="Other") %>% ungroup() %>% summarise(sd = sd(as.numeric(mean_temp)))

ggplot(have_therms,aes(x=What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.,y=gender_recode, fill=factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles",alpha=.8,option = "C") +
  labs(x = "Thermostat Temp",y = "",
       title="Current Thermostat Setting by Gender",
       subtitle = paste("among a very non-random sample of people with opinions about their indoor climate"),
       caption = paste("Standard deviations: Men =",round(men_sd,2)," | Women =",round(women_sd,2)," | Non-binary =",round(nonbinary_sd,2)),
       fill="Legend") +
  theme(legend.position="bottom") 

###############################################
##
## Current temp by outside temp
##
###############################################

indoor_v_outdoor <- have_therms %>%
  mutate(diff = `What.is.the.current.temperature.outside..in.Fahrenheit...You.can.use.a.weather.app.` - What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.)

ggplot(indoor_v_outdoor,aes(x=`What.is.the.current.temperature.outside..in.Fahrenheit...You.can.use.a.weather.app.`, 
                            y=What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.,
                            color=legend))  +
  geom_jitter() +
  geom_abline() +
  scale_x_continuous(limits = c(65,100)) + 
  scale_y_continuous(limits = c(65,100))

###############################################
##
## Overall ideal temps by time of day
##
###############################################

ideal <- have_therms %>%
  select(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.,
         What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.) %>%
  gather() %>%
  mutate(key_recode = if_else(key =="What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.","Day Temp","Night Temp"))

ideal_day <- filter(ideal,key_recode=="Day Temp") %>% ungroup() %>% summarise(mean = mean(as.numeric(value)))
ideal_night <- filter(ideal,key_recode=="Night Temp") %>% ungroup() %>% summarise(mean = mean(as.numeric(value)))

ggplot(ideal,aes(x=value,fill=key_recode)) +
  geom_density(alpha=.4) +
  geom_vline(xintercept = mean(ideal_day$mean)) + 
  geom_vline(xintercept = mean(ideal_night$mean),linetype="dashed") 

###############################################
##
## Ideal temps by time of day & gender
##
###############################################

ideal_gender <- have_therms %>%
  select(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.,
         What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.,
         gender_recode) %>%
  gather("What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.",
         "What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.",
         -gender_recode,value = "value",key="key") %>%
  mutate(key_recode = if_else(key =="What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.","Day Temp","Night Temp")) %>%
  filter(gender_recode != "Other")

means <- ideal_gender %>% group_by(key_recode,gender_recode) %>% summarise(mean = mean(as.numeric(value)))

ggplot(ideal_gender,aes(x=value,fill=gender_recode)) +
  facet_wrap(~key_recode) +
  geom_density(alpha=.4) +
  geom_vline(data = means,mapping = aes(xintercept = mean,linetype=gender_recode))


collapse_stats <- survey_data %>%
  group_by(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.,What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.) %>%
  summarise(n=n()) %>%
  mutate(key=paste0(as.character(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.), 
                    as.character(What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.)))

plot <- ggplot(survey_data,aes(y=What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.,
                       x=What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.,
                       color=gender_recode)) + 
  geom_jitter(size=3,alpha=.6,width = .05) +
  #geom_point(size=4,alpha=.6) +
  #geom_beeswarm(priority = "random",cex=1,groupOnX = F,size=3,alpha=.6) +
  geom_abline() +
  coord_fixed()






new_df <- data.frame(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer. = c(60:83),
                        What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer. = c(60:83)) %>%
  tidyr::expand(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.,
         What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.) %>%
  dplyr::mutate(key = paste0(as.character(What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer.), 
           as.character(What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.)))

new_df <- new_df[is.na(match(new_df$key,collapse_stats$key)),] %>%
  mutate(n = 0)

temp <- rbind(as.data.frame(collapse_stats),as.data.frame(new_df))

ggplot(temp, aes(x = What.is.your.ideal.room.temperature.during.the.day..in.Fahrenheit..in.Summer., 
                        y = What.is.your.ideal.room.temperature.when.you.go.to.bed..in.Fahrenheit..in.Summer.,fill=n)) +
  geom_tile() + 
  coord_fixed() +
  scale_fill_viridis(option = "D") +
  geom_abline(color="white")

#####################################################
##
## correlations
##
#####################################################

demos <- c("age_recode","gender_recode")

all_demos <- data.frame()

for(f in demos) {
  
  temp <- clean_demos %>%
    filter(clean_demos[,f] != "") %>%
    group_by(clean_demos[,f]) %>%
    summarise(avg_temp = mean(current_temp))
  
  all_demos <- rbind(temp,all_demos)
}  

wide_flavors <- dcast(all_demos, `clean_demos[, f]`  ~ avg_temp, value.var = "gpa") 


#####################################################
##
## Regressions
##
#####################################################

clean_demos <- survey_data %>%
  select(age_recode,
         gender_recode,
         race_recode,
         income_recode,
         What.is.the.current.temperature.outside..in.Fahrenheit...You.can.use.a.weather.app.,
         `What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.`,
          Which.most.accurately.describes.your.current.living.situation.,
         How.many.roommates.do.you.have..if.any.) %>%
  rename(current_temp = "What.temperature.is.your.thermostat.currently.set.to...in.Fahrenheit.",
         current_outside_temp = "What.is.the.current.temperature.outside..in.Fahrenheit...You.can.use.a.weather.app.",
         housing = "Which.most.accurately.describes.your.current.living.situation.",
         roommates = "How.many.roommates.do.you.have..if.any.") %>%
  filter(!is.na(current_temp))

clean_demos$female <- ifelse(clean_demos$gender_recode == "Female",1,0)
clean_demos$white <- ifelse(clean_demos$race_recode == "White",1,0)
clean_demos$youth <- ifelse(clean_demos$age_recode == "18-29",1,0)
clean_demos$low_income <- ifelse(clean_demos$income_recode == "Under $50,000",1,0)
clean_demos$apartment <- ifelse(clean_demos$housing == "Apartment",1,0)
clean_demos$live_alone <- ifelse(clean_demos$roommates == 0,1,0)

model <- lm(current_temp ~ female + white + youth + current_outside_temp + apartment + live_alone,
             data=clean_demos)

model_df <- as.data.frame(summary.lm(model)$coefficients,row.names = F)
model_df$iv <- rownames(as.data.frame(summary.lm(model)$coefficients))
model_df$odds <- exp(model_df$Estimate)

ci <- as.data.frame(confint(model),row.names=F) %>%
  filter(!is.na(`2.5 %`))

ci$iv <- rownames(as.data.frame(summary.lm(model)$coefficients))

model_df <- merge(ci,model_df,by="iv") %>%
  filter(iv != "(Intercept)")

model_df$sig <- ifelse((model_df$`97.5 %` < 0 & model_df$`2.5 %`< 0) | (model_df$`97.5 %` > 0 & model_df$`2.5 %`> 0),1,0)

## plot regression coefs

model_df$sig_lab <- ifelse(model_df$sig == 1,"Significant (95% Confidence)","Not Significant")

regression_plot <- ggplot(model_df, aes(x=iv,y=Estimate,color=sig_lab))+
  geom_point() +
  scale_color_manual(values=c("black","red")) +
  coord_flip() + 
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  labs(title = "Current Thermostat Settings: Demographic Regression Coefficients",
       subtitle = paste("among a very non-random sample of people with opinions about thermostats"),
       y = "Linear Regression Coefficients",
       color="Legend") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

ggsave(plot = regression_plot, "figures/5_Regression Coefs.png", w = 10, h = 6,type = "cairo-png")

