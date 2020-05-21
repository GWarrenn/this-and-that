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

#####################################################
##
## Connect to data via googlesheets
##
#####################################################

setwd("./GitLab/this-and-that/definitive-gatorade-ranking/")

drive_find(type = "spreadsheet")

sheet_id = ""

drive_download(as_id(sheet_id), type = "csv",overwrite = T)

survey_data <- read.csv("Definitive Gatorade Ranking (Responses).csv")

#####################################################
##
## clean data 
##
#####################################################

## get rid of question text in column/variable names

columns <- colnames(survey_data)

columns <- sub(x=columns,pattern = "Assign.the.following.Gatorade.flavors.colors.to.tiers..where.A.tier.is.the.best.highest.quality.and.F.tier.is.reserved.for.the.flavors.colors.that.deserve.to.be.banished.....",
               replacement = "")

columns <- gsub(x=columns,pattern = "\\.$",
                replacement = "")

colnames(survey_data) <- columns

survey_data$gender_recode <- ifelse(survey_data$With.which.gender.do.you.most.closely.identify == "Male","Male",
                                    ifelse(survey_data$With.which.gender.do.you.most.closely.identify == "Female","Female","Other"))

survey_data$race_recode <- ifelse(survey_data$Which.race.s..ethnicity.ies..best.describes.you == "White/Caucasian","White","POC")

survey_data$income_recode <- factor(survey_data$What.was.your.total.household.income.before.taxes.during.the.past.12.months,
                                    levels = c("Under $50,000","$50,000 to $100,000","Over $100,000","Prefer not to disclose"))

survey_data$age_recode <- ifelse(survey_data$What.is.your.age == "40-44" | survey_data$What.is.your.age == "45-49" | survey_data$What.is.your.age == "50+","40+",
                                 ifelse(survey_data$What.is.your.age == "18-24" | survey_data$What.is.your.age == "25-29","18-29",
                                        ifelse(survey_data$What.is.your.age == "30-34"| survey_data$What.is.your.age == "35-39","30-39",
                                               as.character(survey_data$What.is.your.age))))

survey_data$ideo_recode <- recode(survey_data$What.is.your.ideology , Conservative = "Conservative/Libertarian", 
                                  .default = levels(survey_data$What.is.your.ideology ))

survey_data$ideo_recode <- recode(survey_data$ideo_recode , Libertarian = "Conservative/Libertarian", 
                                  .default = levels(survey_data$ideo_recode ))
  
survey_data$ideo_recode <- factor(survey_data$ideo_recode,
                                  levels =c("Leftist","Liberal","Moderate","Conservative/Libertarian","Prefer not to disclose"))

survey_data$favorability_recode <- ifelse(survey_data$Please.rate.your.overall.favorability.of.Gatorade..in.general..3.is..meh. == 1 |
                                            survey_data$Please.rate.your.overall.favorability.of.Gatorade..in.general..3.is..meh. == 2,
                                          "Very/Somewhat Unfavorable",
                                          ifelse(survey_data$Please.rate.your.overall.favorability.of.Gatorade..in.general..3.is..meh. == 3,
                                                 "Meh",
                                                 ifelse(survey_data$Please.rate.your.overall.favorability.of.Gatorade..in.general..3.is..meh. == 4,
                                                        "Somewhat Favorable",
                                                        ifelse(survey_data$Please.rate.your.overall.favorability.of.Gatorade..in.general..3.is..meh. == 5,
                                                               "Very Favorable",NA))))

survey_data$favorability_recode <- factor(survey_data$favorability_recode,
                                          levels=c("Very/Somewhat Unfavorable","Meh","Somewhat Favorable","Very Favorable"))

count <- nrow(survey_data)

## reshape colors data to long for top-level aggregation

colors <- c("Cool.Blue..Electric.Blue.","Strawberry.Lemonade..Pink." ,"Lemon.Lime..Yellow.",
            "Orange..Orange.","Lime.Cucumber..Light.Green.","Fruit.Punch..Red.","Fierce...Grape..Purple.",
            "Frost...Glacier.Cherry..White.","Fierce...Green.Apple..Green.","Frost...Icy.Charge..Light.Blue.",
            "Frost...Arctic.Blitz..Teal.", "Strawberry.Watermelon..Pink.","Lemonade..Light.Yellow." )

clean_filtered <- survey_data %>%
  select(colors)

clean_filtered$id <- seq.int(nrow(clean_filtered))

clean_l <- melt(clean_filtered,id.vars = "id")

## fixing color names

clean_l$variable <- gsub("\\.\\."," (",clean_l$variable)
clean_l$variable <- gsub("\\.$",")",clean_l$variable,perl = T)
clean_l$variable <- gsub("\\."," ",clean_l$variable)
clean_l$variable <- gsub("\\( ","- ",clean_l$variable)

clean_l$value_recode <- ifelse(clean_l$value == "A-tier" | clean_l$value == "B-tier","A/B-tier",
                               ifelse(clean_l$value == "D-tier" | clean_l$value == "F-tier","D/F-tier",
                                      clean_l$value))

clean_l$gpa <- ifelse(clean_l$value == "A-tier",4,
                      ifelse(clean_l$value == "B-tier",3,
                             ifelse(clean_l$value == "C-tier",2,
                                    ifelse(clean_l$value == "D-tier",1,
                                           ifelse(clean_l$value == "F-tier",0,
                                                  NA)))))

stats <- clean_l %>%
  group_by(variable,value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

stats_a_tier <- stats %>%
  filter(value == "A-tier") %>%
  rename(a_freq = freq) %>%
  select(a_freq,variable)

stats <- merge(stats,stats_a_tier)

stats$value <- factor(stats$value,levels = c("A-tier","B-tier","C-tier","D-tier","F-tier","Don't Know/Never Tried"))

count <- nrow(survey_data)

heatmap_plot <- ggplot(stats,aes(x=value,y=reorder(variable,a_freq))) +
  geom_tile(aes(fill = freq),colour = "white") +
  geom_text(aes(x=value,y=reorder(variable,a_freq),label=percent(round(freq,3)),color = as.numeric(freq) > 0.25)) +
  scale_color_manual(guide = FALSE, values = c("white", "black")) +
  scale_fill_viridis(name="",labels = scales::percent) +
  labs(title = "Overall Gatorade Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about what is & isn't a sport")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels = function(grouping) str_wrap(grouping, width = 20))

ggsave(plot=heatmap_plot,filename="1_overall_ratings.png",path = "figures/",
       w = 10.67, h = 8,type = "cairo-png")

##########################################################################
##
## Scatter plot of Name Recognition v. Flavor GPA
##
##########################################################################

name_recognition <- clean_l %>%
  mutate(familiar = if_else(value != "Don't Know/Never Tried",1,0)) %>%
  group_by(variable,familiar) %>%
  summarise(total_familiar = sum(familiar),
            n = n()) %>%
  mutate(pct_familiar=total_familiar/sum(n)) %>%
  filter(familiar == 1)

flavor_gpa <- clean_l %>%
  group_by(variable) %>%
  filter(!is.na(gpa)) %>%
  summarise(mean_gpa = mean(gpa))

name_recognition_gpa <- merge(name_recognition,flavor_gpa,by="variable")

temp <- ggplot(name_recognition_gpa,aes(x=mean_gpa,y=pct_familiar)) +
  geom_point(size=2) +
  geom_label_repel(aes(label=variable),
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines")) +
  geom_hline(yintercept = mean(name_recognition_gpa$pct_familiar)) +
  geom_vline(xintercept = mean(name_recognition_gpa$mean_gpa)) +
  scale_x_continuous(limits = c(0,4)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x="Mean Flavor GPA",
       y="Percent of Respondents Tried Flavor")

ggsave(plot=temp,filename="temp.png",path = "figures/",
       w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Correlation Matrix
##
#####################################################

clean_l_filtered <- clean_l %>%
  filter(!is.na(gpa))

wide_flavors <- dcast(clean_l_filtered, id ~ variable, value.var = "gpa") 

correlations <- cor(wide_flavors,use="complete.obs")

wide_corr <- melt(correlations)

drop <- c("id")

wide_corr <- wide_corr %>%
  filter(Var1 != "id" & Var2 != "id")

correlations_matrix <- ggplot(wide_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(aes(fill = value),colour = "white") +
  geom_text(aes(x=Var1,y=Var2,label=round(value,2))) +
  scale_fill_gradientn(colours = c("red","white","#1a9641"), 
                       values = rescale(c(-.3,0,.6)),
                       guide = "colorbar", limits=c(-.3,.6)) +
  labs(title = "Fruit Correlation Matrix",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about Gatorade"),
       fill = "R-Squared") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.width = unit(1, "cm"))

ggsave(plot = correlations_matrix, "figures\\2_correlations_matrix.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Fruit Rankings by Demos
##
#####################################################

clean_demos <- survey_data %>%
  select(colors,
         age_recode,
         gender_recode,
         race_recode,
         income_recode,
         ideo_recode,
         favorability_recode)

clean_l_demos <- melt(clean_demos,id.vars = c("age_recode","race_recode","gender_recode","income_recode","ideo_recode","favorability_recode"))

## fixing color names

clean_l_demos$variable <- gsub("\\.\\."," (",clean_l_demos$variable)
clean_l_demos$variable <- gsub("\\.$",")",clean_l_demos$variable,perl = T)
clean_l_demos$variable <- gsub("\\."," ",clean_l_demos$variable)
clean_l_demos$variable <- gsub("\\( ","- ",clean_l_demos$variable)

clean_l_demos$gpa <- ifelse(clean_l_demos$value == "A-tier",4,
                            ifelse(clean_l_demos$value == "B-tier",3,
                                   ifelse(clean_l_demos$value == "C-tier",2,
                                          ifelse(clean_l_demos$value == "D-tier",1,
                                                 ifelse(clean_l_demos$value == "F-tier",0,
                                                        NA)))))

demos <- c("gender_recode","age_recode","income_recode","race_recode","ideo_recode","favorability_recode")
demo_label <- c("Gender","Age","Income","Race/Ethnicity","Ideology","Gatorade Favorability")

num <- 1

for (d in demos) {
  
  group_var <- d[1]
  
  stats_demos <- clean_l_demos %>%
    filter(!is.na(clean_l_demos$gpa)) %>%
    group_by(.dots = group_var,variable) %>%
    summarise(avg_gpa = mean(gpa),
              n = n()) %>%
    filter(n >= 10) %>%
    drop_na()
  
  stats_demos$sort_gpa <- stats_demos$avg_gpa
  
  avg_all_fruits <- clean_l_demos %>%
    filter(!is.na(clean_l_demos$gpa)) %>%
    group_by(.dots = group_var) %>%
    summarise(avg_gpa = mean(gpa),
              n = n()) %>%
    filter(n >= 10)%>%
    select(group_var,avg_gpa) %>%
    drop_na()
  
  avg_all_fruits$variable <- "All Flavors on Average"
  
  avg_all_fruits$sort_gpa <- 0
  
  avg_all_fruits <- avg_all_fruits[,c(1,3,2,4)]
  
  stats_demos <- bind_rows(data.frame(stats_demos),data.frame(avg_all_fruits))
  
  avg_all_fruits <- avg_all_fruits %>%
    rename(avg_gpa_all = avg_gpa) %>%
    select(avg_gpa_all,group_var)
  
  stats_demos <- merge(stats_demos,avg_all_fruits,by=group_var)
  
  stats_demos$variable <- reorder(stats_demos$variable,stats_demos$sort_gpa)
  stats_demos$avg_gpa <- round(stats_demos$avg_gpa,2)

  demo_plot <- ggplot(stats_demos,aes_string(x=d,y="variable")) +
    geom_tile(aes(fill = avg_gpa),colour = "white") +
    geom_text(aes_string(x=d,y="variable",label="avg_gpa",color=as.numeric(stats_demos$avg_gpa) > 1.9)) +
    scale_color_manual(guide = FALSE, values = c("white", "black")) +
    scale_fill_viridis(name="GPA") +
    labs(title = paste("Overall Flavor GPA by",demo_label[num]),
         subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size=12),
          legend.key.width = unit(1, "cm")) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0),labels = function(grouping) str_wrap(grouping, width = 20))
  
  ggsave(plot = demo_plot, paste0("figures\\3_",num,"_demo_",d,"_plot.png"), w = 10.67, h = 8,type = "cairo-png")
  
  num <- num + 1
  
}

################################################
##
## distribution by gender
##
################################################

distribution_gender <- clean_l_demos %>%
  group_by(variable,gender_recode) %>%
  filter(!is.na(gpa)) %>%
  summarise(mean_gpa = mean(gpa),
            median_gpa = median(gpa),
            std_dv = sd(gpa))

distribution_gender_plot <- ggplot(distribution_gender,aes(x=mean_gpa,y=gender_recode, fill=factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles",alpha=.8) +
  labs(x = "Average Gatorade GPA",y = "",
       title="Average Gatorade GPA Distribution by Gender",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about Gatorade"),
       fill="Legend") +
  theme(legend.position="bottom") 

ggsave(plot = distribution_gender_plot, "figures/4_distribution_gender_plot.png", w = 10, h = 6,type = "cairo-png")

#####################################################
##
## Regressions
##
#####################################################

clean_demos$male <- ifelse(clean_demos$gender_recode == "Male",1,0)
clean_demos$white <- ifelse(clean_demos$race_recode == "White",1,0)
clean_demos$youth <- ifelse(clean_demos$age_recode == "18-29",1,0)
clean_demos$low_income <- ifelse(clean_demos$income_recode == "Under $50,000",1,0)
clean_demos$favorable <- ifelse(clean_demos$favorability_recode == "Somewhat Favorable" | clean_demos$favorability_recode == "Very Favorable" ,1,0)

recode_colors <- function(df,color) {
  new_var <- paste0(color,"_recode")
  
  df[new_var] <- ifelse(df[,color] == "A-tier",1,
                        ifelse(df[,color] == "B-tier",.75,
                               ifelse(df[,color] == "C-tier",.5,
                                      ifelse(df[,color] == "D-tier",.25,
                                             ifelse(df[,color] == "F-tier",0,
                                                    NA)))))
  return(as.data.frame(df))
}

for (f in colors) {
  clean_demos <- recode_colors(clean_demos,f)
}

model_results <- data.frame()

for(f in colors) {
  
  dv <- paste0(f,"_recode")
  
  clean_df <- clean_demos %>%
    filter(clean_demos[,f] != "")
  
  model <- glm(get(dv) ~ male + white + youth + low_income + favorable, 
               family = "binomial",
               data=clean_df)
  
  model_df <- as.data.frame(summary.glm(model)$coefficients,row.names = F)
  model_df$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))
  model_df$color <- f
  model_df$odds <- exp(model_df$Estimate)
  
  ci <- as.data.frame(confint(model),row.names=F) %>%
    filter(!is.na(`2.5 %`))
  
  ci$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))
  
  model_df <- merge(ci,model_df,by="iv")
  
  model_df$sig <- ifelse((model_df$`97.5 %` < 0 & model_df$`2.5 %`< 0) | (model_df$`97.5 %` > 0 & model_df$`2.5 %`> 0),1,0)
  
  model_results <- rbind(model_results,model_df)
  
}

## plot regression coefs

model_results$sig_lab <- ifelse(model_results$sig == 1,"Significant (95% Confidence)","Not Significant")

## fixing color names

model_results$color <- gsub("\\.\\."," (",model_results$color)
model_results$color <- gsub("\\.$",")",model_results$color,perl = T)
model_results$color <- gsub("\\."," ",model_results$color)
model_results$color <- gsub("\\( ","- ",model_results$color)

regression_plot <- ggplot(model_results, aes(iv, Estimate,color=sig_lab))+
  facet_wrap(~color) +
  geom_point() +
  scale_color_manual(values=c("black","red")) +
  coord_flip() + 
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  labs(title = "Definitive Gatorade Ranking: Demographic Regression Coefficients",
       x = "Regression Coefficient",
       color="Legend") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

ggsave(plot = regression_plot, "figures/5_Regression Coefs.png", w = 10, h = 6,type = "cairo-png")

#####################################################
##
## Plot 1: Color or Flavor?
##
#####################################################

clean <- survey_data %>%
  select(`When.referring.to.various.Gatorades..do.you.refer.to.the.flavor.or.the.color.most.often`,
         gender_recode,
         income_recode,
         age_recode,
         race_recode,
         ideo_recode,
         favorability_recode) %>%
  rename(flavor_color = `When.referring.to.various.Gatorades..do.you.refer.to.the.flavor.or.the.color.most.often`)

clean_l <- melt(clean,id.vars = c("gender_recode","income_recode","age_recode","race_recode","ideo_recode","favorability_recode"))

demos <- c("age_recode","gender_recode","income_recode","race_recode","ideo_recode","favorability_recode")
demo_label <- c("Age","Gender","Income","Race/Ethnicity","Gatorade Favorability")

num <- 1

all_demos <- data.frame()

for (d in demos) {
  
  group_var <- d[1]
  
  stats_demos <- clean_l %>%
    group_by_(.dots=c(group_var,"variable","value")) %>%
    summarise(n = n()) %>%
    mutate(freq = n/sum(n))
  
  stats_demos$sort_freq <- stats_demos$freq
  
  n_sizes <-  clean %>%
    group_by_(.dots=c(group_var)) %>%
    summarise(n_size = n()) 
  
  stats_demos <- merge(stats_demos,n_sizes) %>%
    filter(n_size >= 10)
  
  stats_demos$merge_var <- stats_demos[,1]
  
  stats_demos$freq <- ifelse(is.na(stats_demos$freq),0,stats_demos$freq)
  stats_demos$sort_freq <- ifelse(is.na(stats_demos$sort_freq),0,stats_demos$sort_freq)
  
  stats_demos$variable <- reorder(stats_demos$variable,stats_demos$sort_freq)
  stats_demos$freq_lab <- percent(round(stats_demos$freq,2))
  
  stats_demos <- stats_demos %>%
    mutate(group_var = d) %>%
    select(group_var,merge_var,variable,value,freq,sort_freq,n_size,freq_lab)
  
  all_demos <- rbind(as.data.frame(stats_demos),as.data.frame(all_demos))
  
  num <- num + 1
  
}

all_demos$group_var_lab <- ifelse(all_demos$group_var == "age_recode","Age",
                                  ifelse(all_demos$group_var == "favorability_recode","Gatorade Favorability",
                                         ifelse(all_demos$group_var == "gender_recode","Gender",
                                                ifelse(all_demos$group_var == "ideo_recode","Ideology",
                                                       ifelse(all_demos$group_var == "income_recode","Income",
                                                              ifelse(all_demos$group_var == "race_recode","Race/Ethnicity",NA))))))

flavor_color_plot <- ggplot(all_demos,aes(x=merge_var,y=freq,fill=value)) +
  geom_bar(stat="identity", position = position_dodge(),color="black") +
  facet_wrap(~group_var_lab,scales = "free") +
  geom_text(aes(x=merge_var,y=freq,label=scales::percent(freq)),position = position_dodge(width = 1),vjust=-.4,size=3) +
  scale_fill_manual(values = c("#3182bd","#9ecae1")) +
  labs(title = "Flavor or Color: How do various groups refer to various Gatorades",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about Gatorade"),
       fill="Legend") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=10),
        legend.key.width = unit(1, "cm"),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(expand = c(0, 0),labels = function(grouping) str_wrap(grouping, width = 10)) +
  scale_y_continuous(limits = c(0,.92),labels = scales::percent)

ggsave(plot = flavor_color_plot, paste0("figures\\6_Flavor or Color -- Demos.png"), w = 10.67, h = 8,type = "cairo-png")

## regressions

clean$color_flavor_dv <- ifelse(clean$flavor_color == "Color",1,0)
clean$male <- ifelse(clean$gender_recode == "Male",1,0)
clean$white <- ifelse(clean$race_recode == "White",1,0)
clean$youth <- ifelse(clean$age_recode == "18-29",1,0)
clean$low_income <- ifelse(clean$income_recode == "Under $50,000",1,0)
clean$favorable <- ifelse(clean$favorability_recode == "Somewhat Favorable" | clean$favorability_recode == "Very Favorable" ,1,0)

clean$leftist <- ifelse(clean$ideo_recode == "Leftist",1,0)
clean$liberal <- ifelse(clean$ideo_recode == "Liberal",1,0)
clean$moderate <- ifelse(clean$ideo_recode == "Moderate",1,0)
clean$conservative <- ifelse(clean$ideo_recode == "Conservative/Libertarian",1,0)

model <- glm(color_flavor_dv ~ male + white + youth + low_income + leftist + liberal + moderate + conservative + favorable, 
             family = "binomial",
             data=clean)

model_df <- as.data.frame(summary.glm(model)$coefficients,row.names = F)
model_df$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))
model_df$odds <- exp(model_df$Estimate)

ci <- as.data.frame(confint(model),row.names=F) %>%
  filter(!is.na(`2.5 %`))

ci$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))

model_df <- merge(ci,model_df,by="iv")

model_df$sig <- ifelse((model_df$`97.5 %` < 0 & model_df$`2.5 %`< 0) | (model_df$`97.5 %` > 0 & model_df$`2.5 %`> 0),1,0)

model_df$sig_lab <- ifelse(model_df$sig == 1,"Significant (95% Confidence)","Not Significant")

regression_plot <- ggplot(model_df, aes(iv, Estimate,color=sig_lab))+
  geom_point() +
  scale_color_manual(values=c("black","red")) +
  coord_flip() + 
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  labs(title = "Flavor or Color: Demographic Regression Coefficients",
       subtitle="Estimated likelihood of referring to Gatorade by the color instead of the flavor",
       x = "Regression Coefficient",
       color="Legend") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

ggsave(plot = regression_plot, "figures/7_Regression Coefs -- Flavor or Color.png", w = 10, h = 6,type = "cairo-png")

############################################
##
## PCA?
##
############################################

clean_demos <- survey_data %>%
  select(colors,
         age_recode,
         gender_recode,
         race_recode,
         income_recode,
         ideo_recode,
         `Which.race.s..ethnicity.ies..best.describes.you`)

clean_l <- melt(clean_demos,id.vars = c("gender_recode","income_recode","age_recode","race_recode","ideo_recode"))

## fixing color names

clean_l$variable <- gsub("\\.\\."," (",clean_l$variable)
clean_l$variable <- gsub("\\.$",")",clean_l$variable,perl = T)
clean_l$variable <- gsub("\\."," ",clean_l$variable)
clean_l$variable <- gsub("\\( ","- ",clean_l$variable)

clean_l$gpa <- ifelse(clean_l$value == "A-tier",4,
                      ifelse(clean_l$value == "B-tier",3,
                             ifelse(clean_l$value == "C-tier",2,
                                    ifelse(clean_l$value == "D-tier",1,
                                           ifelse(clean_l$value == "F-tier",0,
                                                  NA)))))

demos <- c("age_recode","gender_recode","income_recode","race_recode","ideo_recode")

num <- 1

all_demos <- data.frame()

for (d in demos) {
  
  group_var <- d[1]
  
  cols <- group_var
  to_app <- "demo_var"
  cols <- setNames(cols, to_app)
  
  stats_demos <- clean_l %>%
    filter(!is.na(gpa)) %>%
    group_by_(.dots=c("variable",group_var)) %>%
    summarise(avg_fruit_gpa = mean(as.numeric(gpa))) %>% 
    rename_(.dots = cols) %>%
    select(demo_var,variable,avg_fruit_gpa)
  
  all_demos <- rbind(as.data.frame(stats_demos),as.data.frame(all_demos))
}  

all_demos <- all_demos %>%
  filter(!is.na(demo_var))

all_demos_wide <-all_demos %>% 
  filter(demo_var != "Prefer not to disclose") %>%
  spread(demo_var, avg_fruit_gpa)

row.names(all_demos_wide) = all_demos_wide$variable

gatorade.pca <- prcomp(all_demos_wide[,c(2:16)], center = TRUE,scale. = TRUE)

summary(gatorade.pca)

fviz_eig(gatorade.pca)

pca_plot <- fviz_pca_biplot(gatorade.pca, repel = TRUE,
                col.var = "contrib", # Color by contributions to the PC
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                ggtheme = theme_minimal(),
                title = "Gatorade Flavor & Demographics PCA Biplot" 
                  
)

ggsave(plot = pca_plot, "figures/PCA.png", w = 10, h = 6,type = "cairo-png")

#####################################################
##
## Appendix: Demographics table
##
#####################################################

demos <- survey_data %>%
  select(age_recode,income_recode,ideo_recode,gender_recode,race_recode) 

demos$id <- "ix"

demos_l <- melt(demos,id.vars = "id")

demos_summary <- demos_l %>%
  group_by(variable,value) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) 

demos_summary <- demos_summary %>%
  group_by(variable,value) %>%
  arrange(variable,value) %>%
  ungroup() %>%
  select(value,n,freq) %>%
  rename(Response = value,
         N = n,
         Percent = freq)

demos_summary$Percent <- percent(round(demos_summary$Percent,2))

demo_table <- htmlTable(
  x        = demos_summary,
  caption  = paste("Demographic Summary Table"),
  label    = "",
  rowlabel = "",
  rgroup   = c("Age",
               "Income",
               "Ideology",
               "Gender",
               "Race/Ethnicity"),
  n.rgroup = c(3,
               4,
               5,
               3,
               2),
  ctable   = TRUE,
  type="html")

print(demo_table,useViewer = utils::browseURL)


