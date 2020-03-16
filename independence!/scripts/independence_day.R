library(lubridate)
library(tidyverse)
library(ggbeeswarm)
library(ggridges)

independence_data <- read.csv("C:/users/augus/desktop/independence_day/independence_day.csv")

independence_data$independence_date <- as.Date(as.character(independence_data$independence_date), "%B %d")

independence_data$day_in_yearly <- yday(independence_data$independence_date) - 1

independence_data <- independence_data %>%
  filter(!is.na(day_in_yearly))

independence_data$season <- ifelse(independence_data$day_in_yearly <= 78 | independence_data$day_in_yearly > 355, "Winter",
                                   ifelse(independence_data$day_in_yearly > 78 & independence_data$day_in_yearly <= 171, "Spring",
                                          ifelse(independence_data$day_in_yearly > 171 & independence_data$day_in_yearly <= 266, "Summer",
                                                 ifelse(independence_data$day_in_yearly > 266 & independence_data$day_in_yearly <= 355,"Fall",""))))

#######################################
##
## distribution of days
##
#######################################

plot_01 <- ggplot(independence_data,aes(x=day_in_yearly,fill=season)) +
  geom_
  geom_histogram(color="black") +
  scale_fill_manual(values = c("Winter" = "#A0E6FF","Spring" = "#00ff7f","Summer" = "#FFFE6F","Fall" = "#EDB579")) +
  geom_vline(xintercept = mean(as.numeric(independence_data$day_in_yearly))) +
  labs(x = "Number of Days into Year",y = "Count",
       title="Summer is for Revolutions",
       subtitle="Distribution of Independence Days",
       fill="Legend") +
  theme(legend.position="bottom")

ggsave(plot = plot_01, "C:/users/augus/desktop/independence_day/01_overall_distribtion.png", w = 6, h = 5,type = "cairo-png")

#######################################
##
## distribution of days by geo position
##
#######################################

## bring in Latitude of the country capital to determine relative position
## source: https://en.wikipedia.org/wiki/List_of_national_capitals_by_latitude

lat_longs <- read.csv("C:/users/augus/desktop/independence_day/country_lat_longs.csv")

lat_longs$Country <- trimws(lat_longs$Country,which = "both")
independence_data$country <- trimws(independence_data$country,which = "both")

independence_data_equator <- merge(independence_data,lat_longs,all.x=T,by.x = c("country"),by.y = c("Country")) %>%
                              filter(!is.na(Latitude))

independence_data_equator$deg_north <- as.numeric(independence_data_equator$Latitude)

## https://en.wikipedia.org/wiki/Tropic_of_Cancer

independence_data_equator$position <- ifelse(independence_data_equator$deg_north > 23.43673,
                                             "North of Tropic of Cancer",
                                             ifelse(independence_data_equator$deg_north < -23.43673,
                                                  "South of Tropic of Capricorn","Equator")) 

independence_data_by_geo <- independence_data_equator %>%
  group_by(position) %>%
  summarise(avg_day_of_year = mean(day_in_yearly))

# Notherners: 200 days --> July 20th
# Equator: 199 --> July 19th
# Southerners: --> August 28th

independence_data_equator$position <- factor(independence_data_equator$position,
                                             levels = c("South of Tropic of Capricorn","Equator","North of Tropic of Cancer"))

plot_02 <- ggplot(independence_data_equator,aes(x=day_in_yearly,y=position, fill=factor(..quantile..))) +
  #geom_density(alpha=.5) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles",alpha=.8) +
  #geom_vline(data = independence_data_by_geo, aes(xintercept = avg_day_of_year,linetype = position)) +
  labs(x = "Number of Days into Year",y = "",
       title="Summer is for Revolutions",
       subtitle="Distribution of Independence Days by Latitude",
       fill="Legend") +
  theme(legend.position="bottom") 

ggsave(plot = plot_02, "C:/users/augus/desktop/independence_day/02_distribtion_by_lat.png", w = 6, h = 5,type = "cairo-png")

plot_03 <- ggplot(independence_data_equator,aes(y=day_in_yearly,x=position))+
  geom_beeswarm() +
  geom_boxplot(alpha=.6) +
  labs(x = "",y = "Number of Days into Year",
       title="Summer is for Revolutions",
       subtitle="Distribution of Independence Days by Latitude")

ggsave(plot = plot_03, "C:/users/augus/desktop/independence_day/03_distribtion_by_lat.png", w = 6, h = 5,type = "cairo-png")

