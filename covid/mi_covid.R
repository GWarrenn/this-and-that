library(tidyverse)
library(gridExtra)
library(grid)

mi_data <- read.csv("/Users/august.warren/Documents/GitHub/this-and-that/covid/mi_county_pop_vote.csv", fileEncoding="latin1")

mi_data$trump_bins <- cut(x = mi_data$TRUMPÂ.PCT,breaks = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1),labels = c("0-20%","20-30%","30-40%","40-50%",
                                                                                                   "50-60%","60-70%","70-80%","80-90%","90-100%"))
## https://www.michigan.gov/coronavirus/0,9753,7-406-98178_103214-547150--,00.html
## https://www.michigan.gov/coronavirus/resources/covid-19-vaccine/covid-19-dashboard

#vaccination_data <- readxl::read_xlsx(path = "/Users/august.warren/Downloads/Covid_Vaccine_Doses_Administered_718468_7_0708.xlsx",sheet = "Doses Administered")
vaccination_data_pt1 <- readxl::read_xlsx(path = "/Users/august.warren/Downloads/COVID Vaccines Administered 20201215-20211231-updated-20231121.xlsx",sheet = "Doses Administered")
vaccination_data_pt2 <- readxl::read_xlsx(path = "/Users/august.warren/Downloads/COVID Vaccines Administered 20220101-20221110-updated-20231121.xlsx",sheet = "Doses Administered")
vaccination_data_pt3 <- readxl::read_xlsx(path = "/Users/august.warren/Downloads/COVID Vaccines Administered 20221110-updated-20231121.xlsx",sheet = "Doses Administered")

vaccination_data <- rbind(vaccination_data_pt1,vaccination_data_pt2,vaccination_data_pt3)

vaccination_data_current <- vaccination_data %>%
  #filter(`Dose Number` == "First Dose") %>%
  filter(`Dose Number` == 1) %>%
  #mutate(fmt_date = as.Date(`Data as of`,"%m/%d/%Y")) %>%
  mutate(fmt_date = as.Date(`Week Ending Date`,"%Y-%m-%d",tz="UTC")) %>%
  arrange(fmt_date) %>%
  group_by(`Person's Residence in County`,fmt_date) %>%
  #summarise(cum_vacc = cumsum(`Number of Doses`)) %>%
  summarise(total_weekly = sum(`Number of Doses`)) %>%
  group_by(`Person's Residence in County`) %>%
  mutate(cum_vacc = cumsum(total_weekly)) %>%
  rename(county = `Person's Residence in County`)

write.csv(vaccination_data_current,"/Users/august.warren/Desktop/mi_covid_vaxx_1123.csv")

mi_data_merged <- merge(x=vaccination_data_current,y=mi_data,
                        by.x = "county",by.y="Person.s.Residence.in.County")

summ <- mi_data_merged %>%
  group_by(fmt_date,trump_bins) %>%
  summarise(tot_vacc = sum(cum_vacc),
            tot_pop = sum(Population)) %>%
  mutate(pct_vacc = tot_vacc/tot_pop)

mi_data_merged$actual_pct_vaccinated <- mi_data_merged$cum_vacc / mi_data_merged$Population

##4/10/21 | 5/15/21 | 7/10/21

plot_data <- mi_data_merged %>%
  filter(fmt_date == "2021-04-10")

vote_vacc <- ggplot(plot_data,aes(y=as.numeric(actual_pct_vaccinated),x=TRUMPÂ.PCT)) +
  geom_point(aes(size=as.numeric(Population))) +
  theme_bw() +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0, face= "italic")) +
  labs(x="2020 Trump Vote Percent",
       y="Total Percent with at least one dose",
       title="July 8, 2021",
       caption=paste("R-Squared:",
                round(summary(lm(formula = as.numeric(actual_pct_vaccinated)~TRUMPÂ.PCT, data = plot_data,weights = Population))$r.squared,2))) +
  geom_smooth(se = FALSE,mapping = aes(weight = Population)) +
  geom_vline(xintercept = .5,linetype='dashed') +
  geom_hline(yintercept = sum(plot_data$cum_vacc) / sum(plot_data$Population),linetype='dashed') +
  scale_y_continuous(labels = scales::percent,limits = c(.2,.6)) +
  scale_x_continuous(labels = scales::percent) 

## r-squared over time

r_squared_summary <- data.frame()

for(date in unique(mi_data_merged$fmt_date)){
  date_df <- mi_data_merged %>% filter(fmt_date == date)
  
  temp_df <- data.frame(as.Date(date, format="%Y-%m-%d",origin='1970-01-01'),round(summary(lm(formula = as.numeric(actual_pct_vaccinated)~TRUMPÂ.PCT, data = date_df,weights = Population))$r.squared,2))
  names(temp_df)<-c("date","r_squared")
  
  r_squared_summary <- rbind(temp_df,r_squared_summary)
}

ggplot(r_squared_summary,aes(x=date,y=r_squared)) +
  geom_line()

vote_vacc_april <- ggplot(mi_data,aes(y=april_pct_vaccinated,x=TRUMPÂ.PCT)) +
  geom_point(aes(size=as.numeric(Population))) +
  theme_bw() +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0, face= "italic")) +
  labs(x="2020 Trump Vote Percent",
       y="Total Percent with at least one dose",
       title="April 10, 2021",
       caption=paste("R-Squared:",round(summary(lm(formula = april_pct_vaccinated~TRUMPÊPCT, data = mi_data,weights = Population))$r.squared,2))) +
  geom_smooth(se = FALSE,mapping = aes(weight = Population)) +
  geom_vline(xintercept = .5,linetype='dashed') +
  geom_hline(yintercept = sum(mi_data$april_count_vaccinated) / sum(mi_data$Population),linetype='dashed') +
  scale_y_continuous(labels = scales::percent,limits = c(.2,.6)) +
  scale_x_continuous(labels = scales::percent) 

vote_vacc_may <- ggplot(mi_data,aes(y=may_actual_pct_vaccinated,x=TRUMPÊPCT)) +
  geom_point(aes(size=as.numeric(Population))) +
  theme_bw() +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0, face= "italic")) +
  labs(x="2020 Trump Vote Percent",
       y="Total Percent with at least one dose",
       title="May 10, 2021",
       caption=paste("R-Squared:",round(summary(lm(formula = may_actual_pct_vaccinated~TRUMPÊPCT, data = mi_data,weights = Population))$r.squared,2))) +
  geom_smooth(se = FALSE,mapping = aes(weight = Population)) +
  geom_vline(xintercept = .5,linetype='dashed') +
  geom_hline(yintercept = sum(mi_data$may_actual_count_vaccinated) / sum(mi_data$Population),linetype='dashed') +
  scale_y_continuous(labels = scales::percent,limits = c(.2,.6)) +
  scale_x_continuous(labels = scales::percent) 

vacc_plots <- grid.arrange(vote_vacc_april,vote_vacc_may,vote_vacc,ncol=3,
                             bottom = textGrob("Source: Michigan COVID-19 Vaccine Dashboard: https://www.michigan.gov/coronavirus/0,9753,7-406-98178_103214-547150--,00.html",
                                               x = 0,
                                               y = 0.5,
                                               just = "left",
                                               gp = gpar(fontsize = 8)),
                             top = textGrob("County-level Michigan 2020 Trump vote share by Vaccination percentages (1+ Dose)",
                                            x = 0.03, 
                                            y = 0.5, 
                                            just = "left", 
                                            gp = gpar(fontsize = 18)
                             ))

ggsave(plot = vacc_plots, "/Users/augustwarren/Desktop/vote_vacc_0708.png", w = 16, h = 6)
  
pct_change <- mi_data %>%
  mutate(pct_change_vacc = (actual_count_vaccinated - april_count_vaccinated) / april_count_vaccinated)

change_plot <- ggplot(pct_change,aes(y=pct_change_vacc,x=TRUMPÊPCT)) +
  geom_point(aes(size=as.numeric(Population))) +
  theme_bw() +
  #geom_text(data = filter(pct_change,pct_change_vacc > .27),aes(label=X.3),hjust = 0, nudge_x = 0.01,size=3) +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0, face= "italic"),plot.title = element_text(size=12)) +
  labs(x="2020 Trump Vote Percent",
       y="Percent change in vaccination (April-May 2021)",
       title="County-level change in Vaccinations by Michigan 2020 Trump vote share") +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = .5,linetype='dashed') +
  geom_hline(yintercept = sum(mi_data$actual_count_vaccinated - mi_data$april_count_vaccinated) / sum(mi_data$april_count_vaccinated),linetype='dashed') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)+
  annotate("text", x = .28, y = (sum(mi_data$actual_count_vaccinated - mi_data$april_count_vaccinated) / sum(mi_data$april_count_vaccinated)) + .005, 
           label = "Avg. change")

ggsave(plot = change_plot, "/Users/augustwarren/Desktop/vote_vacc_change.png", w = 8, h = 8)

ggplot(summ,aes(x=trump_bins,y=pct_vacc)) +
  geom_bar(stat = "identity",color="black",fill="darkblue") 





