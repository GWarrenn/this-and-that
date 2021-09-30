library(tidyverse)
library(gridExtra)
library(grid)

mi_data <- read.csv("/Users/augustwarren/Desktop/mi_covid_0708.csv", fileEncoding="latin1")

mi_data$vacc_percent <- as.numeric(gsub(pattern = "%",replacement = "",x = mi_data$FULLY))

plot <- ggplot(mi_data,aes(x=pct_vaccinated,y=per_100_k)) +
  geom_point(aes(size=as.numeric(cases_daily_avg))) +
  theme(legend.position = "none") +
  labs(x="Percent Total Vaccinated",y="Cases Daily Avg. (per 100k)") +
  geom_smooth()

summary(glm(per_100_k ~ vacc_percent,data = mi_data))

ggsave(plot = plot, "/Users/augustwarren/Desktop/mi_covid_plot.png", w = 8, h = 8)

mi_data$trump_bins <- cut(x = mi_data$TRUMPÊPCT,breaks = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1),labels = c("0-20%","20-30%","30-40%","40-50%",
                                                                                                   "50-60%","60-70%","70-80%","80-90%","90-100%"))

## https://www.michigan.gov/coronavirus/0,9753,7-406-98178_103214-547150--,00.html

vaccination_data <- readxl::read_xlsx(path = "/Users/augustwarren/Downloads/Covid_Vaccine_Doses_Administered_718468_7_0708.xlsx",sheet = "Doses Administered")

vaccination_data_current <- vaccination_data %>%
  filter(`Dose Number` == "First Dose") %>%
  mutate(fmt_date = as.Date(`Data as of`,"%m/%d/%Y")) %>%
  arrange(fmt_date) %>%
  group_by(`Person's Residence in County`) %>%
  summarise(cum_vacc = cumsum(`Number of Doses`)) %>%
  group_by(`Person's Residence in County`) %>%
  summarise(total_vacc = max(cum_vacc))

write.csv(vaccination_data_current,"/Users/augustwarren/Desktop/mi_covid_vaxx_0708.csv")

vaccination_data_april <- vaccination_data %>%
  mutate(fmt_date = as.Date(`Data as of`,"%m/%d/%Y")) %>%
  arrange(fmt_date) %>%
  filter(`Dose Number` == "First Dose" & fmt_date <= as.Date("04/10/2021","%m/%d/%Y")) %>%
  group_by(`Person's Residence in County`) %>%
  summarise(cum_vacc = cumsum(`Number of Doses`)) %>%
  group_by(`Person's Residence in County`) %>%
  summarise(total_vacc = max(cum_vacc))

write.csv(vaccination_data_april,"/Users/augustwarren/Desktop/mi_covid_vaxx_april.csv")

summ <- mi_data %>%
  group_by(trump_bins) %>%
  summarise(tot_vacc = sum(People.Vaccinated.with.at.least.One.Dose),
            tot_pop = sum(Population)) %>%
  mutate(pct_vacc = tot_vacc/tot_pop)

vote_vacc <- ggplot(mi_data,aes(y=as.numeric(actual_pct_vaccinated),x=TRUMPÊPCT)) +
  geom_point(aes(size=as.numeric(Population))) +
  theme_bw() +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0, face= "italic")) +
  labs(x="2020 Trump Vote Percent",
       y="Total Percent with at least one dose",
       title="July 8, 2021",
       caption=paste("R-Squared:",
                round(summary(lm(formula = as.numeric(actual_pct_vaccinated)~TRUMPÊPCT, data = mi_data,weights = Population))$r.squared,2))) +
  geom_smooth(se = FALSE,mapping = aes(weight = Population)) +
  geom_vline(xintercept = .5,linetype='dashed') +
  geom_hline(yintercept = sum(mi_data$actual_count_vaccinated) / sum(mi_data$Population),linetype='dashed') +
  scale_y_continuous(labels = scales::percent,limits = c(.2,.6)) +
  scale_x_continuous(labels = scales::percent) 

vote_vacc_april <- ggplot(mi_data,aes(y=april_pct_vaccinated,x=TRUMPÊPCT)) +
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





