# -*- coding: utf-8 -*-
"""
Created on Fri Jun  5 12:30:07 2020

@author: August.Warren
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd
import re

years_url = list(range(2000, 2019))
leagues = ['premier-league','championship','league-one','league-two']

finance_df = pd.DataFrame(columns=['league',
                                   'club',
                                   'season',
                                   'expenditure',
                                   'arrivals',
                                   'income',
                                   'departures',
                                   'balance'])

for year_url in years_url:
    for league in leagues:
        if league == 'premier-league':
            tier = 1
        elif league == 'championship':
            tier = 2
        elif league == 'league-one':
            tier = 3
        elif league == 'league-two':
            tier =4
            
        url = 'https://www.transfermarkt.com/' + league + '/einnahmenausgaben/wettbewerb/GB' + str(tier) + '/plus/0?ids=a&sa=&saison_id=' + str(year_url) + '&saison_id_bis=' + str(year_url) + '&nat=&pos=&altersklasse=&w_s=&leihe=&intern=0'

        response = requests.get(url, headers={'User-Agent': 'Custom5'})
        print(response.status_code)
        financial_data = response.text
        soup = BeautifulSoup(financial_data, 'html.parser')
        
        for tr in soup.find_all('tr')[2:]:
            tds = tr.find_all('td')
            if re.search('greentext', str(tr)) is not None:   
                year_club_list = []
                
                club = tds[2].text
                expenditure = tds[3].text
                arrivals = tds[4].text
                income = tds[5].text
                departures = tds[6].text
                balance = tds[7].text
                
                year_club_list = [league,club,year_url,expenditure,arrivals,income,departures,balance]
                
                finance_df.loc[len(finance_df)] = year_club_list
  
############################                    
##    
## export to csv
##
############################                
             
finance_df.to_csv("C:/Users/august.warren/Documents/GitLab/this-and-that/english-football-trends/data/transfermarkt_data.csv",index=False)
