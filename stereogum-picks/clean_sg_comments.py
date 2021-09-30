import pandas as pd
import re

comment_file = open("sg_comments_1102.txt","r")
file = comment_file.readlines()

pattern = re.compile("^[0-9]?[0-9].*")

picks = []

for line in file:
    if pattern.match(line):
        picks.append(line)

df = pd.DataFrame(picks,columns=['string_text'])      

df.to_csv("sg_picks_cleaned_1102.csv",sep=",")