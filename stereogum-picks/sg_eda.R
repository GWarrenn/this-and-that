library("tidyverse")

sg_cleaned_data <- read.csv("/Users/augustwarren/Desktop/sg_picks_cleaned_1102.csv") %>%
  filter(!grepl("REPLY", string_text)) %>%
  filter(!grepl("64percentmice", string_text)) %>%
  mutate(orig_text = string_text,
         rank = as.numeric(regmatches(string_text,regexpr("^[0-9]?[0-9]",string_text))),
         string_text = str_replace(string_text,"Mil-Spec","Mil Spec"),
         string_text = str_replace(string_text," by ","—"),
         string_text = str_replace(string_text,"\\.-",""),
         string_text = str_replace(string_text,"([0-9]) -","\1"),
         string_text = str_replace(string_text,"([0-9]):","\1"),
         string_text = str_replace(string_text,"([0-9]) :","\1"),
         string_text = str_replace(string_text,"^([0-9]?[0-9]) —","\1"),
         string_text = str_replace(string_text,"^([0-9]?[0-9])\\.","\1"),
         string_text = str_replace(string_text,"^[0-9]?[0-9]",""),
         string_text = str_replace(string_text,"-","—"),
         string_text = str_replace(string_text,"//","—"),
         string_text = str_replace(string_text,":","—"),
         string_text = str_replace(string_text,"\\|","—"),
         string_text = str_replace(string_text,"\\)",""),
         string_text = str_replace(string_text,"–","—")) %>%
  separate(string_text,
           sep = "—", 
           into = c("artist", "album")) 

## artist clean up (pretty ad-hoc)

sg_cleaned_data <- sg_cleaned_data %>%
  mutate(artist = if_else(grepl(x = artist,pattern = "rtj",ignore.case = T),"Run the Jewels",artist),
         artist = if_else(grepl(x = artist,pattern = "Boldy James and Alchemist",ignore.case = T),"Boldy James",artist))

## spell check?

artist_check <- sg_cleaned_data %>%
  mutate(artist_cleaned = tolower(trimws(str_replace(artist,"\001 ","")))) %>%
  group_by(artist_cleaned) %>%
  summarise(count = n()) %>%
  filter(count > 5) %>%
  select(artist_cleaned) %>%
  distinct()

pattern_list <- as.list(artist_check$artist_cleaned)
sg_cleaned_data$already_matched <- ""

for(pattern in pattern_list){
  pattern <- str_replace(pattern,"\\(","")
  print(pattern)
  
  sg_cleaned_data <- sg_cleaned_data %>% 
    mutate(artist_lower = tolower(artist),
           match = mapply(grepl, pattern, artist_lower),
           artist = if_else(match == TRUE,pattern,artist),
           already_matched = if_else(match == TRUE,pattern,already_matched))
}

## now let's try and find artist names in albums? ie. people using album - artist format

pattern_list <- append(pattern_list,c("carly rae jepson","mil spec"))

sg_cleaned_data$already_matched <- ""

for(pattern in pattern_list){
  pattern <- str_replace(pattern,"\\(","")
  print(pattern)
  
  sg_cleaned_data <- sg_cleaned_data %>% 
    mutate(album_lower = tolower(album),
           match = mapply(grepl, pattern, album_lower),
           artist = if_else(match == TRUE,album,artist),
           already_matched = if_else(match == TRUE,pattern,already_matched))
}

all_artists <- sg_cleaned_data %>%
  mutate(artist_cleaned = tolower(trimws(str_replace(artist,"\001 ","")))) %>%
  group_by(artist_cleaned) %>%
  summarise(count = n(),
            avg_rank = mean(rank)) %>%
  filter(count >= 5)

sg_plot <- ggplot(all_artists,aes(x=count,y=avg_rank)) +
  geom_point(size=2) +
  geom_label_repel(data=filter(all_artists,count >= 25),
            aes(x=count,y=avg_rank,label = str_to_title(artist_cleaned))) +
  labs(x="Total number of appearances in commentariat lists",
       y="Average rank in commentariat lists",
       title="Stereogum Commentariat's 2020 Year in Review",
       subtitle="The best artists/albums of the year as picked by the comment section of Stereogum") +
  theme_bw()
  
ggsave(plot = sg_plot, "sg_plot.png", w = 12, h = 6)

## how do these picks stack up against the SG Staff

sg_staff <- read.csv("/Users/augustwarren/Desktop/sg_picks_authored.txt",sep = ",") %>%
  filter(artist != "") %>%
  mutate(artist = tolower(artist),
         artist = if_else(grepl(x = artist,pattern = "Boldy James \\& the Alchemist",ignore.case = T),"boldy james",
                          if_else(grepl(x = artist,pattern = "touché amoré",ignore.case = T),"touche amore",artist)))

sg_comments <- sg_cleaned_data %>%
  mutate(artist_cleaned = tolower(trimws(str_replace(artist,"\001 ","")))) %>%
  group_by(artist_cleaned) %>%
  summarise(count = n(),
            avg_rank = mean(rank),
            median_rank = median(rank)) %>%
  filter(count >= 5) %>%
  arrange(avg_rank) %>%
  mutate(rank = row_number(),
         artist = tolower(artist_cleaned))

sg_comments_staff <- merge(sg_staff,sg_comments,by="artist",all = T) %>%
  filter(rank.y < 50) 

sg_comments_staff$count <- ifelse(is.na(sg_comments_staff$count),0,sg_comments_staff$count)

p1 <- ggplot(sg_comments_staff,aes(x=as.numeric(rank.x),y=count)) +
  geom_point(size=2) +
  geom_smooth(method = "lm",se = F) +
  labs(title="Stereogum Rank by Commentariat List Appearances",
       x="Stereogum Official Rank",
       y="Number of Apperances in Comment Section") +
  theme_bw()

p2 <- ggplot(sg_comments_staff,aes(x=as.numeric(rank.x),y=rank.y)) +
  geom_point(size=2) +
  geom_smooth(method = "lm",se = F) +
  labs(title="Stereogum Rank by Commentariat Rank",
       x="Stereogum Official Rank",
       y="Stereogum Commentariat Rank (Rank-sorted average score)") +
  theme_bw()

p1_p2 <- grid.arrange(p1,p2,ncol=2,
                     top = textGrob("Stereogum 2020 Albums of the Year vs. the Commentariat",
                                    x = 0.03, 
                                    y = 0.5, 
                                    just = "left", 
                                    gp = gpar(fontsize = 18)
                     ),
                     bottom = textGrob("Artists with less than 5 list appearances were excluded to avoid outliers skewing relationship",
                                       x = 0,
                                       y = 0.5,
                                       just = "left",
                                       gp = gpar(fontsize = 8)))

ggsave(plot = p1_p2, "sg_comment_v_ivory_tower.png", w = 12, h = 6)

## artists that didn't make the authors list

comments_only <- sg_comments_staff %>%
  filter(is.na(rank.x))
