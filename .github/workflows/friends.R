
library(tidyverse)
library(ggthemes)
library(magick)
library(ggimage)

friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

View(friends)
View(friends_emotions)

friends_wb <- inner_join(friends, friends_emotions)
View(friends_wb)

char_friends <- friends_wb %>% 
  filter(speaker %in% c("Ross Geller","Joey Tribbiani",
                                     "Chandler Bing","Monica Geller",
                                     "Phoebe Buffay","Rachel Green")) %>% 
  group_by(episode, season, scene, utterance) %>% 
  ungroup()


friends_emotions %>% 
  distinct(season)

friends_emotions %>% 
  count(cut_interval(season, 10), episode) %>% 
  ggplot(aes(episode, n)) + 
  geom_point()

img <- "C:/Users/mahendra/Pictures/friends-logo.png"

g <- ggplot(friends_emotions, aes(episode, utterance, colour=emotion)) + 
  geom_count(aes(fill = emotion), alpha = 0.42) + 
  facet_grid(emotion~season) + 
  theme(axis.text.x = element_text(color = "#658525"),
        axis.text.y = element_text(color = "#658525"),
        axis.title.x = element_text(color = "#04879c"),
        axis.title.y = element_text(color = "#04879c")) + 
  labs(x = "Episode", y = "Utterance")


ggbackground(g, img)

img1 <- "C:/Users/mahendra/Pictures/Friends.jpeg"

gg <- ggplot(char_friends, aes(episode, utterance, colour=emotion)) + 
  geom_point(alpha = 0.52) + 
  facet_grid(speaker~emotion) + 
  theme(plot.background = element_rect(fill="#222831"),
        panel.background = element_rect(fill = "#222831"),
        legend.background = element_rect(fill = "#222831"))

ggbackground(gg, img1)





















































