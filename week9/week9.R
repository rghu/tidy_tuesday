setwd("/Users/raghu/OneDrive - Baylor College of Medicine/Work/ParsonsLab/Software/tidytuesday/data/")

############
#Author: Raghu Chandramohan
#Title: Week 9 - TidyTuesday
#Date: 29 May, 2018
############

library(tidyverse)
library(cowplot)

#Reading data
df = read_csv("week9_comic_characters.csv")

#New characters introduced per year by DC and Marvel
A = df %>% ggplot() + geom_bar(aes(x = year, fill= publisher)) + facet_grid(~publisher)

pub_year_count = df %>% group_by(year, publisher) %>% summarise(count = n()) 
pub_year_count_female =  df %>% group_by(year, publisher, sex) %>% 
  summarise(count = n()) %>% filter(sex == "Female Characters")

#Percent female comic characters in the DC and Marvel worlds from 1930s
B = left_join(pub_year_count, pub_year_count_female, by=c("year", "publisher")) %>% 
  group_by(publisher) %>% filter(sex == "Female Characters") %>%
  mutate(cum_heros = cumsum(count.x), cum_female_heros = cumsum(count.y), percent_female = cum_female_heros/cum_heros) %>%
  ggplot() + geom_line(aes(x = year, y = percent_female, group = publisher, color = publisher))

#Percent female comic characters introduced by year since 1980 in the DC and Marvel worlds
C = left_join(pub_year_count, pub_year_count_female, by=c("year", "publisher")) %>% 
  filter(sex == "Female Characters") %>% filter(year >= 1980) %>%
  mutate(percent_female_intro = count.y/count.x) %>%
  ggplot() + geom_line(aes(x = year, y = percent_female_intro, group = publisher, color = publisher))

#Character split
D = df %>% group_by(align, publisher, sex) %>% summarise(count = n()) %>% 
  filter(sex == "Female Characters" | sex == "Male Characters") %>% replace_na(list(align = "Neutral")) %>% 
  filter(align != "Reformed Criminals") %>% 
  ggplot(aes(x = publisher, y = count, fill = factor(align, levels = c("Good Characters", "Neutral", "Bad Characters")))) + 
  geom_bar( stat = "identity", position = "fill") + facet_wrap(~sex) + theme(legend.title = element_blank())

cowplot::plot_grid(A,B,C,D, labels = c("A", "B", "C", "D"))

