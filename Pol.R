install.packages("ggplot2")
install.packages("gifski")
install.packages("dplyr")
install.packages("png")
installed.packages('devtools')
library(ggplot2)
library(tidyverse)
library(dslabs)
library(dplyr)
library(gganimate)
install.packages('installr')
library(installR)
install.Rtools()
train <- read.csv('E:/Machinehack/Air Pollution/All_Cities.csv', stringsAsFactors = F)
devtools::install_github('https://github.com/thomasp85/gganimate/releases/tag/v0.1.1')
str(train)
train %>% ggplot(aes(PM10, color=State)) 
  +   geom_histogram(alpha=0.6, binwidth = 5) 
+ facet_grid(.~year) 
  + scale_fill_viridis(discrete=TRUE) 
  +   scale_color_viridis(discrete=TRUE)

# install.packages("ggplot2")
# load package and data
options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Scatterplot - Bubble
train %>%
  group_by(State) %>%
  ggplot(aes(PM10,State)) +
  geom_point( ) +
  #  geom_point(aes(col=year)) + 
  labs(subtitle="State Vs PM10", 
       y="State", 
       x="PM10", 
       title="Scatterplot") +
  geom_count(aes(color = ..n..,size = ..n..))  +
    guides(color = 'legend')
  

# Scatterplot - Animate 
g <- train %>%
  group_by(State) %>%
  ggplot(aes(PM10,State,frame=year)) +
  geom_point( ) +
  
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  #facet_wrap(~continent, scales = "free") +
  scale_x_log10()  # convert to log scale

gganimate(g, interval=0.2)
  
  #  geom_point(aes(col=year)) + 
  labs(subtitle="State Vs PM10", 
       y="State",  
       x="PM10", 
       title="Scatterplot") +
  geom_count(aes(color = ..n..,size = ..n..))  +
  guides(color = 'legend')

 anim <- train %>%
    group_by(State) %>%
    ggplot(aes(PM10,State,frame=year)) +
  #ggplot(aes(gdpPercap, lifeExp, size = pop, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    transition_states(year, transition_length = 2, state_length = 1) +
    enter_fade() + 
    exit_fade()
  #animate(anim,renderer = av_renderer())
  animate(anim, nframes = 100, end_pause = 10, rewind = TRUE)