# linearity 

library(tidyverse)
library(broom)
load("stat_data/popularity.RData")
load("stat_data/movies.RData")

ggplot(popularity, aes(x=parent_income, y=nominations))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  geom_smooth(se=FALSE, color="red")+
  theme_bw()

model <- lm(nominations~parent_income, data=popularity)
augment(model) |>
  ggplot(aes(x=.fitted, y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0, linetype=2, color="red", size=1)+
  geom_smooth(se=FALSE)+
  theme_bw()

model <- lm(box_office~rating_imdb, data=movies)
augment(model) |>
  ggplot(aes(x=.fitted, y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0, linetype=2, color="red", size=1)+
  geom_smooth(se=FALSE)+
  theme_bw()
