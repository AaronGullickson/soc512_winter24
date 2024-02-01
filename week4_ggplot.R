#####################
# Aaron Gullickson
# Week 3 - ggplot
# Soc 412/512
# Winter 2024
#####################

# Load libraries and data -------------------------------------------------

# the tidyverse includes ggplot2
library(tidyverse)

# ggthemes will give you access to more pre-built themes
library(ggthemes)

# these packages give you predefined color palettes
library(PNWColors)     # https://github.com/jakelawlor/PNWColors
library(ggsci)         # https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
library(harrypotter)   # https://github.com/aljrico/harrypotter
library(wesanderson)   # https://github.com/karthik/wesanderson

# scales is useful for adjusting tickmark labels without having to manually
# change labels
library(scales)

load("stat_data/earnings.RData")


# First ggplot ------------------------------------------------------------

ggplot(earnings, aes(x = gender, y = wages))+
  geom_violin(aes(fill = gender))+
  geom_boxplot(coef = 100, width=0.1, fill="grey70")+
  #scale_y_continuous(labels = c("$0", "$25", "$50", "$75", "$100"))+ 
  scale_y_continuous(labels = scales::dollar)+
  #scale_fill_manual(values = c("purple", "seagreen"))+
  #scale_fill_brewer(palette = "Pastel1")+
  #scale_fill_viridis_d(begin=0.25)+
  #scale_fill_manual(values = wes_palette("Moonrise2"))+
  #scale_fill_simpsons()+
  scale_fill_hp(house = "ravenclaw")+
  #coord_flip()+
  facet_wrap(~race)+
  #facet_grid(education~race)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = NULL, y = "hourly wages in US dollars",
       fill = "gender of worker",
       title = "Comparative boxplot of hourly wages by gender",
       subtitle = "A subtitle!",
       caption = "Source: Current Population Survey 2018")
  
