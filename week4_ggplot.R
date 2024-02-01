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
  scale_fill_hp_d(option = "LunaLovegood")+
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
  

# Conditional Barplot -----------------------------------------------------

# univariate barplot
ggplot(earnings, aes(x = education, y = after_stat(prop), group = 1))+
  scale_y_continuous(labels = scales::percent)+
  geom_bar(fill = "tomato", color="grey20")+
  labs(x = "highest degree attained", y = NULL)+
  theme_bw()

# conditional barplot using color
ggplot(earnings, aes(x = education, y = after_stat(prop), group = foreign_born, 
                     fill=foreign_born))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_hp_d(option="Gryffindor")+
  geom_bar(position = "dodge")+
  labs(x = "highest degree attained", y = NULL, fill = "foreign born?")+
  theme_bw()

# conditional barplot by faceting
ggplot(earnings, aes(x = education, y = after_stat(prop), group=1))+
  scale_y_continuous(labels = scales::percent)+
  geom_bar()+
  facet_wrap(~foreign_born)+
  labs(x = "highest degree attained", y = NULL)+
  theme_bw()

# conditional barplot by both
ggplot(earnings, aes(x = education, y = after_stat(prop), group = foreign_born, 
                     fill=foreign_born))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_hp_d(option="Gryffindor")+
  geom_bar(position = "dodge")+
  facet_wrap(~race)+
  coord_flip()+
  labs(x = "highest degree attained", y = NULL, fill = "foreign born?")+
  theme_bw()

# Scatterplot -------------------------------------------------------------

ggplot(earnings, aes(x = age, y = wages))+
  geom_jitter(alpha = 0.05, width = 0.7, height = 0.7)+
  geom_smooth(method = "lm", se = FALSE, linewidth=2)+
  geom_smooth(se = FALSE, color="tomato", linewidth=2)+
  scale_y_log10(labels = scales::dollar)+
  labs(y="hourly wage in US Dollars")+
  theme_bw()

ggplot(earnings, aes(x = factor(nchild), y = wages))+
  geom_boxplot(varwidth = TRUE, fill="skyblue")+
  scale_y_continuous(labels = scales::dollar)+
  labs(x="number of children", y="hourly wage in US Dollars")+
  theme_bw()


# Conditional means -------------------------------------------------------

# base R use tapply ("t-apply")
tapply(earnings$wages, earnings$marstat, mean)

# tidyverse approach
earnings |>
  group_by(marstat) |>
  summarize(mean_wages = mean(wages),
            n = n())

# feed results into ggplot
earnings |>
  group_by(marstat) |>
  summarize(mean_wages = mean(wages), n=n()) |>
  ggplot(aes(x = reorder(marstat, mean_wages, mean), 
             y = mean_wages, 
             size=n))+
  #geom_col()+
  geom_point(color = "steelblue")+
  scale_y_continuous(labels = scales::dollar)+
  labs(x = NULL, y = "mean wages", size="sample size")+
  theme_bw()
