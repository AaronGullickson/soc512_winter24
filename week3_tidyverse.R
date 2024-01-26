########################
# Week 3 - Tidyverse
# Aaron Gullickson
# Soc 412/512
# Winter 2024
########################


# Load Libraries and Data -------------------------------------------------

# libraries
library(tidyverse)

# data
load("stat_data/popularity.RData")
load("stat_data/earnings.RData")

# Two-Way Table Review ----------------------------------------------------

tab <- table(popularity$race, popularity$smoker)
prop.table(tab, 1)

round(prop.table(table(popularity$race, popularity$smoker), 1), 3)*100
round(prop.table(tab, 1), 3)*100

# Tidyverse ---------------------------------------------------------------

## tibbles

chars_df <- data.frame(
  name = c("Joad", "Romaer", "Dilith", "Starling", "Usche"),
  level = c(3, 4, 2, NA, 2),
  char_class = factor(c("Artificer","Bard","Paladin","Fighter","Warlock"),
                      levels=c("Artificer","Barbarian", "Bard", "Cleric",
                               "Druid", "Fighter", "Paladin", "Ranger", "Rogue",
                               "Sorcerer", "Warlock", "Wizard")),
  inspiration = factor(c("Inspired", "Not Inspired",  "Not Inspired",
                       "Inspired", "Not Inspired")))

chars_df

as_tibble(chars_df)

popularity

as.data.frame(popularity)

## piping

# tidyverse pipe: %>%
# base R pipe: |>

x <- c(10, 6, 14, 44, 27, 10)
round(sum(log(x)), 1)

x |> 
  log() |>
  sum() |> 
  round(1)


# base R data wrangling operation

# create has_children variable
earnings$has_children <- factor(earnings$nchild>0,
                                levels=c(FALSE, TRUE),
                                labels=c("No Children","Children Present"))

# check yourself before you wreck yourself
table(earnings$nchild, earnings$has_children)

# subset earnings to those under 45 years of age and just the variables 
# we want
earnings_sub <- subset(earnings, age<45, 
                       select=c("wages", "gender", "race", "has_children"))

# calculate mean earnings by gender, race, and children status
earnings_agg <- aggregate(wages~gender+race+has_children, data=earnings_sub, 
                          mean)

# reorder the aggregate earnings from lowest to highest wage
earnings_agg <- earnings_agg[order(earnings_agg$wages),]

earnings_agg

## pipe it

earnings_agg <- earnings |> 
  mutate(has_children = factor(case_when(
    nchild > 0 ~ "Children Present",
    nchild== 0 ~  "No Children"
  ))) |>
  filter(age < 45) |>
  select(wages, gender, race, has_children) |>
  group_by(gender, race, has_children) |>
  summarize(mean_wages=mean(wages)) |>
  arrange(mean_wages)














