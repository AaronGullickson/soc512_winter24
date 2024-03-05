#####################
# Aaron Gullickson
# Week 8 - reshape, aggregate, and merge data
# Soc 412/512
# Winter 2024
#####################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readr)

# Load  data ---------------------------------------------------------------

# load world bank data
world_bank <- read_csv("stat_data/wdi/world_bank.csv", n_max = 651, na ="..") |>
  rename(country_name = `Country Name`, country_code = `Country Code`,
         series_code = `Series Code`, year2018 = `2018 [YR2018]`, 
         year2019 = `2019 [YR2019]`) |>
  select(!`Series Name`)


# Reshape world bank data -------------------------------------------------


# reshape into the longest format possible
world_bank <- world_bank |>
  pivot_longer(cols = c(year2018, year2019), names_to = "year", 
               names_prefix = "year") |>
  mutate(year = as.numeric(year),
         series_code = case_when(
           series_code == "NY.GDP.MKTP.CD" ~ "gdp",
           series_code == "SP.DYN.LE00.IN" ~ "life_exp",
           series_code == "EN.ATM.CO2E.PC" ~ "emissions"
         ))

# reshape into a country-year format
world_bank <- world_bank |> 
  pivot_wider(names_from = series_code, values_from = value)

# reshape into a country format
world_bank <- world_bank |>
  pivot_wider(names_from = year, values_from = c(gdp, life_exp, emissions),
              names_sep = ".")

# reshape back into a long country-year format
world_bank <- world_bank |>
  pivot_longer(cols= c(starts_with("gdp"), starts_with("life_exp"), 
                       starts_with("emissions")),
               names_sep = "\\.",
               names_to = c("variable", "year")) |>
  pivot_wider(names_from = variable, values_from = value)

world_bank |> pivot_wider(names_from = year, 
                          values_from = c(gdp, life_exp, emissions),
                          names_sep = ".") |>
  ggplot(aes(x=gdp.2018, y=gdp.2019))+
  geom_point()

# Aggregate ipums data ----------------------------------------------------



# Merge world bank and VDEM data ------------------------------------------


