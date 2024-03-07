#####################
# Aaron Gullickson
# Week 8 - reshape, aggregate, and merge data
# Soc 412/512
# Winter 2024
#####################


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readr)
library(ggvenn)

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
               names_sep = "\\.", # separator needs to be double escaped
               names_to = c("variable", "year")) |>
  pivot_wider(names_from = variable, values_from = value)

world_bank |> pivot_wider(names_from = year, 
                          values_from = c(gdp, life_exp, emissions),
                          names_sep = ".") |>
  ggplot(aes(x=gdp.2018, y=gdp.2019))+
  geom_point()

# Aggregate ipums data ----------------------------------------------------

acs <- read_fwf("stat_data/ipums/usa_00131.dat.gz",
                col_positions = fwf_cols(SEX = c(53, 53),
                                         AGE = c(54, 56),
                                         MARST = c(57, 57),
                                         EDUCD = c(69, 71),
                                         SEI = c(72, 73),
                                         STATEFIP = c(37, 38)),
                col_types = cols(.default = "i")) |>
  mutate(sei = ifelse(SEI == 0, NA, SEI),
         sex = factor(SEX,
                      levels = c(1, 2),
                      labels = c("Male", "Female")),
         degree = factor(case_when(
           EDUCD <= 1 | EDUCD == 999 ~ NA,
           EDUCD < 62 ~ "LHS",
           EDUCD < 81 ~ "HS",
           EDUCD < 101 ~ "AA",
           EDUCD < 114 ~ "BA",
           TRUE ~ "Grad"), 
           levels = c("LHS", "HS", "AA", "BA", "Grad")),
         marst = factor(case_when(
           MARST == 9 ~ NA,
           MARST <= 2 ~ "Married",
           MARST <= 4 ~ "Divorced/Separated",
           MARST == 5 ~ "Widowed",
           MARST == 6 ~ "Never Married"
         ), 
         levels = c("Never Married", "Married", "Divorced/Separated", "Widowed")),
         age = ifelse(AGE == 999, NA, AGE),
         state = factor(STATEFIP, 
                        levels = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16,
                                   17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
                                   27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
                                   37, 38, 39, 40, 41, 42, 44, 45, 46, 47,
                                   48, 49, 50, 51, 53, 54, 55, 56),
                        labels = state.name)) |>
  filter(!is.na(state)) |>
  select(state, sex, degree, marst, sei, age)

# I want at the state level:
# * mean sei
# * mean age
# * proportion with a college degree
acs |>
  group_by(state) |>
  summarize(sei_mean = mean(sei, na.rm = TRUE),
            age_mean = mean(age, na.rm = TRUE),
            college_prop = mean(degree == "BA" | degree == "Grad", 
                                na.rm = TRUE),
            n = n())

# get conditional means by tapply
tapply(acs$sei, acs$sex, mean, na.rm=TRUE)
# get conditional means by group_by |> summarize
acs |>
  group_by(sex) |>
  summarize(mean_sei = mean(sei, na.rm=TRUE))

# group by both state and sex for analytical reasons
acs |>
  group_by(state, sex) |>
  summarize(sei_mean = mean(sei, na.rm = TRUE),
            age_mean = mean(age, na.rm = TRUE),
            college_prop = mean(degree == "BA" | degree == "Grad", 
                                na.rm = TRUE),
            n = n()) |>
  ggplot(aes(x=state, y=college_prop, color=sex))+
  geom_point()+
  coord_flip()

# Merge world bank and VDEM data ------------------------------------------

world_bank <- world_bank |>
  mutate(year = as.numeric(year))

vdem <- read_csv("stat_data/vdem/V-Dem-CY-Full+Others-v13.csv.gz") |>
  select(country_name, country_text_id, year, v2x_libdem) |>
  filter(year == 2018 | year ==2019)

# inner_join - drop all unmatched cases in both datasets
# full_join - keep everything
# left_join - keep everything from the first dataset, drop from the second
# right_join - keep everythign from the second dataset, drop from the first

temp <- full_join(world_bank, vdem, by = c("country_name", "year"))

list(`World Bank` = world_bank$country_name,
     `VDEM` = vdem$country_name) |>
  ggvenn(auto_scale = TRUE)

"bob" %in% c("bob", "sue", "harry")
"john" %in% c("bob", "sue", "harry")

unique(world_bank$country_name[!(world_bank$country_name %in% vdem$country_name)])
unique(vdem$country_name[!(vdem$country_name %in% world_bank$country_name)])

list(`World Bank` = world_bank$country_code,
     `VDEM` = vdem$country_text_id) |>
  ggvenn(auto_scale = TRUE)

unique(vdem$country_name[!(vdem$country_text_id %in% world_bank$country_code)])

vdem <- vdem |>
  rename(country_code = country_text_id) |>
  select(!country_name)

world_combined <- left_join(world_bank, vdem, by = c("country_code", "year"))
