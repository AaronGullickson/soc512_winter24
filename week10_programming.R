


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Load tract data ---------------------------------------------------------
tracts <- read_csv("stat_data/social_explorer/R13598833_SL140.csv",
                   col_types = cols(.default = "i", 
                                    Geo_QName = "c",
                                    Geo_NAME = "c",
                                    Geo_STUSAB = "c",
                                    Geo_FIPS = "c")) |>
  mutate(pop_race_indigenous = SE_B04001_005 + SE_B04001_007,
         county_id = Geo_STATE * 1000 + Geo_COUNTY,
         county_name = str_remove(Geo_QName, paste0(Geo_NAME, ", ")),
         tract_id = as.numeric(Geo_FIPS)) |>
  rename(pop_total = SE_B04001_001,
         pop_race_white = SE_B04001_003,
         pop_race_black = SE_B04001_004,
         pop_race_asian = SE_B04001_006,
         pop_race_other = SE_B04001_008,
         pop_race_multi = SE_B04001_009,
         pop_race_latino = SE_B04001_010) |>
  select(tract_id, starts_with("county_"), starts_with("pop_")) |>
  filter(pop_total > 0)




# Create a custom function ------------------------------------------------

print_this <- function(x, exclamation = FALSE) {
  
  ## write some code for the function ##
  print(x)
  if(exclamation) {
    print("!")
  }
  
  ## return something ##
  #return(some_value)
}

print_this(2)
print_this("Kwisatz Haderach")
print_this(2+2)
print_this(2, exclamation = TRUE)

factorial_this <- function(x) {
  return(factorial(x))
}

factorial_this(5)
factorial_this(10)


# Calculate Theil's H for a Casee --------------------------------------------

tracts_multnomah <- tracts |>
  filter(county_name == "Multnomah County, Oregon")

pop_county <- sum(tracts_multnomah$pop_total)

prop_race_county <- tracts_multnomah |>
  select(starts_with("pop_race_")) |>
  colSums() |>
  prop.table()

entropy_county <- sum(prop_race_county * log(1 / prop_race_county), 
                      na.rm = TRUE)

#tracts_multnomah |>
#  mutate(prop_white = pop_race_white / pop_total,
#         prop_black = pop_race_black / pop_total,
#         ...,
#         entropy = prop_white * log(1/prop_white)+...)

tracts_multnomah <- tracts_multnomah |>
  pivot_longer(cols = starts_with("pop_race_"), names_prefix = "pop_race_",
               names_to = "race", values_to = "pop") |>
  mutate(prop = pop / pop_total,
         e = prop * log(1 / prop)) |>
  group_by(tract_id) |>
  summarize(entropy = sum(e, na.rm = TRUE), pop_total = sum(pop))

1 - sum(tracts_multnomah$pop_total * tracts_multnomah$entropy)/
  (pop_county * entropy_county)

# Create function for theil's H -------------------------------------------

calc_theil_h <- function(tracts_county) {
  
  pop_county <- sum(tracts_county$pop_total)
  
  prop_race_county <- tracts_county |>
    select(starts_with("pop_race_")) |>
    colSums() |>
    prop.table()
  
  entropy_county <- sum(prop_race_county * log(1 / prop_race_county), 
                        na.rm = TRUE)
  
  tracts_county <- tracts_county |>
    pivot_longer(cols = starts_with("pop_race_"), names_prefix = "pop_race_",
                 names_to = "race", values_to = "pop") |>
    mutate(prop = pop / pop_total,
           e = prop * log(1 / prop)) |>
    group_by(tract_id) |>
    summarize(entropy = sum(e, na.rm = TRUE), pop_total = sum(pop))
  
  return(1 - sum(tracts_county$pop_total * tracts_county$entropy)/
           (pop_county * entropy_county))
}

tracts |>
  filter(county_name == "Multnomah County, Oregon") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Whitman County, Washington") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Wayne County, Michigan") |>
  calc_theil_h()

tracts |>
  filter(county_name == "King County, Washington") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Milwaukee County, Wisconsin") |>
  calc_theil_h()

tracts |>
  filter(county_name == "New York County, New York") |>
  calc_theil_h()
