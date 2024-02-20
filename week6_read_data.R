#######
# Week 6 - Read data
#######


# Load libraries ----------------------------------------------------------

# you will need to install these to have access to them
library(readr)       # read in text files
library(haven)       # read in binary files from stata, sas, and spss
library(readxl)      # read in Excel files
library(tidyverse)
# uncomment next line to see how to deal with excel nonsense
#library(stringr)     # manipulate character strings

# Read CSV ----------------------------------------------------------------

counties <- read_csv("stat_data/social_explorer/R13583091_SL050.csv", 
                     skip = 1, # skip first row
                     col_types = cols(.default = "i", # specify column type
                                      Geo_QName = "c",
                                      Geo_STUSAB = "c",
                                      Geo_NAME = "c"))

# rename variables and just take the ones we want, leaving junk behind
counties <- counties |> 
  rename(fips = Geo_FIPS, county_name = Geo_QName, pop_total = SE_A04001_001) |>
  select(fips, county_name, pop_total)


# Read fwf ----------------------------------------------------------------

# Look in stat_data/gss/GSS.dct for the starting positions
gss <- read_fwf("stat_data/gss/GSS.dat",
                # two numbers give starting and ending positions
                col_positions = fwf_cols(SEX = c(11, 14),   
                                         ABANY = c(15, 19)),
                col_types = cols(.default = "i"))


# Read xslx ---------------------------------------------------------------

gss <- read_excel("stat_data/gss/GSS.xlsx",
                  na = c(".d:  Do not Know/Cannot Choose", 
                         ".i:  Inapplicable",
                         ".n:  No answer",
                         ".s:  Skipped on Web"))

# the labeling of certain vallues in age and educ still forces those variables
# to character strings. Uncomment out the following and stringr library above
# to see how to deal with it.

#gss <- gss |>
#  mutate(age = as.numeric(str_remove(age, " or older")),
#         educ = as.numeric(ifelse(educ == "No formal schooling", 0, educ)))
