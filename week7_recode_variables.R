######
# Week 7 - Recode variables
######

# Load libraries ------------------------------------------------------------

library(tidyverse)
library(readr)

# Load data ---------------------------------------------------------------

acs <- read_fwf("stat_data/ipums/usa_00131.dat.gz",
                col_positions = fwf_cols(SEX = c(53, 53),
                                         AGE = c(54, 56),
                                         MARST = c(57, 57),
                                         EDUCD = c(69, 71),
                                         SEI = c(72, 73)),
                col_types = cols(.default = "i"))

# File Type:                    rectangular
# Case Selection:               No
# Variable                         Columns        Len   2022    
# CBSERIAL                     H   1-13          13     X       
# HHWT                         H  14-23          10     X       
# CLUSTER                      H  24-36          13     X       
# STATEFIP                     H  37-38           2     X       
# PERNUM                       P  39-42           4     X       
# PERWT                        P  43-52          10     X       
# SEX                          P  53              1     X       
# AGE                          P  54-56           3     X       
# MARST                        P  57              1     X       
# RACE                         P  58              1     X       
# RACED                        P  59-61           3     X       
# HISPAN                       P  62              1     X       
# HISPAND                      P  63-65           3     X       
# HCOVANY                      P  66              1     X       
# EDUC                         P  67-68           2     X       
# EDUCD                        P  69-71           3     X       
# SEI                          P  72-73           2     X       

# Recode variables --------------------------------------------------------

# encode missing values for SEI
# bracket and boolean
#acs$SEI[!is.na(acs$SEI) & acs$SEI == 0] <- NA #Don't use!
# ifelse approach
#acs$sei <- ifelse(acs$SEI == 0, NA, acs$SEI)

acs <- acs |>
  mutate(sei = ifelse(SEI == 0, NA, SEI))

# check yourself
table(acs$SEI==0, acs$sei==0, exclude=NULL)

# encode sex into a factor variable
#SEX                 Sex
#1                   Male
#2                   Female
#9                   Missing/blank
acs <- acs |>
  mutate(sex = factor(SEX,
                      levels = c(1, 2),
                      labels = c("Male", "Female")))

# case_when option
acs <- acs |>
  mutate(sex = factor(case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  )))

# case when just applies nested ifelse statements, which you could do by hand
# but you shouldn't!
#factor(ifelse(acs$SEX == 1, "Male", 
#              ifelse(acs$SEX == 2, "Female", NA)))

# check yourself
table(acs$SEX, acs$sex, exclude = NULL)

# encode education into categorical variable
acs <- acs |>
  mutate(degree = factor(case_when(
    EDUCD <= 1 | EDUCD == 999 ~ NA,
    EDUCD < 62 ~ "LHS",
    EDUCD < 81 ~ "HS",
    EDUCD < 101 ~ "AA",
    EDUCD < 114 ~ "BA",
    TRUE ~ "Grad"), 
    levels = c("LHS", "HS", "AA", "BA", "Grad")))

table(acs$EDUCD, acs$degree, exclude = NULL)

# encode marital status
acs <- acs |>
  mutate(marst = factor(case_when(
    MARST == 9 ~ NA,
    MARST <= 2 ~ "Married",
    MARST <= 4 ~ "Divorced/Separated",
    MARST == 5 ~ "Widowed",
    MARST == 6 ~ "Never Married"
  ), 
  levels = c("Never Married", "Married", "Divorced/Separated", "Widowed")))

table(acs$MARST, acs$marst, exclude = NULL)

# encode missing values for age
acs <- acs |>
  mutate(age = ifelse(AGE == 999, NA, AGE))

table(acs$age, acs$AGE==999, exclude=NULL)

# Get final dataset -------------------------------------------------------


acs <- acs |>
  select(age, sei, sex, degree, marst)

save(acs, file="acs.RData")
