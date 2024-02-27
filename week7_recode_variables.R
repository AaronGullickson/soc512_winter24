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



# Get final dataset -------------------------------------------------------


acs <- acs |>
  select(sei)

save(acs, file="acs.RData")
