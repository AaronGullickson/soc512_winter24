load("stat_data/addhealth.RData")
library(tidyverse)

summary(addhealth$parent_income)

model <- lm(sqrt(parent_income)~grade+race+gender+alcohol_use+smoker+pseudo_gpa+
              honor_society+bandchoir+nsports, data=addhealth)

addhealth$yhat <- predict(model, addhealth)^2
summary(addhealth$yhat)

addhealth <- addhealth |>
  mutate(parent_income_regi=ifelse(is.na(parent_income), yhat, parent_income))

summary(addhealth$parent_income_regi)

library(mice)

temp <- mice(addhealth, 1)
