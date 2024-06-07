load("stat_data/addhealth.RData")

model1 <- lm(nominations~nsports, data=addhealth)
model2 <- lm(nominations~nsports+parent_income, data=addhealth)
model2 <- update(model1, .~.+parent_income)
model2_weighted <- update(model2, weight=sweight)
