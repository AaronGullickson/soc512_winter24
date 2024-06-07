# Interaction terms

load("stat_data/popularity.RData")
load("stat_data/earnings.RData")

model1 <- lm(nominations ~ smoker + pseudo_gpa, data=popularity)
model2 <- lm(nominations ~ smoker * pseudo_gpa, data=popularity)
model2 <- lm(nominations ~ (alcohol_use + smoker) + I(pseudo_gpa-3), data=popularity)

model1 <- lm(nominations ~ alcohol_use , data=popularity)
model2 <- lm(nominations ~ alcohol_use+I(pseudo_gpa-3), data=popularity)
model3 <- lm(nominations ~ alcohol_use*I(pseudo_gpa-3), data=popularity)
modelsummary(list(model1, model2, model3), stars=TRUE)

model1 <- lm(wages~gender+nchild, data=earnings)
model2 <- lm(wages~gender*nchild, data=earnings)
