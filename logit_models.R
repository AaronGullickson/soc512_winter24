# logit models!
library(modelsummary)
load("stat_data/titanic.RData")

titanic$survival <- relevel(titanic$survival, "Died")
titanic$sex <- relevel(titanic$sex, "Male")

model1 <- glm(survival~sex, data=titanic, family=binomial)
model2 <- update(model1, .~.+fare)
model3 <- update(model1, .~.+pclass)

modelsummary(list(model1, model2, model3))
modelsummary(list(model1, model2, model3), exponentiate = TRUE)

modelsummary(lapply(list(model1, model2, model3), margins))

model1_lpm <- lm(I(survival == "Survived") ~ sex, data=titanic)
model2_lpm <- lm(I(survival == "Survived") ~ sex + pclass, data=titanic)


modelsummary(list(model1_lpm, model2_lpm))
