######################
# Learning R Script
# Aaron Gullickson
# Soc 412/512
# Winter 2024
######################

# Load Libraries and Data -------------------------------------------------



# Object Types ------------------------------------------------------------

# Atomic types

bob <- 3 # numeric
sue <- "ice storm" # character string
tom <- TRUE # logical - boolean - true/false

BOB <- 2

bob+BOB

# recast
as.character(bob)
as.numeric("15")
as.numeric("fifteen")
as.numeric(tom)

# Vectors

name <- c("Joad", "Romaer", "Dilith", "Starling", "Usche")

name[4] # fourth name
name[c(1,4)] # first and fourth name
name[3:5] # third through fifth name

lvl <- c(3, 4, 2, 10, 2)

sum(lvl)
mean(lvl)
median(lvl)
sd(lvl)

# Factor

char_class <- c("Artificer","Bard","Paladin","Fighter","Warlock")

char_class <- factor(char_class,
                     levels=c("Artificer","Barbarian", "Bard", "Cleric",
                              "Druid", "Fighter", "Paladin", "Ranger", "Rogue",
                              "Sorcerer", "Warlock", "Wizard"))

