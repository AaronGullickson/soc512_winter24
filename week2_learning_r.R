######################
# Learning R Script
# Aaron Gullickson
# Soc 412/512
# Winter 2024
######################

# Load Libraries and Data -------------------------------------------------

library(gapminder)


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

char_class <- factor(char_class)

summary(char_class)

char_class <- c("Artificer","Bard","Paladin","Fighter","Warlock")

char_class <- factor(char_class,
                     levels=c("Artificer","Barbarian", "Bard", "Cleric","Druid",
                              "Fighter", "Paladin", "Ranger", "Rogue",
                              "Sorcerer", "Warlock", "Wizard"))

summary(char_class)

# see levels
levels(char_class)

# see the numeric values
as.numeric(char_class)

# reorder
char_class <- relevel(char_class, "Warlock")

# we can turn any type into a factor
inspired <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
inspired <- factor(inspired, 
                   levels=c(FALSE, TRUE),
                   labels=c("Not Inspired", "Inspired"))

summary(inspired)

# Data.frame

chars_df <- data.frame(name, level=lvl, char_class, inspiration=inspired)

# to access a variable
mean(chars_df$level)

# second row, second and third variable
chars_df[2, c(2,3)]

# its better to reference columns by name
chars_df[2, c("level","char_class")]

summary(chars_df)


# Functions ---------------------------------------------------------------

# function-form: function_name(arg1, arg2, name_arg=arg5, ...)

mean(chars_df$level)

# get help!
?mean

chars_df$level[4] <- NA

mean(chars_df$level)

# specify a named argument
mean(chars_df$level, na.rm=TRUE)


# Boolean Statements ------------------------------------------------------

chars_df$level[4] <- 10

# which observations are level 4 or higher?
chars_df$level >= 4

# boolean operators
# >= greater than or equal
# <= less than or equal
# > greater than
# < less than
# == equal to
# != not equal to

chars_df$char_class == "Bard"

# Compound boolean statement with AND (&)
chars_df$level >= 4 & chars_df$char_class == "Bard"

# compound boolean statement with OR (|)
chars_df$level >= 4 | chars_df$char_class == "Bard"

# use parenthesis to clarify ordering
chars_df$level >= 4 & (chars_df$char_class == "Bard" | 
                         chars_df$inspiration == "Inspired")

# specify numeric range
chars_df$level >= 3 & chars_df$level <=6

# subset command to drop observations
chars_df <- subset(chars_df, level >= 3 & level <= 6)

# Managing Packages -------------------------------------------------------

# don't put this in your script! install.packages("gapminder")

# reference a function without loading library

usethis::git_sitrep()
