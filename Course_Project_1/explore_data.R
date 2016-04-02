
library(dplyr)
library(ggplot2)
library(datasets)

names(mtcars)
str(mtcars)

help(mtcars)
# [, 9]	 am	 Transmission (0 = automatic, 1 = manual)
cars1 <- mtcars 
cars1$vs <- factor(cars1$vs, levels = c("0","1"), labels = c("automatic", "manual"))

g1 <- ggplot(data = cars1,
             mapping = aes(x = vs, y = mpg)) + 
    geom_boxplot() + 
    labs(title = "mpg for automatic and manual cars",
         x = "transmission")
g1

# It appears from the plot that the mpg is very different for automatic and manual.
# I have an feeling cylinder has a part to play as well.

cars1$cyl <- factor(cars1$cyl, levels = c("4","6","8"))

g2 <- ggplot(data = cars1,
             mapping = aes(x = vs, y = mpg)) + 
    geom_boxplot(aes(fill = cyl)) + 
    labs(title = "mpg for automatic and manual cars",
         x = "transmission")
g2

# oooohhhhhh, now we're getting a different picture

# 

mdl1 <- lm(formula = mpg ~ cyl, data = mtcars)
mdl1
summary(mdl)

mdl2 <- lm(formula = mpg ~ cyl + vs, data = mtcars)
mdl2
summary(mdl2)

# Adding vs (transmission) decreases the R-squared value indicating is doesn't improve our model.
# Does adding vs (transmission) increase the variance?

anova(mdl1, mdl2)

# The deviance (RSS residual sim of squares) decreases slighty.

g3 <- ggplot(data = mtcars,
             mapping = aes(x = cyl, y = mpg)) +
    geom_point(mapping = aes(colour = factor(vs, levels = c("0","1"), labels = c("automatic","manual")))) +
    geom_smooth(method = "lm") + 
    labs(title = "model 1", colour = "transmission")
g3




