library(tidyverse)
library(scales)
library(rlang)


# Load the data ----------------------------------------------------------------
dat <- readRDS("./data/cidat1.RDS")


colors1 <- c("use" = "grey", "nmu" = "cornflowerblue")
colors2 <- c("use" = "red", "nmu" = "blue") 
colors3 <- c("use" = "black", "nmu" = "purple")

shape1 <- c("use" = 16, "nmu" = 17)


# Error Bar --------------------------------------------------------------------
ggerrorbar(dat, x = name, estimate = mean, ymin = lower, ymax = upper, group = use_type)

## Change the color to red
ggerrorbar(dat, x = name, estimate = mean, ymin = lower, ymax = upper, group = use_type,
         errorbar.color = "red")

## Change the color based on group
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
         errorbar.color = colors1)


# Error Bar + Point ------------------------------------------------------------
ggerrorbar(dat, x = name, estimate = mean, ymin = lower, ymax = upper, group = use_type,
           point = TRUE, point.size = 2L)


## Change the errobar.color to red point should follow
ggerrorbar(dat, x = name, estimate = mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "red",
           point = TRUE, point.size = 2L)

## Change the errobar.color by group named vector, point should follow
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = colors1,
           point = TRUE, point.size = 2L)

## Change the errobar.color by group named vector, change point.color to "red"
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = colors1,
           point = TRUE, point.size = 2L, point.color = "red")

## Change the errobar.color to "black", change point.color to "red"
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           point = TRUE, point.size = 2L, point.color = "red")

## Change the errobar.color to "black", change point.color by group named vector
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           point = TRUE, point.size = 2L, point.color = colors1)

## Change the errobar.color to "black", change point.color by group named vector + change shape to 21 (open circle)
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           point = TRUE, point.size = 2L, point.color = colors1, point.shape = 21)

## Change the errobar.color to "black", change point.color by group named vector + change shape by group
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           point = TRUE, point.size = 2L, point.color = colors1, point.shape = shape1)

## Change the errobar.color to "black", change point.color by group named vector + change shape by group
### [TBD] Need to figure out how to default this to the same column as group
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           point = TRUE, point.size = 2L, point.color = colors1, point.shape = shape1, point.shapegroup = use_type)

# Error Bar + Point + Line