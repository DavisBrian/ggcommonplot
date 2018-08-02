library(tidyverse)
library(scales)
library(rlang)

source("./R/ggerrorbar.R")
# Load the data ----------------------------------------------------------------
dat <- readRDS("./data/cidat1.RDS")


colors1 <- c("use" = "grey", "nmu" = "cornflowerblue")
colors2 <- c("use" = "red", "nmu" = "blue") 
colors3 <- c("use" = "black", "nmu" = "purple")

shape1 <- c("use" = 16, "nmu" = 17)

api_colors <- c(Oxycodone        = "#000080",
                Fentanyl         = "#00FF00",
                Codeine          = "#CD5B45",
                Hydrocodone      = "#FF0000",
                Hydromorphone    = "#FF00FF",
                Morphine         = "#800080",
                Oxymorphone      = "#000000",
                Methadone        = "#00CCFF",
                Buprenorphine    = "#FF9900",
                Tramadol         = "#808080",
                Tapentadol       = "#817D00",
                Methylphenidate  = "#00FF00",
                Amphetamines     = "#FF9900",
                Ketamine         = "#458B00",
                Schedule2        = "#458B00",
                ADF              = "turquoise",
                NonADF           = "#660000")

api_lines <- c(Oxycodone        = 1,
               Fentanyl         = 2,
               Codeine          = 3,
               Hydrocodone      = 4,
               Hydromorphone    = 5,
               Morphine         = 6,
               Oxymorphone      = 1,
               Methadone        = 2,
               Buprenorphine    = 3,
               Tramadol         = 4,
               Tapentadol       = 5,
               Methylphenidate  = 6,
               Amphetamines     = 1,
               Ketamine         = 2,
               Schedule2        = 3,
               ADF              = 4,
               NonADF           = 5)

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

# Error Bar + Point + Line -----------------------------------------------------
ggerrorbar(dat, x = use_type, estimate = mean, ymin = lower, ymax = upper, group = name, 
           point = TRUE, point.size = 2L,
           line  = TRUE)

## change point.color by api color, line.color should follow
ggerrorbar(dat, x = use_type, estimate = mean, ymin = lower, ymax = upper, group = name, 
           point = TRUE, point.size = 2L, point.color = api_colors,
           line  = TRUE)

## change point.color by api color, line.color should follow
ggerrorbar(dat, x = use_type, estimate = mean, ymin = lower, ymax = upper, group = name, 
           point = TRUE, point.size = 2L, point.color = "blue",
           line  = TRUE)

## change point color by api color, line.color to blue
ggerrorbar(dat, x = use_type, estimate = mean, ymin = lower, ymax = upper, group = name, 
           point = TRUE, point.size = 2L, point.color = api_colors,
           line  = TRUE, line.color = "blue")

## change point color to red, line.color to blue
ggerrorbar(dat, x = use_type, estimate = mean, ymin = lower, ymax = upper, group = name, 
           point = TRUE, point.size = 2L, point.color = "red",
           line  = TRUE, line.color = "blue")

## change point.color by api color, line.color should follow, change linetype based on group
ggerrorbar(dat, x = use_type, estimate = mean, ymin = lower, ymax = upper, group = name, 
           point = TRUE, point.size = 2L, point.color = api_colors,
           line  = TRUE, line.linetypegroup = name, line.linetype = api_lines)

# Error Bar + Bar ------------------------------------------------------------
ggerrorbar(dat, x = name, estimate = mean, ymin = lower, ymax = upper, group = use_type,
           bar = TRUE)

## Change the errobar.color to red bar should follow
ggerrorbar(dat, x = name, estimate = mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "red",
           bar = TRUE)

## Change the errobar.color by group named vector, bar should follow
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = colors1,
           bar = TRUE)

## Change the errobar.color by group named vector, change bar.outline to "black"
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = colors1,
           bar = TRUE, bar.outline = "black")

## Change the errobar.color to "black", bar.outline should follow, change fill by group
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           bar = TRUE, bar.fill = colors1)

## Change the errobar.color to "black", change bar.outline to "red", change fill by group
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           bar = TRUE, bar.outline = "red", bar.fill = colors1)

## Change the errobar.color to "black", bar.outline should follow, change fill by group, change linetype
ggerrorbar(dat, x = name, estimate= mean, ymin = lower, ymax = upper, group = use_type,
           errorbar.color = "black",
           bar = TRUE, bar.fill = colors1, bar.linetype = "dashed")
