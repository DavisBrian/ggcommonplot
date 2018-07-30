library(tidyverse)
library(scales)
library(rlang)


# trademark lookup -------------------------------------------------------------

# api lookup -------------------------------------------------------------------
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



# report functions -------------------------------------------------------------

use_type <- tribble(
  
  ~level, ~label,
  "nmu", "Lifetime Non-Medical Use", 
  "nmu_yr", "Last 12 Month Non-Medical Use",
  "nmu_nty", "Last 90 Day Non-Medical Use"
)



# Read in data sets --------------------------------------------------------
# TBD 
# pass aes or y, x, ymin, ymax
# color
# shape
# linetype
# x/y label
# http://sape.inf.usi.ch/quick-reference/ggplot2/size
ciplot <- function(data,
                   x, 
                   y, 
                   ymin, 
                   ymax, 
                   group = NULL,
                   dodge_width = 0.75, 
                   errorbar.size = 0.5,
                   errorbar.linetype = 1,
                   errorbar.color = "black",
                   errorbar.width = 0.5,
                   errorbar.alpha = 1,
                   bar.width = 0.75,
                   bar.color = NULL,
                   bar.outline = NULL,
                   bar.size = .5,
                   bar.alpha = 1,
                   point.size = 0.5,
                   point.shape = 16,
                   point.color = errorbar.color,
                   point.fill = NA,
                   point.alpha = 1,
                   point.na_remove = FALSE,
                   xlab = NULL,
                   ylab = NULL,
                   caption = NULL,
                   title = NULL,
                   point = FALSE,
                   bar   = FALSE,
                   line  = FALSE) {

  # take unquoted column names and make variables -----------------------------
  x_quo    <- rlang::enquo(x)
  y_quo    <- rlang::enquo(y)
  ymin_quo <- rlang::enquo(ymin)
  ymax_quo <- rlang::enquo(ymax)

  group_quo <- rlang::enquo(group)
  
  # preparing x/y labels from given dataframe ----------------------------------
  lab.df <- colnames(dplyr::select(
    .data = data,
    !!rlang::enquo(x),
    !!rlang::enquo(y)
  ))
  
  # if xlab is not provided, use the variable x name
  if (is.null(xlab)) {
    xlab <- lab.df[[1]]
  }
  
  # if ylab is not provided, use the variable y name
  if (is.null(ylab)) {
    ylab <- lab.df[[2]]
  }
  

  # Plot Set up ----------------------------------------------------------------
  p <- ggplot2::ggplot(data,
                       aes(x     = !! x_quo, 
                           y     = !! y_quo, 
                           group = !! group_quo,
                           fill  = !! group_quo,
                           color = !! group_quo)) +
    ggplot2::labs(x       = xlab,
                  y       = ylab,
                  title   = title,
                  caption = caption) +
    ggplot2::scale_y_continuous(expand = c(0,0))   # move the x axis to the plot bottom
  
  
  # Bar ------------------------------------------------------------------------
  if(bar) {
    if(is.null(bar.outline)) {
      p <- p + ggplot2::geom_col(position = position_dodge(dodge_width), 
                                 width = bar.width,
                                 size  = bar.size,
                                 alpha = bar.alpha)
    } else {
      p <- p + ggplot2::geom_col(position = position_dodge(dodge_width), 
                                 width = bar.width,
                                 size  = bar.size,
                                 alpha = bar.alpha,
                                 color = bar.outline)
    }

    if(!is.null(bar.color)) {
      p <- p + ggplot2::scale_fill_manual(values=bar.color)
    }
 
  }
  
  # Lines ----------------------------------------------------------------------
  if(line) {
    p <- p +   ggplot2::geom_line(aes(group = !! group_quo), 
                                  position = position_dodge(dodge_width))
  }
  
  
  # Error Bar ------------------------------------------------------------------
  if(length(errorbar.color) == 1L){
    p <- p + ggplot2::geom_errorbar(aes(ymin  = !! ymin_quo, 
                                        ymax  = !! ymax_quo),
                                    position = position_dodge(dodge_width),
                                    size     = errorbar.size,
                                    linetype = errorbar.linetype,
                                    color    = errorbar.color,
                                    width    = errorbar.width,
                                    alpha    = errorbar.alpha)
  } else if(length(errorbar.color) > 1L) {
    p <- p + ggplot2::geom_errorbar(aes(ymin  = !! ymin_quo, 
                                        ymax  = !! ymax_quo,
                                        color = !! group_quo),
                                    position = position_dodge(dodge_width), 
                                    size     = errorbar.size,
                                    linetype = errorbar.linetype,
                                    width    = errorbar.width,
                                    alpha    = errorbar.alpha) +
      ggplot2::scale_color_manual(values=errorbar.color)
  } else {
    stop("Error bar color missing")
  }

  # Points ---------------------------------------------------------------------
  if(point) {
    if(length(point.color) == 1L){
      p <- p + ggplot2::geom_point(position = position_dodge(dodge_width),
                                   size     = point.size,
                                   shape    = point.shape,
                                   color    = point.color,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
    } else if(length(point.color) != 1L) {
      p <- p + ggplot2::geom_point(aes(color = !! group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   shape    = point.shape,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove) 
    } 

  }
  
  p
}

# Test Function ----------------------------------------------------------------

dat1 <- readRDS("./data/cidat1.RDS")

# dat1 <- dat1 %>%
#   mutate(group = factor(use_type, 
#                            levels = c("use", "nmu"), 
#                            labels = c("Lifetime Use", "Lifetime Non-Medical Use")),
#          use_type = factor(use_type, levels = c("use", "nmu")))

colors1 <- c("use" = "grey", "nmu" = "cornflowerblue")
colors2 <- c("use" = "red", "nmu" = "blue") 
colors3 <- c("use" = "black", "nmu" = "purple")


ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, point = TRUE)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, point = TRUE, group = use_type)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, point = TRUE, group = use_type,
       point.color = "red", point.size = 3)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, point = TRUE, group = use_type,
       point.color = "green", point.size = 3,
       errorbar.color = colors2)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, point = TRUE, group = use_type,
       point.size = 2,
       errorbar.color = colors1)

ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, point = TRUE, group = use_type,
       point.color = colors3, point.size = 3)

ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       point.color = c("use" = "grey", "nmu" = "cornflowerblue"), point.size = 3,
       errorbar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       xlab = "", ylab = "Prevalence % (95% CI)")

ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, bar = TRUE, group = use_type)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, xlab = "", ylab = "Prevalence % (95% CI)")
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, bar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       bar.outline = "black", bar.alpha = .75,
       xlab = "", ylab = "Prevalence % (95% CI)")
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, bar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       xlab = "", ylab = "Prevalence % (95% CI)")
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, bar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       bar.outline = "black", bar.alpha = .75,
       errorbar.color = c("use" = "red", "nmu" = "green"),
       xlab = "", ylab = "Prevalence % (95% CI)")

ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, bar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       bar.outline = "black", bar.alpha = .75,
       errorbar.color = "red",
       xlab = "", ylab = "Prevalence % (95% CI)")

ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, bar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       bar.outline = "black", bar.alpha = .75,
       errorbar.color = c("use" = "grey", "nmu" = "cornflowerblue"),
       xlab = "", ylab = "Prevalence % (95% CI)")


ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, group = use_type, 
       bar = TRUE, bar.color = bar_colors <- c("use" = "grey", "nmu" = "cornflowerblue"),
       bar.outline = "black", bar.alpha = .75,
       xlab = "", ylab = "Prevalence % (95% CI)")

ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper, line = TRUE)


ciplot(dat4, x = name, y = mean, ymin = lower, ymax = upper)
ciplot(dat3, x = name, y = mean, ymin = lower, ymax = upper)
ciplot(dat2, x = name, y = mean, ymin = lower, ymax = upper)
ciplot(dat1, x = name, y = mean, ymin = lower, ymax = upper)