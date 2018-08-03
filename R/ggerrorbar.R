# [TBD] Error checking around point.shape and point.shapegroup (need both or default to group???)
# TBD
# -    Function for labels
# -    Legend
# -    rotate x/y text
# -    y-axis to the top
# -    commas, $, % in axis labels
ggerrorbar <- function(data,
                       x,
                       estimate,
                       ymin,
                       ymax,
                       group = NULL,
                       dodge_width = 0.75,
                       errorbar.alpha    = 1,
                       errorbar.color    = "black",
                       errorbar.linetype = 1,
                       errorbar.size     = 0.5,
                       errorbar.width    = 0.5,
                       bar.alpha    = 1,
                       bar.fill     = errorbar.color,
                       bar.linetype = 1,
                       bar.outline  = errorbar.color,
                       bar.size     = .5,
                       bar.width    = 0.75,
                       point.alpha      = 1,
                       point.color      = errorbar.color,
                       point.fill       = NA,
                       point.size       = 0.5,
                       point.shape      = 16,
                       point.shapegroup = NULL,
                       point.na_remove  = FALSE,
                       line.alpha = 1,
                       line.color = point.color,
                       line.linetype = "solid",
                       line.linetypegroup = NULL,
                       line.size  = .5,
                       xlab     = NULL,
                       ylab     = NULL,
                       caption  = NULL,
                       title    = NULL,
                       subtitle = NULL,
                       point = FALSE,
                       bar   = FALSE,
                       line  = FALSE,
                       scale.y.name   = NULL,
                       scale.y.breaks = waiver(),
                       scale.y.limits = NULL, 
                       scale.y.label  = waiver(),
                       scale.y.trans  = NULL,
                       scale.y.expand = c(0,0),
                       theme = NULL) {
  
  # take unquoted column names and make variables -----------------------------
  x_quo    <- rlang::enquo(x)
  est_quo  <- rlang::enquo(estimate)
  ymin_quo <- rlang::enquo(ymin)
  ymax_quo <- rlang::enquo(ymax)
  
  group_quo <- rlang::enquo(group)
  shape_quo <- rlang::enquo(point.shapegroup)
  ltype_quo <- rlang::enquo(line.linetypegroup)
  
  # preparing x/y labels from given dataframe ----------------------------------
  lab.df <- colnames(dplyr::select(
    .data = data,
    !!rlang::enquo(x),
    !!rlang::enquo(estimate)
  ))
  
  # if xlab is not provided, use the variable x name
  if (is.null(xlab)) {
    xlab <- lab.df[[1]]
  }
  
  # if ylab is not provided, use the variable y name
  if (is.null(ylab)) {
    ylab <- lab.df[[2]]
  }
  
  # prepare the named vectors --------------------------------------------------
  errorbar_color_named <- !is.null(names(errorbar.color))
  
  point_color_named    <- !is.null(names(point.color)) 
  point_shape_named    <- !is.null(names(point.shape))
  
  line_color_named     <- !is.null(names(line.color))
  linetype_named       <- !is.null(names(line.linetype))
  
  bar_fill_named       <- !is.null(names(bar.fill))
  bar_outline_named    <- !is.null(names(bar.outline))
  
  # Plot Set up ----------------------------------------------------------------
  p <- ggplot2::ggplot(data,
                       aes(x     = !! x_quo, 
                           y     = !! ymax_quo, 
                           group = !! group_quo)) +
    ggplot2::labs(x        = xlab,
                  y        = ylab,
                  title    = title,
                  subtitle = subtitle,
                  caption  = caption) 
  
  # Bar ------------------------------------------------------------------------
  if(bar) {
    if (bar_outline_named & bar_fill_named) {
      p <- p + ggplot2::geom_col(aes(y     = !!est_quo,
                                     color = !!group_quo,
                                     fill  = !!group_quo),
                                 position = position_dodge(dodge_width), 
                                 width    = bar.width,
                                 size     = bar.size,
                                 alpha    = bar.alpha,
                                 linetype = bar.linetype)
    } else if (bar_outline_named & !bar_fill_named) {
      p <- p + ggplot2::geom_col(aes(y     = !!est_quo,
                                     color = !!group_quo),
                                 position = position_dodge(dodge_width), 
                                 width    = bar.width,
                                 size     = bar.size,
                                 alpha    = bar.alpha,
                                 fill     = bar.fill,
                                 linetype = bar.linetype)
    } else if (!bar_outline_named & bar_fill_named) {
      p <- p + ggplot2::geom_col(aes(y     = !!est_quo,
                                     fill  = !!group_quo),
                                 position = position_dodge(dodge_width), 
                                 width    = bar.width,
                                 size     = bar.size,
                                 alpha    = bar.alpha,
                                 color    = bar.outline,
                                 linetype = bar.linetype)
    } else {
      p <- p + ggplot2::geom_col(aes(y = !!est_quo),
                                 position = position_dodge(dodge_width), 
                                 width    = bar.width,
                                 size     = bar.size,
                                 alpha    = bar.alpha,
                                 color    = bar.outline,
                                 fill     = bar.fill,
                                 linetype = bar.linetype)    
    }
  }
    
  # Error Bar ------------------------------------------------------------------
  # if the a named vector is used for the errorbar.color use scale_color_manual
  if (errorbar_color_named) {
    p <- p + ggplot2::geom_errorbar(aes(ymin  = !! ymin_quo, 
                                        ymax  = !! ymax_quo,
                                        color = !! group_quo),
                                    position = position_dodge(dodge_width), 
                                    size     = errorbar.size,
                                    linetype = errorbar.linetype,
                                    width    = errorbar.width,
                                    alpha    = errorbar.alpha)
  } else{
    p <- p + ggplot2::geom_errorbar(aes(ymin  = !! ymin_quo, 
                                        ymax  = !! ymax_quo),
                                    position = position_dodge(dodge_width),
                                    size     = errorbar.size,
                                    linetype = errorbar.linetype,
                                    color    = errorbar.color,
                                    width    = errorbar.width,
                                    alpha    = errorbar.alpha)
  }
  
  # Points ---------------------------------------------------------------------
  # points.color should default to the same as errorbar.color.  Also, 
  # should be able to set the points a constant color and the errorbar by group
  # and the errorbar to a constant color and the points by the grouping variable
  # 
  if (point) {
    if (point_color_named & point_shape_named) {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !!group_quo,
                                       shape = !!shape_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
    } else if (point_color_named & !point_shape_named) {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !! group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   shape    = point.shape,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
    } else if (!point_color_named & point_shape_named) {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !!group_quo,
                                       shape = !!shape_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   color    = point.color,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove) 
    } else {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !! group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   color    = point.color,
                                   shape    = point.shape,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
    }
  }
  
  # Line ---------------------------------------------------------------------
  # line.color should default to the same as errorbar.color.  Also, 
  # should be able to set the lines to a constant color and the errorbar by group
  # and the errorbar to a constant color and the lines by the grouping variable
  # 
  if (line) {
    if (line_color_named & linetype_named) {
      p <- p + ggplot2::geom_line(aes(y        = !!est_quo,
                                      color    = !!group_quo,
                                      linetype = !!ltype_quo),
                                   position = position_dodge(dodge_width),
                                   size     = line.size,
                                   alpha    = line.alpha)
    } else if (line_color_named & !linetype_named) {
      p <- p + ggplot2::geom_line(aes(y     = !!est_quo,
                                      color = !! group_quo),
                                  position = position_dodge(dodge_width),
                                  size     = line.size,
                                  linetype = line.linetype,
                                  alpha    = line.alpha)
    } else if (!line_color_named & linetype_named) {
      p <- p + ggplot2::geom_line(aes(y        = !!est_quo,
                                      color    = !!group_quo,
                                      linetype = !!ltype_quo),
                                   position = position_dodge(dodge_width),
                                   size     = line.size,
                                   color    = line.color,
                                   alpha    = line.alpha) 
    } else {
      p <- p + ggplot2::geom_line(aes(y     = !!est_quo,
                                      color = !! group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = line.size,
                                   color    = line.color,
                                   linetype = line.linetype,
                                   alpha    = line.alpha)
    }
    
  }
  
  # Named colors ---------------------------------------------------------------
  if (errorbar_color_named) {
    p <- p + ggplot2::scale_color_manual(values = errorbar.color)
  } else if (length(errorbar.color) == 1L & point_color_named) {
    p <- p + ggplot2::scale_color_manual(values = point.color)
  } else if (length(errorbar.color) == 1L & line_color_named) {
    p <- p + ggplot2::scale_color_manual(values = line.color)
  } else if (length(errorbar.color) == 1L & bar_outline_named) {
    p <- p + ggplot2::scale_color_manual(values = bar.outline)
  }
  
  # Named point shapes ---------------------------------------------------------
  if (point_shape_named) {
    p <- p + ggplot2::scale_shape_manual(values = point.shape)
  }
  
  # Named linetypes ------------------------------------------------------------
  if (linetype_named) {
    p <- p + ggplot2::scale_linetype_manual(values = line.linetype)
  }
  
  # Named bar fill colors ------------------------------------------------------
  if (bar_fill_named) {
    p <- p + ggplot2::scale_fill_manual(values = bar.fill)
  }
  
  # scale_y_continuous ---------------------------------------------------------
  # Y label defaults -----------------------------------------------------------
  if (is.null(scale.y.limits)) {
    y <- pretty(pull(data, !!ymax_quo))
    gap <- 10^(ceiling(log10(max(y))) -1L)
    scale.y.limits <- c(0, max(y))
    scale.y.breaks <- seq(0, max(y), by = gap)
    scale.y.label <- scale.y.breaks
    
    # scale.y.expand <-  c(min(y), max(y))
  }

  p <- p + scale_y_continuous(expand = scale.y.expand,
                              breaks = scale.y.breaks, 
                              label  = scale.y.label, 
                              limits = scale.y.limits)

  
  # add theme ------------------------------------------------------------------
  if (!is.null(theme)) {
    p <- p + theme
  }
  
  p
}
