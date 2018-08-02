# [TBD] Error checking around point.shape and point.shapegroup (need both or default to group???)
ggerrorbar <- function(data,
                     x, 
                     estimate, 
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
                     point.shapegroup = NULL,      # not implemented
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
  est_quo  <- rlang::enquo(estimate)
  ymin_quo <- rlang::enquo(ymin)
  ymax_quo <- rlang::enquo(ymax)
  
  group_quo <- rlang::enquo(group)
  shape_quo <- rlang::enquo(point.shapegroup)
  
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
  
  # Plot Set up ----------------------------------------------------------------
  p <- ggplot2::ggplot(data,
                       aes(x     = !! x_quo, 
                           y     = !! ymax_quo, 
                           group = !! group_quo)) +
    ggplot2::labs(x       = xlab,
                  y       = ylab,
                  title   = title,
                  caption = caption) +
    ggplot2::scale_y_continuous(expand = c(0,0))   # move the x axis to the plot bottom
    
  # Error Bar ------------------------------------------------------------------
  # if the a named vector is used for the errorbar.color use scale_color_manual
  if(errorbar_color_named) {
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
  if(point) {
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
  
  if(errorbar_color_named) {
    p <- p + ggplot2::scale_color_manual(values=errorbar.color)
  } else if (length(errorbar.color) == 1L & point_color_named) {
    p <- p + ggplot2::scale_color_manual(values=point.color)
  }
  
  if(point_shape_named) {
    p <- p + ggplot2::scale_shape_manual(values = point.shape)
  }
  
  p
}
