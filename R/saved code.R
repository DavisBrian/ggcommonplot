if(point) {
  if (is.null(names(point.color))) {
    p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                     color = !! group_quo),
                                 position = position_dodge(dodge_width),
                                 size     = point.size,
                                 color    = point.color,
                                 shape    = point.shape,
                                 alpha    = point.alpha,
                                 na.rm    = point.na_remove)
  } else {
    p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                     color = !! group_quo),
                                 position = position_dodge(dodge_width),
                                 size     = point.size,
                                 shape    = point.shape,
                                 alpha    = point.alpha,
                                 na.rm    = point.na_remove)
  }

if(!is.null(names(errorbar.color))) {
  p <- p + ggplot2::scale_color_manual(values=errorbar.color)
} else if (length(errorbar.color) == 1L & !is.null(names(point.color))) {
  p <- p + ggplot2::scale_color_manual(values=point.color)
}

p

}

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


if(point) {
  if(!is.null(shape_quo$expr) & !null(names(point.shape))) {
    if (!is.null(names(point.color))) {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !!group_quo,
                                       shape = !!shape_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
      
      if (length(point.shape) == 1L) {
        p <- p +  ggplot2::scale_color_manual(values=point.color)
      }
    } else {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !!group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   color    = point.color,
                                   shape    = point.shape,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
    }
    
    
    
    
    
    p <- p + ggplot2::scale_shape_manual(values = point.shape)
      
  } else {
    if (!is.null(names(point.color))) {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !!group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   shape    = point.shape,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
      if (length(errorbar.color) == 1L) {
        p <- p +  ggplot2::scale_color_manual(values=point.color)
      }
    } else {
      p <- p + ggplot2::geom_point(aes(y     = !!est_quo,
                                       color = !!group_quo),
                                   position = position_dodge(dodge_width),
                                   size     = point.size,
                                   color    = point.color,
                                   shape    = point.shape,
                                   alpha    = point.alpha,
                                   na.rm    = point.na_remove)
    }
  }
  
  
  

}
