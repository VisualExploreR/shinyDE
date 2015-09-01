## function for line plot
plotLine <- function(dataset, x, y, color, alpha=NULL) {
  color <- convertNoneToNULL(color)
  colorAsFactor <- varNameAsFactorOrNULL(color)
  
  if (is.null(alpha)) alpha <- 1
  
  p <- ggplot(dataset, aes_string(x=x, y=y))
  
  if (is.null(color)) {
    p <- p + geom_line(aes(group=1), alpha=alpha)
  } else if (x==color) {
    p <- p + geom_line(aes(group=1), alpha=alpha)
  } else {
    p <- p + geom_line(aes_string(group=color), alpha=alpha)
  }

  if (!is.null(color)) {
    p <- p + aes_string(color=colorAsFactor) + 
      guides(color = guide_legend(title=color))
  }
  
  return(p)
}


## function for scatter plot
plotScatter <- function(dataset, x, y, color, treatAsFacVarCol, shape, size, smooth, jitter, alpha=NULL, sizeMag=NULL) {
  color <- convertNoneToNULL(color)
  colorAsFactor <- varNameAsFactorOrNULL(color)
  shape <- convertNoneToNULL(shape)
  shapeAsFactor <- varNameAsFactorOrNULL(shape)
  size <- convertNoneToNULL(size)
  smooth <- convertNoneToNULL(smooth)

  if (is.null(alpha)) alpha <- 1
  if (is.null(sizeMag)) sizeMag <- 4
  
  ## jitter variable's value should be either "jitter" or NULL
  if (!is.null(jitter)) {
    if (jitter) 
      jitter <- 'jitter' 
    else 
      jitter <- NULL
  }

  if (!is.null(size)) {
    p <- ggplot(dataset, aes_string(x=x, y=y)) + 
      geom_point(aes_string(shape=shapeAsFactor, size=size), 
                 alpha=alpha, position=jitter) + 
      scale_size(range = c(1, sizeMag))
  } else {
    p <- ggplot(dataset, aes_string(x=x, y=y)) + 
      geom_point(aes_string(shape=shapeAsFactor), 
                 alpha=alpha, position=jitter, size=sizeMag)
  }
  
  ## coloring points
  if (treatAsFacVarCol) {
    p <- p + aes_string(color=colorAsFactor)
    p <- p + guides(color = guide_legend(title=color))
  } else {
    p <- p + aes_string(color=color)
  }
  
  ## legend label for point shapes
  if (!is.null(shape)) {
    p <- p + guides(shape = guide_legend(title=shape))
  }
  
  ## line smoothing
  if (!is.null(smooth)) {
    p <- p + stat_smooth(method=smooth)
  }

  return(p)
}


## function for points overlay
plotPointsOverlay <- function(plot, shape, size, smooth, jitter, alpha=NULL, sizeMag=NULL) {
  shape <- convertNoneToNULL(shape)
  shapeAsFactor <- varNameAsFactorOrNULL(shape)
  size <- convertNoneToNULL(size)
  smooth <- convertNoneToNULL(smooth)
  
  if (is.null(alpha)) alpha <- 1
  if (is.null(sizeMag)) sizeMag <- 4
  
  ## jitter variable's value should be either "jitter" or NULL
  if (!is.null(jitter)) {
    if (jitter) 
      jitter <- 'jitter' 
    else 
      jitter <- NULL
  }
  
  ## 
  if (!is.null(size)) {
    p <- plot + 
      geom_point(aes_string(shape=shapeAsFactor, size=size), 
                 alpha=alpha, position=jitter) + 
      scale_size(range = c(1, sizeMag))
  } else {
    p <- plot + 
      geom_point(aes_string(shape=shapeAsFactor), 
                 alpha=alpha, position=jitter, size=sizeMag)
  }
  
  ## legend label for point shapes
  if (!is.null(shape)) {
    p <- p + guides(shape = guide_legend(title=shape))
  }
  
  if (!is.null(smooth)) {
    p <- p + stat_smooth(method=smooth)
  }
  
  return(p)
}

## function for histogram
plotHistogram <- function(dataset, x, fill, position, binWidth, alpha=NULL) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)
  position <- convertNoneToNULL(position)
  
  if (is.null(alpha)) alpha <- 1
  
  p <- ggplot(dataset, aes_string(x=x)) + 
    geom_histogram(alpha=alpha, position=position, binwidth=binWidth) + 
    aes_string(fill=fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(fill)) {
    p <- p + guides(fill = guide_legend(title=fill))
  }

  return(p)
}


## function for density plot 
plotDensity <- function(dataset, x, fill, densBlkLineCond=NULL, alpha=NULL) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)

  if (is.null(densBlkLineCond)) densBlkLineCond <- FALSE
  if (is.null(alpha)) alpha <- 1
  
  p <- ggplot(dataset, aes_string(x=x)) 
  if (densBlkLineCond) {
    p <- p + geom_density(aes_string(group=fillAsFactor, fill=fillAsFactor), alpha=alpha)
    if (!is.null(fill)) 
      p <- p + guides(group = guide_legend(title=fill),
                      fill = guide_legend(title=fill))
  } else {
    p <- p + geom_density(aes_string(group=fillAsFactor, color=fillAsFactor, fill=fillAsFactor), alpha=alpha)
    if (!is.null(fill))
      p <- p + guides(group = guide_legend(title=fill),
                      color = guide_legend(title=fill),
                      fill = guide_legend(title=fill))
  }
  return(p)
}


## function for bar plot
plotBar <- function(dataset, x, y, fill, position, alpha=NULL) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)
  position <- convertNoneToNULL(position)
  
  if (is.null(alpha)) alpha <- 1
  
  p <- ggplot(dataset, aes_string(x=x, y=y)) +
    geom_bar(stat='identity', position=position, alpha=alpha) + 
    aes_string(fill=fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(fill)) {
    p <- p + guides(fill = guide_legend(title=fill))
  }

  return(p)
}

## function for box plot
plotBox <- function(dataset, x, y, fill, alpha=NULL) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)

  if (is.null(alpha)) alpha <- 1
  
  p <- ggplot(dataset, aes_string(x=x, y=y)) + 
    geom_boxplot(alpha=alpha) + 
    aes_string(fill=fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(fill)) {
    p <- p + guides(fill = guide_legend(title=fill))
  }

  return(p)
}


## function for path plot
plotPath <- function(dataset, x, y, alpha=NULL) {
  if (is.null(alpha)) alpha <- 1
  p <- ggplot(dataset, aes_string(x=x, y=y)) +
    geom_path(alpha=alpha)
  return(p)
}
