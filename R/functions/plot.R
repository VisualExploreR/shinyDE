## function for line plot
plotLine <- function(dataset, x, y, color, alpha, xlim, ylim) {
  p <- ggplot(dataset, aes_string(x=x, y=y))
  if (x==color | color=='None') {
    p <- p + geom_line(aes(group=1), alpha=alpha)
  }
  else {
    p <- p + geom_line(aes_string(group=color), alpha=alpha)
  }
  
  p <- p + xlim(xlim) + ylim(ylim)
  
  if (color != 'None') {
    p <- p + aes_string(color=color)
  }
  
  return(p)
}


## function for scatter plot
plotScatter <- function(dataset, x, y, color, treatAsFacVarCol, shape, size, alpha, jitter, smooth, sizeMag, xlim, ylim) {
  color <- convertNoneToNULL(color)
  colorAsFactor <- varNameAsFactorOrNULL(color)
  shape <- convertNoneToNULL(shape)
  shapeAsFactor <- varNameAsFactorOrNULL(shape)
  size <- convertNoneToNULL(size)
  jitter <- convertNoneToNULL(jitter)
  smooth <- convertNoneToNULL(smooth)
  
  if (!is.null(size)) {
    p <- ggplot(dataset, aes_string(x=x, y=y)) + 
      geom_point(aes_string(shape=shapeAsFactor, size=size), 
                 alpha=alpha, position=jitter) + 
      scale_size(range = c(1, sizeMag))
      #scale_size_area(max_size=sizeMag)
      #scale_size_continuous(range = c(1, sizeMag))
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
  
  ## limiting x and y values
  p <- p + xlim(xlim) + ylim(ylim)  
  
  return(p)
}


## function for points overlay
plotPointsOverlay <- function(plot, shape, size, alpha, jitter, smooth, sizeMag) {
  shape <- convertNoneToNULL(shape)
  size <- convertNoneToNULL(size)
  jitter <- convertNoneToNULL(jitter)
  smooth <- convertNoneToNULL(smooth)
  
  ## 
  if (!is.null(size)) {
    p <- plot + 
      geom_point(aes_string(shape=shape, size=size), 
                 alpha=alpha, position=jitter) + 
      scale_size(range = c(1, sizeMag))
    #scale_size_area(max_size=sizeMag)
    #scale_size_continuous(range = c(1, sizeMag))
  } else {
    p <- plot + 
      geom_point(aes_string(shape=shape), 
                 alpha=alpha, position=jitter, size=sizeMag)
  }
  
  if (!is.null(smooth)) {
    p <- p + stat_smooth(method=smooth)
  }
  
  return(p)
}

## function for histogram
plotHistogram <- function(dataset, x, fill, position, binWidth, alpha, xlim) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)
  position <- convertNoneToNULL(position)
  p <- ggplot(dataset, aes_string(x=x)) + 
    geom_histogram(alpha=alpha, position=position, binwidth=binWidth) + 
    aes_string(fill=fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(fill)) {
    p <- p + guides(fill = guide_legend(title=fill))
  }
  
  p <- p + xlim(xlim)
  return(p)
}


## function for density plot 
plotDensity <- function(dataset, x, fill, alpha, densBlkLineCond, xlim) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)

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
  
  #p <- p + xlim(xlim)
  return(p)
}


## function for bar plot
plotBar <- function(dataset, x, y, fill, position, alpha, xlim, ylim) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)
  
  position <- convertNoneToNULL(position)
  p <- ggplot(dataset, aes_string(x=x, y=y)) +
    geom_bar(stat='identity', position=position, alpha=alpha) + 
    aes_string(fill=fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(fill)) {
    p <- p + guides(fill = guide_legend(title=fill))
  }
  
  #p <- p + xlim(xlim) + ylim(ylim)
  
  return(p)
}


## function for box plot
plotBox <- function(dataset, x, y, fill, alpha, xlim, ylim) {
  fill <- convertNoneToNULL(fill)
  fillAsFactor <- varNameAsFactorOrNULL(fill)
  
  p <- ggplot(dataset, aes_string(x=x, y=y)) + 
    geom_boxplot(alpha=alpha) + 
    aes_string(fill=fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(fill)) {
    p <- p + guides(fill = guide_legend(title=fill))
  }
  
  p <- p + xlim(xlim) + ylim(ylim)
  
  return(p)
}


## function for path plot
plotPath <- function(dataset, x, y, alpha, xlim, ylim) {
  p <- ggplot(dataset, aes_string(x=x, y=y)) +
    geom_path(alpha=alpha)
  p <- p + xlim(xlim) + ylim(ylim)
  return(p)
}


###### FACET WRAP RESEARCH
#     df <- mtcars
#     aggBy <- 'cyl'
#     aggTarget <- 'mpg'
#     aggMeth <- 'mean'
#     tuck <- aggregate(df, aggBy, aggTarget, aggMeth)

#     ggplot(tuck, aes(x=aggBy, y=mpg_mean)) + 
#       geom_point() +
#       geom_line(aes(group=1)) + 
#       facetWrap(~cyl)

# 
#     if (facetWrap != '~ .')
#       p <- p + facetWrap(facetWrap)

