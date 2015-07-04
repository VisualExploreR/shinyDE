## this function modifies and ensures proper y (measure variable)
## for semi-automatic aggregation dataset
ensureProperYVarName <- function(dataset, y) {
  if (!(y %in% colnames(dataset))) {
    
    ## this step will do the following: e.g. y=='mpg' to y=='mpg_mean'
    y <- colnames(dataset)[grepl(y, colnames(dataset))]
    
    ## this step will do the following: e.g. y=='mpg' to y=='count'
    if (length(y)==0L & 'count' %in% colnames(dataset)) {
      y <- 'count' 
    }
  }
  return(y)
}

## function to convert 'None' to NULL
convertNoneToNULL <- function(var) {
  if (tolower(var)=='none') {var <- NULL}; return(var)
}

## function to check if specified widgets are loaded on shiny UI
checkWidgetsLoaded <- function(input, widgets) {
  for (widget in widgets) {
    if (is.null(input[[widget]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

## function for line plot
plotLine <- function(dataset, x, y, color, alpha) {
  p <- ggplot(df, aes_string(x=x, y=y))
  if (x==color | color=='None') {
    p <- p + geom_line(aes(group=1), alpha=alpha)
  }
  else {
    p <- p + geom_line(aes_string(group=color), alpha=alpha)
  }
  return(p)
}

## function for scatter plot
plotScatter <- function(dataset, x, y, shape, size, alpha, jitter, smooth, sizeMag) {
  shape <- convertNoneToNULL(shape)
  size <- convertNoneToNULL(size)
  jitter <- convertNoneToNULL(jitter)
  smooth <- convertNoneToNULL(smooth)

  ## 
  if (!is.null(size)) {
    p <- ggplot(dataset, aes_string(x=x, y=y)) + 
      geom_point(aes_string(shape=shape, size=size), 
                 alpha=alpha, position=jitter) + 
      scale_size(range = c(1, sizeMag))
      #scale_size_area(max_size=sizeMag)
      #scale_size_continuous(range = c(1, sizeMag))
  } else {
    p <- ggplot(dataset, aes_string(x=x, y=y)) + 
      geom_point(aes_string(shape=shape), 
                 alpha=alpha, position=jitter, size=sizeMag)
  }
  
  if (!is.null(smooth)) {
    p <- p + stat_smooth(method=smooth)
  }
  
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
plotHistogram <- function(dataset, x, fill, position, binWidth, alpha) {
  fill <- convertNoneToNULL(fill)
  position <- convertNoneToNULL(position)
  p <- ggplot(dataset, aes_string(x=x)) + 
    geom_histogram(alpha=alpha, position=position, binwidth=binWidth) + 
    aes_string(fill=fill)
  return(p)
}

## function for density plot 
plotDensity <- function(dataset, x, fill, alpha, densBlkLineCond) {
  fill <- convertNoneToNULL(fill)
  p <- ggplot(dataset, aes_string(x=x)) 
  
  if (densBlkLineCond) {
    p <- p + geom_density(aes_string(group=fill, fill=fill), alpha=alpha)     
  } else {
    p <- p + geom_density(aes_string(group=fill, color=fill, fill=fill), alpha=alpha) 
  }

  return(p)
}

## function for bar plot
plotBar <- function(dataset, x, y, fill, position, alpha) {
  fill <- convertNoneToNULL(fill)
  position <- convertNoneToNULL(position)
  p <- ggplot(dataset, aes_string(x=x, y=y)) +
    geom_bar(stat='identity', position=position, alpha=alpha) + 
    aes_string(fill=fill)
  return(p)
}

## function for box plot
plotBox <- function(dataset, x, y, fill, alpha) {
  fill <- convertNoneToNULL(fill)
  p <- ggplot(dataset, aes_string(x=x, y=y)) + 
    geom_boxplot(alpha=alpha) + 
    aes_string(fill=fill)
  return(p)
}

## function for path plot
plotPath <- function(dataset, x, y, alpha) {
  p <- ggplot(dataset, aes_string(x=x, y=y)) +
    geom_path(alpha=alpha)
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

