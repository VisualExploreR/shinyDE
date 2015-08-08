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
plotScatter <- function(dataset, x, y, shape, size, alpha, jitter, smooth) {
  p <- ggplot(dataset, aes_string(x=x, y=y))
  
  ## add shape, size, and alpha
  if (shape=='None') {
    shape <- 'NULL'
  } else {
    p <- p + guides(shape=guide_legend(title=shape))
    shape <- paste0('as.factor(', shape, ')')
  }
  if (size=='None') {
    size <- 'NULL'
    #size <- sizeMag
  }
  
  p <- p + 
    geom_point(aes_string(shape=shape, size=size), alpha=alpha) 
  
  ## add jitter
  if (jitter=='jitter') {
    p <- p + geom_jitter()
  }
  
  ## add smoothing
  if (smooth != 'None') {
    p <- p + stat_smooth(method=smooth)
  }
  
  return(p)
}

## function for graph plot
plotGraph <- function(scatPlot, color, alpha) {
  p <- scatPlot
  if (x==color | color=='None') {
    p <- p + geom_line(aes(group=1), alpha=alpha)
  }
  else {
    p <- p + geom_line(aes_string(group=color), alpha=alpha)
  }
  return(p)
}


