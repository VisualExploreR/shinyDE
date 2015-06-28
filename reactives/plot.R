## x options reactive
xOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  xOpts <- names(dataset)
  #    if (!is.null(input$plotType)) 
  #      if (input$plotType=='line') 
  #        xOpts <- setdiff(xOpts, input$color)
  xOpts
})

## y options reactive
yOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  yOpts <- names(dataset)
  #    if (!is.null(input$plotType)) 
  #      if (input$plotType=='line') 
  #        yOpts <- setdiff(yOpts, c(input$x, input$color))
  yOpts
})

## color options reactive
colOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  colOpts <- c('None', factorVars())
  #     if (!is.null(input$plotType)) 
  #       if (input$plotType=='line') 
  #         colOpts <- setdiff(colOpts, input$x)
  colOpts
})

## facet options reactive
facetOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  facetOpts <- c(None='.', factorVars())
  #     if (!is.null(input$plotType)) 
  #       if (input$plotType=='line')
  #         facetOpts <- setdiff(facetOpts, c(input$x, input$y))
  facetOpts
})

## size options reactive
sizeOpts <- reactive({
  c('None', numericVars())
})

## shape options reactive
shapeOpts <- reactive({
  c('None', factorVars())
})



# 1. xlim, slim (sliders, with limits set by current maxima)
# 2. "scales" argument in facet_grid/wrap : it can be "none" (default), "free_x", "free_y" and "none"
# 6. For box plot : X variables have an "as.factor" applied, and a new field created in a temporary data frame that is a factor version of the original field.
# Incidentally, there should be a "factorise" option for "colour" in case a numeric is selected, but you want to see it as factor colours instead of a gradient, which you would for a small number of values.
# 100 or less unique values
# 7. geom_path
# 9. fill option
# 10. binwidth for histogram
# 11. size magnifier

## COMPLETED ITEMS
# shapes, sizes, alpha, jitter, geom_smooth
# dynamic plot control options
# coord_flip option
# graph plot type


## plot reactive
plotInput <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) {return(NULL)}

  ## general control variables
  x <- input$x; y <- input$y
  facetRow <- input$facetRow; facetCol <- input$facetCol
  facetWrap <- input$facetWrap
  color <- input$color
  plotType <- input$plotType
  alpha <- input$alpha
  coordFlip <- input$coordFlip
  
  ## control variables specific to scatter and line plots
  shape <- input$shape
  size <- input$size
  jitter <- input$jitter
  smooth <- input$smooth
  
  ## control variables specific to histogram
  binWidth <- input$binWidth
  
  ## don't plot anything if any of the general control pieces are missing (i.e. not loaded)
  if (is.null(x) | is.null(y)) {return(NULL)}
  if (!(x %in% xOpts()) | !(y %in% yOpts())) {return(NULL)}
  if (is.null(facetRow) | is.null(facetCol)) {return(NULL)}
  if (is.null(color) | is.null(plotType)) {return(NULL)}
  if (is.null(alpha) | is.null(coordFlip)) {return(NULL)}
  
  ## modify y (measure variable) for semi-automatic aggregation dataset
  if (!(y %in% colnames(dataset))) {
    
    ## this step will do the following: e.g. y=='mpg' to y=='mpg_mean'
    y <- colnames(dataset)[grepl(y, colnames(dataset))]
    
    ## this step will do the following: e.g. y=='mpg' to y=='count'
    if (length(y)==0L & 'count' %in% colnames(dataset)) {
      y <- 'count' 
    }
  }
  
  ## scatter, graph plots
  if (plotType %in% c('scatter', 'graph')) {
    if (is.null(shape) | is.null(size)) {return(NULL)}
    if (is.null(jitter) | is.null(smooth)) {return(NULL)}
    
    ## scatter plot
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
    }
    p <- p + geom_point(aes_string(shape=shape, size=size), alpha=alpha)
    
    ## add jitter
    if (jitter=='jitter') {
      p <- p + geom_jitter()
    }
    
    ## add smoothing
    if (smooth != 'None') {
      p <- p + geom_smooth(method=smooth)
    }
    
    ## graph plot
    if (plotType=='graph') {
      if (x==color | color=='None') {
        p <- p + geom_line(aes(group=1), alpha=alpha)
      }
      else {
        p <- p + geom_line(aes_string(group=color), alpha=alpha)
      }
    }
  }

  ## line plot
  else if (plotType=='line') {
    p <- ggplot(dataset, aes_string(x=x, y=y))
    if (x==color | color=='None') {
      p <- p + geom_line(aes(group=1), alpha=alpha)
    }
    else {
      p <- p + geom_line(aes_string(group=color), alpha=alpha)
    }
  }
  
  ## bar plot
  else if (plotType=='bar') {
    p <- ggplot(dataset, aes_string(x=x, y=y)) +
      geom_bar(stat='identity', alpha=alpha)
    
    if (color != 'None') {
      p <- p + aes_string(fill=color)
    }
  }
  
  ## histogram
  else if (plotType=='histogram') {
    p <- ggplot(dataset, aes_string(x=x)) + 
      geom_histogram(alpha=alpha)
  }
  
  ## density plot
  else if (plotType=='density') {
  }
  
  ## box plot
  else if (plotType=='box') {
    p <- ggplot(dataset, aes_string(x=x, y=y)) + 
      geom_boxplot(alpha=alpha)
  }
  
  ## path plot
  else if (plotType=='path') {
    p <- ggplot(dataset, aes_string(x=x, y=y)) +
      geom_path()
  }

  ## plot colors
  if (color != 'None') {
    p <- p + aes_string(color=color)      
  }
  
  ## facet grids
  facetGrids <- paste(facetRow, '~', facetCol)
  if (facetGrids != '. ~ .')
    p <- p + facet_grid(facetGrids) 
  
  ## facet wrap
  # facetWrap <- paste('~', facetWrap)
  
  ## coordinate flip
  if (coordFlip) {
    p <- p + coord_flip()
  }
  
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
  
  ## return
  p
})



