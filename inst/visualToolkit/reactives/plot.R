## scatter plot inputs

## scatter plot inputs
scatterPlotInputs <- reactive({
  list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), treatAsFacVarCol=treatAsFacVarCol(), 
       shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), jitter=jitter(), 
       alpha=alpha(), sizeMag=sizeMag())
})

## line plot inputs
linePlotInputs <- reactive({
  list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), alpha=alpha())
})

## line points overlay inputs
linePtsOverlayInputs <- reactive({
  list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), 
       smooth=smooth(), jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
})


## bar plot inputs
barPlotInputs <- reactive({
  list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), alpha=alpha())
})

## histogram inputs
histogramInputs <- reactive({
  list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), 
       binWidth=binWidth(), alpha=alpha())
})

## density plot inputs
densityPlotInputs <- reactive({
  list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), 
       densBlkLineCond=densBlkLineCond(), alpha=alpha())
})

## box plot inputs
boxPlotInputs <- reactive({
  list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), alpha=alpha())
})

## path plot inputs
pathPlotInputs <- reactive({
  list(x=x(), y=y(), alpha=alpha())
})

## path points overlay inputs
pathPtsOverlayInputs <- reactive({
  list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), 
       jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
})


## plot reactive
plotInput <- reactive({

  ## load dataset to use (already subsetted/filtered)
  dataset <- plotDF(); if (is.null(dataset)) return()
  
  ## don't plot anything if any of universal control widgets is not loaded
  if (!universalPlotWidgetsLoaded()) return()
  
  ## don't plot anything if x hasn't been updated according to new dataset
  if (!(x() %in% xOpts())) return()
  
  ## scatter plot
  if (plotType()=='scatter')  {
    if (is.null(scatterWidgetsLoaded())) return()
    if (!scatterWidgetsLoaded()) return()
      if (!(y() %in% finalDFVars())) return()
    p <- plotScatter(dataset, scatterPlotInputs())
  }

  ## line plot
  else if (plotType()=='line') {
    if (!lineWidgetsLoaded()) return()
    if (!(y() %in% finalDFVars())) return()
    p <- plotLine(dataset, linePlotInputs())

    ## line plot with points overlay
    if (!linePtsOverlayWidgetsLoaded()) return()
    if (ptsOverlayCond) 
      p <- plotPointsOverlay(p, linePtsOverlayInputs())
  }
  
  ## bar plot
  else if (plotType()=='bar') {
    if (!barWidgetsLoaded()) return()
    if (!(y() %in% finalDFVars())) return()
    p <- plotBar(dataset, barPlotInputs())
  }
  
  ## histogram
  else if (plotType()=='histogram') {
    if (!histogramWidgetsLoaded()) return()
    p <- plotHistogram(dataset, histogramInputs())
  }

  ## density plot
  else if (plotType()=='density') {
    if (!densityWidgetsLoaded()) return()
    p <- plotDensity(dataset, densityPlotInputs())
  }
  
  ## box plot
  else if (plotType()=='box') {
    if (!boxWidgetsLoaded()) return()
    if (!(y() %in% finalDFVars())) return()
    p <- plotBox(dataset, boxPlotInputs())
  }
  
  ## path plot
  else if (plotType()=='path') {
    if (!pathWidgetsLoaded()) return()
    if (!(y() %in% finalDFVars())) return()
    p <- plotPath(dataset, pathPlotInputs())
    
    ## path plot with points overlay
    if (!pathPtsOverlayWidgetsLoaded()) return()
    if (ptsOverlayCond)
      p <- plotPointsOverlay(p, pathPtsOverlayInputs())
  }
  
  ## plot facet controls
  if (!noFacetSelected()) {

    ## facet grids
    if (facetGridSelected()) {
      p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
    } 
    
    ## facet wrap
    else if (facetWrapSelected()) {
      p <- p + facet_wrap(facets=facetWrap(), scales=facetScale())
    }
  }
  
  
  ## plot coord flip control 
  if (coordFlip()) 
    p <- p + coord_flip()

  ## return
  p
})

