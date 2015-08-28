## x options reactive
xOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  names(dataset)
})

## y options reactive
yOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  names(dataset)
})

## color options reactive
colOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  if (is.null(input$plotType)) return()
  
  if (input$plotType=='scatter') {
    colOpts <- c('None', names(dataset()))
  } else if (input$plotType %in% c('line', 'path')) {
    colOpts <- c('None', factorVars())
  }

  colOpts
})

## fill options reactive
fillOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  fillOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  fillOpts      
})

## facet options reactive
facetOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  facetOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  facetOpts
})

## size options reactive
sizeOpts <- reactive({
  c('None', numericVars())
})

## shape options reactive
shapeOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  #vars <- setdiff(varsUniqValsCntLOE6, numericVars())
  #c('None', vars)
  c('None', varsUniqValsCntLOE6)
})

## histogram max bin width reactive
histMaxBinWidth <- reactive({
  if (is.null(input$x)) return()
  dataset <- dataset(); if (is.null(dataset)) return()
  if (!(input$x %in% colnames(dataset))) return()
  maxBinWidth <- round(diff(range(dataset[[input$x]], na.rm=TRUE)))
  maxBinWidth 
})


## additional aggregation options reactive
plotAddAggByOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  setdiff(origVars(), plotSemiAutoAggByBase())
})



#### display conditional reactives
## display y condition reactive
displayYCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- TRUE
  return (!(input$plotType %in% c('histogram', 'density')))
})
  
## display color condition reactive
displayColCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('line', 'scatter', 'path'))) 
})

## display treat-as-a-factor-variable (for color) condition reactive
displayTreatAsFacVarColCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('scatter'))) 
})

## display fill condition reactive
displayFillCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('box', 'histogram', 'bar', 'density')))
})


## display position condition reactive
displayPosCond <- reactive({
  if (is.null(input$plotType)) return()
  return (any(input$plotType %in% c('histogram', 'bar')))
})

## display shape condition reactive
displayShapeCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display
})

## display size condition reactive
displaySizeCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display
})

## display smooth condition reactive
displaySmthCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- TRUE
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- TRUE
  }
  display  
})

## display jitter condition reactive (belongs to advanced control widgets)
displayJitCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAdvCtrlWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAdvCtrlWgts
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAdvCtrlWgts
  }
  display
})

## display size magnifier condition reactive (belongs to advanced control widgets)
displaySizeMagCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAdvCtrlWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAdvCtrlWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {    
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAdvCtrlWgts
  }
  display
})

## display bin width condition reactive
displayBinWidthCond <- reactive({
  if (is.null(input$plotType)) return() 
  if (is.null(input$x)) return()
  return (input$plotType=='histogram')
})

## display density blakc line condition reactive
displayDensBlkLineCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAdvCtrlWgts)) return()
  return (input$plotType=='density' & input$showAdvCtrlWgts)
})


## display points overlay checkbox condition reactive
displayPtsOverlayCond <- reactive({
  if (is.null(input$plotType)) return()
  return (input$plotType %in% c('line', 'path'))
})

## display additional aggregation select field condition reactive
displayPlotAddAggBy <- reactive({
  if (is.null(input$semiAutoAgg)) return()
  return (input$semiAutoAgg=='allowed')
})

## display xlim condition reactive
displayXlim <- reactive({
  if (is.null(dataset())) return()
  if (is.null(input$x)) return()
  if (is.null(y())) return()
  if (is.null(input$semiAutoAgg)) return()
  
  if (is.null(input$showAdvCtrlWgts)) return()
  return(input$showAdvCtrlWgts)
})

## display ylim condition reactive
displayYlim <- reactive({
  if (is.null(dataset())) return()
  if (is.null(input$x)) return()
  if (is.null(y())) return()
  if (is.null(input$semiAutoAgg)) return()
  
  if (is.null(input$plotType)) return()  
  if (input$plotType=='histogram') return()
  
  if (is.null(input$showAdvCtrlWgts)) return()
  return(input$showAdvCtrlWgts)
})



#### widgets loaded conditional reactives

## universal widgets
universalBaseWidgets <- c('plotType', 'x')
#universalAdvWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale', 'alpha', 'coordFlip', 'xlim')
universalAdvWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale', 'alpha', 'coordFlip')
universalFullWidgets <- c(universalBaseWidgets, universalAdvWidgets)

scatterBaseWidgets <- c(universalBaseWidgets, 'y', 'color', 'treatAsFacVarCol', 'shape', 'size', 'smooth')
#scatterAdvWidgets <- c(universalAdvWidgets, 'sizeMag', 'jitter', 'ylim')
scatterAdvWidgets <- c(universalAdvWidgets, 'sizeMag', 'jitter')
scatterFullWidgets <- c(scatterBaseWidgets, scatterAdvWidgets)

lineBaseWidgets <- c(universalBaseWidgets, 'y', 'color')
#lineAdvWidgets <- c(universalAdvWidgets, 'ylim')
lineAdvWidgets <- c(universalAdvWidgets)
lineFullWidgets <- c(lineBaseWidgets, lineAdvWidgets)

barBaseWidgets <- c(universalBaseWidgets, 'y', 'fill', 'position')
#barAdvWidgets <- c(universalAdvWidgets, 'ylim')
barAdvWidgets <- c(universalAdvWidgets)
barFullWidgets <- c(barBaseWidgets, barAdvWidgets)

histogramBaseWidgets <- c(universalBaseWidgets, 'fill', 'position', 'binWidth')
histogramAdvWidgets <- c(universalAdvWidgets)
histogramFullWidgets <- c(histogramBaseWidgets, histogramAdvWidgets)

densityBaseWidgets <- c('fill', 'color')
densityAdvWidgets <- c(universalAdvWidgets, 'densBlkLineCond')
densityFullWidgets <- c(densityBaseWidgets, densityAdvWidgets)

boxBaseWidgets <- c('y', 'fill')
#boxAdvWidgets <- c(universalAdvWidgets, 'ylim')
boxAdvWidgets <- c(universalAdvWidgets)
boxFullWidgets <- c(boxBaseWidgets, boxAdvWidgets)

pathBaseWidgets <- c('y')
#pathAdvWidgets <- c(universalAdvWidgets, 'ylim')
pathAdvWidgets <- c(universalAdvWidgets)
pathFullWidgets <- c(pathBaseWidgets, pathAdvWidgets)



## universal widgets loaded
universalWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, universalFullWidgets)
  else
    checkWidgetsLoaded(input, universalBaseWidgets)
})

## scatter plot widgets loaded
scatterWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, scatterFullWidgets)
  else
    checkWidgetsLoaded(input, scatterBaseWidgets)
})

lineWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, lineFullWidgets)
  else
    checkWidgetsLoaded(input, lineBaseWidgets)
})

# linePtsOverlayWidgetsLoaded <- reactive({
#   if (!generalWidgetsLoaded()) return(FALSE)
#   wgtCtrls <- c('shape', 'size', 'jitter', 'smooth', 'ptsOverlayCond')
#   checkWidgetsLoaded(input, wgtCtrls)
# })

## bar plot widgets loaded
barWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, barFullWidgets)
  else
    checkWidgetsLoaded(input, barBaseWidgets)
})

## histogram widgets loaded
histogramWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, histogramFullWidgets)
  else
    checkWidgetsLoaded(input, histogramBaseWidgets)
})

## density plot widgets loaded
densityWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, densityFullWidgets)
  else
    checkWidgetsLoaded(input, densityBaseWidgets)
})

## box plot widgets loaded
boxWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, boxFullWidgets)
  else
    checkWidgetsLoaded(input, boxBaseWidgets)
})

## path plot widgets loaded
pathWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, pathFullWidgets)
  else
    checkWidgetsLoaded(input, pathBaseWidgets)
})

# pathPtsOverlayWidgetsLoaded <- reactive({
#   if (!generalWidgetsLoaded()) return(FALSE)
#   wgtCtrls <- c('shape', 'size', 'ptsOverlayCond')
#   checkWidgetsLoaded(input, wgtCtrls)    
# })






## plot reactive
plotInput <- reactive({
  
  ## load dataset to use (already subsetted/filtered)
  dataset <- plotDF(); if (is.null(dataset)) return()

  ## load variables from control widgets
  semiAutoAggSet <- input$semiAutoAgg=='allowed'
  plotType <- input$plotType
  x <- input$x
  y <- input$y
  facetRow <- input$facetRow
  facetCol <- input$facetCol
  facetWrap <- input$facetWrap
  facetScale <- input$facetScale
  color <- input$color
  fill <- input$fill
  alpha <- input$alpha
  coordFlip <- input$coordFlip
  shape <- input$shape
  size <- input$size
  sizeMag <- input$sizeMag
  jitter <- input$jitter
  smooth <- input$smooth
  position <- input$position
  binWidth <- input$binWidth
  densBlkLineCond <- input$densBlkLineCond
  ptsOverlayCond <- input$ptsOverlayCond
  xlim <- input$xlim
  ylim <- input$ylim
  treatAsFacVarCol <- input$treatAsFacVarCol
  plotAggMeth <- input$plotAggMeth
  semiAutoAggOn <- ifelse(input$semiAutoAgg=='allowed', TRUE, FALSE)
  
  ## don't plot anything if any of universal control widgets is not loaded
  if (!universalWidgetsLoaded()) return() 
  if (!(x %in% xOpts())) return()

  ## ensure proper variable names (in case of semi-automatic aggregation)
  y <- y()
  color <- ensureProperVarName2(colnames=colnames(dataset), var=color, aggMeth=plotAggMeth, semiAutoAggOn=semiAutoAggOn)
  size <- ensureProperVarName2(colnames=colnames(dataset), var=size, aggMeth=plotAggMeth, semiAutoAggOn=semiAutoAggOn)

  ## scatter plot
  if (plotType=='scatter')  {
    if (!scatterWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotScatter(dataset, x, y, color, treatAsFacVarCol, shape, size, smooth, alpha, sizeMag, jitter)
  }

  ## line plot
  else if (plotType=='line') {
    if (!lineWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotLine(dataset, x, y, color, alpha)

    ## line plot with points overlay
#     if (!linePtsOverlayWidgetsLoaded()) return()
#     if (ptsOverlayCond)
#       p <- plotPointsOverlay(p, shape, size, alpha, jitter, smooth, sizeMag)
  }
  
  ## bar plot
  else if (plotType=='bar') {
    if (!barWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotBar(dataset, x, y, fill, position, alpha)
  }
  
  ## histogram
  else if (plotType=='histogram') {
    if (!histogramWidgetsLoaded()) return()
    p <- plotHistogram(dataset, x, fill, position, binWidth, alpha)
  }
    
  ## density plot
  else if (plotType=='density') {
    if (!densityWidgetsLoaded()) return()
    p <- plotDensity(dataset, x, fill, densBlkLineCond, alpha)
  }    
  
  ## box plot
  else if (plotType=='box') {
    if (!boxWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotBox(dataset, x, y, fill, alpha)
  }
  
  ## path plot
  else if (plotType=='path') {
    if (!pathWidgetsLoaded()) return()
    if (!(y %in% finalDFVars())) return()
    p <- plotPath(dataset, x, y, alpha)
    
#     ## path plot with points overlay
#     if (!pathPtsOverlayWidgetsLoaded()) return()
#     if (ptsOverlayCond)
#       p <- plotPointsOverlay(p, shape, size, alpha, jitter, smooth, sizeMag)
  }
  
  ## plot advanced controls
  if (input$showAdvCtrlWgts) {

    ## if faceting is selected
    if (!noFacetSelected()) {
      
      facetCol <- ifelse(facetCol=='None', '.', facetCol)
      facetRow <- ifelse(facetRow=='None', '.', facetRow)
      facetWrap <- ifelse(facetWrap=='None', '.', facetWrap)
      
      ## facet grids
      if (facetGridSelected()) {
        facetGrids <- paste(facetRow, '~', facetCol)
        p <- p + facet_grid(facets=facetGrids, scales=facetScale)
      } 
      
      ## facet wrap
      else if (facetWrapSelected()) {
        p <- p + facet_wrap(facets=facetWrap, scales=facetScale)
      }
    }

    ## coordinate flip
    if (coordFlip) {
      p <- p + coord_flip()
    }
  }
    
  ## return
  p
})

