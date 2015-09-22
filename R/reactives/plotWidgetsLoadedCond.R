#### widgets required by plot type

## universal widgets
universalBaseWidgets <- c('plotType', 'x')
universalAdvWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale', 'alpha', 'coordFlip', 'xlim')
universalFullWidgets <- c(universalBaseWidgets, universalAdvWidgets)

## scatter plot widgets
scatterBaseWidgets <- c(universalBaseWidgets, 'y', 'color', 'treatAsFacVarCol', 'shape', 'size', 'smooth', 'jitter')
scatterAdvWidgets <- c(universalAdvWidgets, 'sizeMag', 'ylim')
scatterFullWidgets <- c(scatterBaseWidgets, scatterAdvWidgets)

## line plot widgets
lineBaseWidgets <- c(universalBaseWidgets, 'y', 'color')
lineAdvWidgets <- c(universalAdvWidgets, 'ylim')
lineFullWidgets <- c(lineBaseWidgets, lineAdvWidgets)

## line plot points overlay widgets
linePtsOverlayBaseWidgets <- c('shape', 'size', 'smooth', 'jitter', 'ptsOverlayCond')
linePtsOverlayAdvWidgets <- c(universalAdvWidgets, 'ylim')
linePtsOverlayFullWidgets <- c(linePtsOverlayBaseWidgets, linePtsOverlayAdvWidgets)

## bar plot widgets
barBaseWidgets <- c(universalBaseWidgets, 'y', 'fill', 'position')
barAdvWidgets <- c(universalAdvWidgets, 'ylim')
barFullWidgets <- c(barBaseWidgets, barAdvWidgets)

## histogram widgets
histogramBaseWidgets <- c(universalBaseWidgets, 'fill', 'position', 'binWidth')
histogramAdvWidgets <- c(universalAdvWidgets)
histogramFullWidgets <- c(histogramBaseWidgets, histogramAdvWidgets)

## density plot widgets
densityBaseWidgets <- c('fill', 'color')
densityAdvWidgets <- c(universalAdvWidgets, 'densBlkLineCond')
densityFullWidgets <- c(densityBaseWidgets, densityAdvWidgets)

## box plot widgets
boxBaseWidgets <- c('y', 'fill')
boxAdvWidgets <- c(universalAdvWidgets, 'ylim')
boxFullWidgets <- c(boxBaseWidgets, boxAdvWidgets)

## path plot widgets
pathBaseWidgets <- c('y')
pathAdvWidgets <- c(universalAdvWidgets, 'ylim')
pathFullWidgets <- c(pathBaseWidgets, pathAdvWidgets)

## path plot points overlay widgets loaded
pathPtsOverlayBaseWidgets <- c('shape', 'size', 'ptsOverlayCond')
pathPtsOverlayAdvWidgets <- c(universalAdvWidgets, 'ylim')
pathPtsOverlayFullWidgets <- c(pathPtsOverlayBaseWidgets, pathPtsOverlayAdvWidgets)




#### widgets loaded conditional reactives

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

## line widgets loaded
lineWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, lineFullWidgets)
  else
    checkWidgetsLoaded(input, lineBaseWidgets)
})

## line plot points overlay widgets loaded
linePtsOverlayWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, linePtsOverlayFullWidgets)
  else
    checkWidgetsLoaded(input, linePtsOverlayBaseWidgets)
})

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

## path plot points overlay widgets loaded
pathPtsOverlayWidgetsLoaded <- reactive({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkWidgetsLoaded(input, pathPtsOverlayFullWidgets)
  else
    checkWidgetsLoaded(input, pathPtsOverlayBaseWidgets)
})