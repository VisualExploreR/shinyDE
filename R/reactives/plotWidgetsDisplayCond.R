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
  if (is.null(xType())) return()
  if (is.null(yType())) return()
  
  display <- FALSE
  if (input$plotType=='scatter') {
    if (xType()=='continuous') {
      print('xType evaluated')
      if (yType()=='continuous') {
        print('yType evaluated')
        display <- TRUE   
      }
    }
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) {
      if (xType()=='continuous' & yType()=='continuous')
      display <- TRUE 
    }
  }
  display  
})

## display jitter condition reactive (belongs to advanced control widgets)
displayJitCond <- reactive({
  if (is.null(input$plotType)) return()
  #if (is.null(input$showAdvCtrlWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    #display <- input$showAdvCtrlWgts
    display <- TRUE
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    #if (input$ptsOverlayCond) display <- input$showAdvCtrlWgts
    if (input$ptsOverlayCond) display <- TRUE
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
  if (is.null(semiAutoAggOn())) return()
  return (semiAutoAggOn())
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
