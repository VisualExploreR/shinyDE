## plot type options
output$plotTypeCtrl <- renderUI({
  selectInput(inputId = "plotType", label = "Plot Type", 
              choices = c('Scatter'='scatter', 'Line'='line',
                          'Bar'='bar', 'Histogram'='histogram', 
                          'Density'='density', 'Box'='box',
                          'Path'='path'
                          #'Violin'='violin', 
                          #'Image'='image', 
                          #'2-Density', 'density2d'
              ),
              multiple = FALSE)
})

## semi-automatic aggregation option
output$semiAutoAggCtrl <- renderUI({
  radioButtons('semiAutoAgg', label = "Semi-auto Aggregation",
               choices = list("Allowed" = 'allowed', "Disabled" = 'disabled'),
               selected = 'disabled', inline = FALSE)
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  aggMethOpts <- c('sum', 'mean', 'count', 'min', 'max', 'median')
  selectInput('plotAggMeth', 'Aggregation Method', aggMethOpts)
  #selectInput('plotAggMeth', 'Aggregation Method', choices=c(aggMethOpts, 'None'))
})

## x-axis options
output$xCtrl <- renderUI({
  if (is.null(input$dataset)) return()
  
  selected <- NULL
  if (input$dataset=='diamonds') 
    selected <- 'carat'
  else if (input$dataset=='mtcars')
    selected <- 'mpg'
  else if (input$dataset=='rock')
    selected <- 'area'

  selectInput('x', 'X', choices=xOpts(), selected=selected)
})

## y-axis options
output$yCtrl <- renderUI({
  if (is.null(input$dataset)) return()
  if (is.null(displayYCond())) return()
  if (displayYCond()) {
    
    selected <- NULL
    if (input$dataset=='diamonds')
      selected <- 'price'
    else if (input$dataset=='mtcars')
      selected <- 'hp'
    else if (input$dataset=='rock')
      selected <- 'peri'
    
    selectInput('y', 'Y', choices=yOpts(), selected=selected)
  }
})

## color control options
output$colCtrl <- renderUI({
  if (is.null(displayColCond())) return()
  if (displayColCond())
    selectInput('color', 'Color', colOpts())    
})

## treat-as-a-factor-variable option for color
output$treatAsFacVarColCtrl <- renderUI({
  if (is.null(displayTreatAsFacVarColCond())) return()
  if (displayTreatAsFacVarColCond())
    checkboxInput('treatAsFacVarCol', 'Treat as a factor variable.', value=FALSE)
})

## fill control options
output$fillCtrl <- renderUI({
  if (is.null(displayFillCond())) return()
  if (displayFillCond())
    selectInput('fill', 'Fill', fillOpts())
})

## position (stack vs. dodge) control options
output$posCtrl <- renderUI({
  if (is.null(displayPosCond())) return()
  if (displayPosCond())
    selectInput('position', 'Position', c('None', 'dodge', 'stack'))
})

## jitter options
output$jitCtrl <- renderUI({
  if (is.null(displayJitCond())) return()  
  if (displayJitCond())
    checkboxInput('jitter', 'Apply jitter effect', value=FALSE)
})

## geom smoothing options
output$smthCtrl <- renderUI({
  if (is.null(displaySmthCond())) return()
  if (displaySmthCond()) {
    if (all(c(input$x, input$y) %in% numericVars())) {
      selectInput('smooth', 'Smoothing Effect', 
                  c('None'='None', 'Linear'='lm', 'Auto'='auto'))
    }
  } 
})

## size options
output$sizeCtrl <- renderUI({
  if (is.null(displaySizeCond())) return()
  if (displaySizeCond())
    selectInput('size', 'Size', sizeOpts())        
})

## shape options
output$shapeCtrl <- renderUI({
  if (is.null(displayShapeCond())) return()
  if (displayShapeCond())
    selectInput('shape', 'Shape', shapeOpts())
})

## histogram binwidth options
output$binWidthCtrl <- renderUI({
  if (is.null(displayBinWidthCond())) return()
  if (is.null(histMaxBinWidth())) return()
  if (displayBinWidthCond())
    sliderInput('binWidth', label = "Bin Width",
                min=1, max=histMaxBinWidth(), value=1, step=1)
})

## density line color options
output$densBlkLineCondCtrl <- renderUI({
  if (is.null(displayDensBlkLineCond())) return()
  if (displayDensBlkLineCond())
    checkboxInput('densBlkLineCond', 'Draw black outline', value = FALSE)
})

## points overlay options
output$ptsOverlayCondCtrl <- renderUI({  
  if (is.null(displayPtsOverlayCond())) return()
  if (displayPtsOverlayCond())
    checkboxInput('ptsOverlayCond', 'Points Overlay', value=FALSE)
})





## show advanced options
output$showAdvCtrlWgtsCtrl <- renderUI({
  checkboxInput('showAdvCtrlWgts', 'Show advanced control widgets', value=FALSE)
})

## row-wise facet options
output$facetRowCtrl <- renderUI({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    selectInput('facetRow', 'Facet Row', facetOpts())
})

## column-wise facet options
output$facetColCtrl <- renderUI({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    selectInput('facetCol', 'Facet Column', facetOpts())
})

## facet wrap options
output$facetWrapCtrl <- renderUI({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    selectInput('facetWrap', 'Facet Wrap', facetOpts())
})

## facet scale options
output$facetScaleCtrl <- renderUI({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    selectInput('facetScale', 'Facet Scale',
                c('None'='fixed', 'Free X'='free_x', 
                  'Free Y'='free_y', 'Free X & Y'='free'))
})

## alpha (opacity) options
output$alphaCtrl <- renderUI({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    sliderInput("alpha", label = "Opacity",
                min=0, max=1, value=1, step=0.1)
})


## size magnifier option
output$sizeMagCtrl <- renderUI({
  if (is.null(displaySizeMagCond())) return()
  if (displaySizeMagCond()) 
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=4, step=1)
})

## coordinate flip options 
output$coordFlipCtrl <- renderUI({
  if (is.null(input$showAdvCtrlWgts)) return()
  if (input$showAdvCtrlWgts)
    checkboxInput('coordFlip', 'Flip X and Y coordinates.', value = FALSE)
})



## additional aggregation by options
output$plotAddAggByCtrl <- renderUI({
  if (is.null(displayPlotAddAggBy())) return()
  if (displayPlotAddAggBy())
    selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
                choices=plotAddAggByOpts(), multiple=T)
})

# ## xlim control
# output$xlimCtrl <- renderUI({
#   if (is.null(displayXlim())) return()
#   if (displayXlim()) {
#     if (input$x %in% finalDFNumericVars()) {
#       if (is.null(xRange())) return()
#       sliderInput("xlim", label="X Range Filter",
#                   min=xRange()[1], max=xRange()[2], value=xRange(), round=FALSE)
#     } else if (input$x %in% finalDFFactorVars()) {
#       selectInput('xlim', label='X Value Filter', 
#                   choices=xFactorVarUniqVals(), 
#                   selected=xFactorVarUniqVals(),
#                   multiple=T)
#     }
#   }
# })
# 
# ## ylim control
# ## note: ylim() is NOT applicable to histograms
# output$ylimCtrl <- renderUI({
#   if (is.null(displayYlim())) return()
#   if (displayYlim()) {
#     y <- y()
#     
#     if (y %in% finalDFNumericVars()) {
#       if (is.null(yRange())) return()
#       sliderInput("ylim", label="Y Range Filter",
#                   min=yRange()[1], max=yRange()[2], value=yRange(), round=FALSE)
#     } else if (y %in% finalDFFactorVars()) {
#       selectInput('ylim', label='Y Value Filter',
#                   choices=yFactorVarUniqVals(), 
#                   selected=yFactorVarUniqVals(),
#                   multiple=T)
#     }
#   }
# })


