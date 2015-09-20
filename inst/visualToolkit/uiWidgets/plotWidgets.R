## plot type options
output$plotTypeCtrl <- renderUI({
  #selected <- ifelse(is.null(plotType_cache()), 'scatter', plotType_cache())
  selectInput(inputId = "plotType", label = "Plot Type", 
              choices = c('Scatter'='scatter', 'Line'='line',
                          'Bar'='bar', 'Histogram'='histogram', 
                          'Density'='density', 'Box'='box',
                          'Path'='path'
                          #'Violin'='violin', 
                          #'Image'='image', 
                          #'2-Density', 'density2d'
                          )
              )
})

## dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  if (is.null(displayRawVsManAgg())) return()
  if (displayRawVsManAgg()) {
    selectInput("rawVsManAgg", "Dataset Type",
                c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                rawVsManAgg())
  }
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  if (is.null(displayPlotAggMeth())) return()
  if (displayPlotAggMeth()) {
    aggMethOpts <- c('None', 'sum', 'mean', 'count', 'min', 'max', 'median')
    selectInput('plotAggMeth', 'Aggregation Method', aggMethOpts, plotAggMeth())
  }
})

## x-axis options
output$xCtrl <- renderUI({
  if (is.null(input$dataset)) return()
  selectInput('x', 'X', choices=xOpts(), selected=x_sel())
})

## y-axis options
output$yCtrl <- renderUI({
  if (is.null(input$dataset)) return()
  if (is.null(displayYCond())) return()
  if (displayYCond()) {
    selectInput('y', 'Y', choices=yOpts(), selected=y_sel())
  }
})

## color control options
output$colCtrl <- renderUI({
  if (is.null(displayColCond())) return()
  if (displayColCond()) {
    selectInput('color', 'Color', colOpts(), selected=color_sel())
  }
})

## treat-as-a-factor-variable option for color
output$treatAsFacVarColCtrl <- renderUI({
  if (is.null(displayTreatAsFacVarColCond())) return()
  if (displayTreatAsFacVarColCond()) {
    checkboxInput('treatAsFacVarCol', 'Treat as a factor variable.', value=treatAsFacVarCol())
  }
})

## fill control options
output$fillCtrl <- renderUI({
  if (is.null(displayFillCond())) return()
  if (displayFillCond()) {
    selectInput('fill', 'Fill', fillOpts(), fill_sel())
  }
})

## position (stack vs. dodge) control options
output$posCtrl <- renderUI({
  if (is.null(displayPosCond())) return()
  if (displayPosCond()) {
    selectInput('position', 'Position', c('None', 'dodge', 'stack'), position())
  }
})

## jitter options
output$jitCtrl <- renderUI({
  if (is.null(displayJitCond())) return()  
  if (displayJitCond()) {
    checkboxInput('jitter', 'Apply jitter effect', value=jitter())
  }
})

## geom smoothing options
output$smthCtrl <- renderUI({
  if (is.null(displaySmthCond())) return()
  if (displaySmthCond()) {
    if (all(c(input$x, input$y) %in% numericVars())) {
      selectInput('smooth', 'Smoothing Effect', 
                  c('None'='None', 'Linear'='lm', 'Non-linear'='auto'),
                  smoothOrig())
    }
  } 
})

## size options
output$sizeCtrl <- renderUI({
  if (is.null(displaySizeCond())) return()
  if (displaySizeCond()) {
    selectInput('size', 'Size', sizeOpts(), size_sel())
  }
})

## shape options
output$shapeCtrl <- renderUI({
  if (is.null(displayShapeCond())) return()
  if (displayShapeCond()) {
    selectInput('shape', 'Shape', shapeOpts(), shape_sel())
  }
})

## histogram binwidth options
output$binWidthCtrl <- renderUI({
  if (is.null(displayBinWidthCond())) return()
  if (is.null(histMaxBinWidth())) return()
  if (displayBinWidthCond()) {
    sliderInput('binWidth', label = "Bin Width",
                min=1, max=histMaxBinWidth(), value=binWidth(), step=1) 
  }
})

## density line color options
output$densBlkLineCondCtrl <- renderUI({
  if (is.null(displayDensBlkLineCond())) return()
  if (displayDensBlkLineCond()) {
    checkboxInput('densBlkLineCond', 'Draw black outline', value=densBlkLineCond())
  }
})

## points overlay options
output$ptsOverlayCondCtrl <- renderUI({  
  if (is.null(displayPtsOverlayCond())) return()
  if (displayPtsOverlayCond()) { 
    checkboxInput('ptsOverlayCond', 'Points Overlay', value=ptsOverlayCond())
  }
})



## row-wise facet options
output$facetRowCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetRow', 'Facet Row', facetOpts(), facetRow_sel())
  }
})

## column-wise facet options
output$facetColCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetCol', 'Facet Column', facetOpts(), facetCol_sel())
  }
})

## facet wrap options
output$facetWrapCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetWrap', 'Facet Wrap', facetOpts(), facetWrap_sel())
  }
})

## facet scale options
output$facetScaleCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetScale', 'Facet Scale',
                c('None'='fixed', 'Free X'='free_x', 
                  'Free Y'='free_y', 'Free X & Y'='free'),
                  facetScale()) 
  }
})

## alpha (opacity) options
output$alphaCtrl <- renderUI({
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts) {
    value <- ifelse(is.null(alpha()), 1, alpha())
    sliderInput("alpha", label = "Opacity",
                min=0, max=1, value=value, step=0.1)
  }
})


## size magnifier option
output$sizeMagCtrl <- renderUI({
  if (is.null(displaySizeMagCond())) return()
  if (displaySizeMagCond()) {
    value <- ifelse(is.null(sizeMag()), 4, sizeMag())
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=value, step=1)
  }
})

## coordinate flip options 
output$coordFlipCtrl <- renderUI({
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts) {
    checkboxInput('coordFlip', 'Flip X and Y coordinates.', value=coordFlip())
  }
})


## additional aggregation by options
# output$plotAddAggByCtrl <- renderUI({
#   if (is.null(displayPlotAddAggBy())) return()
#   if (displayPlotAddAggBy()) {
#     selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
#                 choices=plotAddAggByOpts(), multiple=T)
#   }
# })

## xlim control
output$xlimCtrl <- renderUI({
  if (is.null(displayXlim())) return()
  if (displayXlim()) {
    if (input$x %in% finalDFNumericVars()) {
      if (is.null(xRange())) return()
      sliderInput("xlim", label="X Range",
                  min=xRange()[1], max=xRange()[2], value=xRange(), round=FALSE)
    } else if (input$x %in% finalDFFactorVars()) {
      selectInput('xlim', label='X Value', 
                  choices=xFactorVarUniqVals(), 
                  #selected=xFactorVarUniqVals(),
                  multiple=T)
    }
  }
})

## ylim control
## note: ylim() is NOT applicable to histograms
output$ylimCtrl <- renderUI({
  if (is.null(displayYlim())) return()
  if (displayYlim()) {
    y <- y()
    
    if (y %in% finalDFNumericVars()) {
      if (is.null(yRange())) return()
      sliderInput("ylim", label="Y Range Filter",
                  min=yRange()[1], max=yRange()[2], value=yRange(), round=FALSE)
    } else if (y %in% finalDFFactorVars()) {
      selectInput('ylim', label='Y Value Filter',
                  choices=yFactorVarUniqVals(), 
                  #selected=yFactorVarUniqVals(),
                  multiple=T)
    }
  }
})





output$plotTitleCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    textInput('plotTitle', 'Plot Title', value=plotTitle())
})

output$xLabelCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    textInput('xLabel', 'X Label', value=xLabel())
})

output$yLabelCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    textInput('yLabel', 'Y Label', value=yLabel())
})

output$labelFontSizeCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    numericInput('labelFontSize', 'Label Font Size', value=labelFontSize(), step=1)
})

output$labelFontFamilyCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts()) {
    labelFontFamilyOpts <- c('Calibri TT', 'sans', 'serif', 'mono', 'Times', 'Helvetica', 'Courier')
    selectInput('labelFontFamily', 'Label Font Family', labelFontFamilyOpts, labelFontFamily())
  }
})

output$labelFontFaceCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts()) {
    labelFontFaceOpts <- c('plain', 'bold', 'italic', 'bold.italic')
    selectInput('labelFontFace', 'Label Font Face', labelFontFaceOpts, labelFontFace())
  }
})

output$labelFontColorCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    colourInput('labelFontColor', 'Label Font Color', value=labelFontColor())
})

output$hjustCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    numericInput('hjust', 'Horizontal Adjust', value=hjust(), min=-1, max=1, step=0.1)
})

output$vjustCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    numericInput('vjust', 'Vertical Adjust', value=vjust(), min=-1, max=1, step=0.1)
})



#### show/hide checkbox widgets
## show aesthetic controls
output$showAesWgtsCtrl <- renderUI({
  checkboxInput('showAesWgts', 'Show aesthetics widgets', value=FALSE)
})

## show facet controls
output$showFacetWgtsCtrl <- renderUI({
  checkboxInput('showFacetWgts', 'Show facet widgets', value=FALSE)
})

## show X & Y range controls
output$showXYRangeWgtsCtrl <- renderUI({
  checkboxInput('showXYRangeWgts', 'Show range widgets', value=FALSE)
})

## show aggregation controls
output$showPlotAggWgtCtrl <- renderUI({
  checkboxInput('showPlotAggWgt', 'Show plot aggregation widget', value=FALSE)
})

## show theme controls
output$showThemeWgtsCtrl <- renderUI({
  checkboxInput('showThemeWgts', 'Show theme widgets', value=FALSE)
})

## show dataset type and plot aggregation method controls
output$showDSTypeAndPlotAggWgtsCtrl <- renderUI({
  checkboxInput('showDSTypeAndPlotAggWgts', 'Show dataset type and aggregation widgets', value=FALSE)
})
