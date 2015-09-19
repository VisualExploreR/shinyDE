
## x selected
x_sel <- reactive({
  if (is.null(input$dataset)) return()
  
  if (!is.null(x_cache()))
    if (x_cache() %in% xOpts())
      return(x_cache())
  
  selected <- NULL
  if (input$dataset=='diamonds') 
    selected <- 'carat'
  else if (input$dataset=='mtcars')
    selected <- 'mpg'
  else if (input$dataset=='rock')
    selected <- 'area'
  return(selected)
})

## y selected
y_sel <- reactive({
  if (is.null(input$dataset)) return()
  
  if (!is.null(y_cache()))
    if (y_cache() %in% yOpts())
      return(y_cache())
  
  selected <- NULL
  if (input$dataset=='diamonds')
    selected <- 'price'
  else if (input$dataset=='mtcars')
    selected <- 'hp'
  else if (input$dataset=='rock')
    selected <- 'peri'
  return(selected)
})

## color selected
color_sel <- reactive({
  if (!is.null(color_cache()))
    if (color_cache() %in% colOpts())
      return(color_cache())      
})

# ## treat-as-a-factor-variable option for color
# output$treatAsFacVarColCtrl <- renderUI({
#   if (is.null(displayTreatAsFacVarColCond())) return()
#   if (displayTreatAsFacVarColCond()) {
#     checkboxInput('treatAsFacVarCol', 'Treat as a factor variable.', value=treatAsFacVarCol_cache())
#   }
# })

## fill selected
fill_sel <- reactive({
  if (!is.null(fill_cache()))
    if (fill_cache() %in% fillOpts()) 
      return(fill_cache())
})


# ## position (stack vs. dodge) control options
# output$posCtrl <- renderUI({
#   if (is.null(displayPosCond())) return()
#   if (displayPosCond()) {
#     selectInput('position', 'Position', c('None', 'dodge', 'stack'), position_cache())
#   }
# })

## jitter options
# output$jitCtrl <- renderUI({
#   if (is.null(displayJitCond())) return()  
#   if (displayJitCond()) {
#     checkboxInput('jitter', 'Apply jitter effect', value=jitter_cache())
#   }
# })

# ## geom smoothing options
# output$smthCtrl <- renderUI({
#   if (is.null(displaySmthCond())) return()
#   if (displaySmthCond()) {
#     if (all(c(input$x, input$y) %in% numericVars())) {
#       selectInput('smooth', 'Smoothing Effect', 
#                   c('None'='None', 'Linear'='lm', 'Non-linear'='auto'),
#                   smooth_cache())
#     }
#   } 
# })


## size selected
size_sel <- reactive({
  if (!is.null(size_cache()))
    if (size_cache() %in% sizeOpts())
      return(size_cache())
})

## shape selected
shape_sel <- reactive({
  if (!is.null(shape_cache()))
    if (shape_cache() %in% shapeOpts())
      return(shape_cache())
})

# ## histogram binwidth options
# output$binWidthCtrl <- renderUI({
#   if (is.null(displayBinWidthCond())) return()
#   if (is.null(histMaxBinWidth())) return()
#   if (displayBinWidthCond()) {
#     sliderInput('binWidth', label = "Bin Width",
#                 min=1, max=histMaxBinWidth(), value=binWidth_cache(), step=1) 
#   }
# })
# 
# ## density line color options
# output$densBlkLineCondCtrl <- renderUI({
#   if (is.null(displayDensBlkLineCond())) return()
#   if (displayDensBlkLineCond()) {
#     checkboxInput('densBlkLineCond', 'Draw black outline', value=densBlkLineCond_cache())
#   }
# })
# 
# ## points overlay options
# output$ptsOverlayCondCtrl <- renderUI({  
#   if (is.null(displayPtsOverlayCond())) return()
#   if (displayPtsOverlayCond()) { 
#     checkboxInput('ptsOverlayCond', 'Points Overlay', value=ptsOverlayCond_cache())
#   }
# })
# 

## facet row selected
facetRow_sel <- reactive({
  if (!is.null(facetRow_cache()))
    if (facetRow_cache() %in% facetOpts())
      return(facetRow_cache())
})

## facet col selected
facetCol_sel <- reactive({
  if (!is.null(facetCol_cache()))
    if (facetCol_cache() %in% facetOpts())
      return(facetCol_cache())
})

## facet wrap selected
facetWrap_sel <- reactive({
  if (!is.null(facetWrap_cache()))
    if (facetWrap_cache() %in% facetOpts())
      return(facetWrap_cache())
})

# ## facet scale options
# output$facetScaleCtrl <- renderUI({
#   if (is.null(input$showFacetWgts)) return()
#   if (input$showFacetWgts) {
#     selectInput('facetScale', 'Facet Scale',
#                 c('None'='fixed', 'Free X'='free_x', 
#                   'Free Y'='free_y', 'Free X & Y'='free'),
#                 facetScale_cache()) 
#   }
# })
# 
# ## alpha (opacity) options
# output$alphaCtrl <- renderUI({
#   if (is.null(input$showAesWgts)) return()
#   if (input$showAesWgts) {
#     value <- ifelse(is.null(alpha_cache()), 1, alpha_cache())
#     sliderInput("alpha", label = "Opacity",
#                 min=0, max=1, value=value, step=0.1)
#   }
# })
# 
# 
# ## size magnifier option
# output$sizeMagCtrl <- renderUI({
#   if (is.null(displaySizeMagCond())) return()
#   if (displaySizeMagCond()) {
#     value <- ifelse(is.null(sizeMag_cache()), 4, sizeMag_cache())
#     sliderInput("sizeMag", label="Size Magnifier",
#                 min=1, max=25, value=value, step=1)
#   }
# })
# 
# ## coordinate flip options 
# output$coordFlipCtrl <- renderUI({
#   if (is.null(input$showAesWgts)) return()
#   if (input$showAesWgts) {
#     checkboxInput('coordFlip', 'Flip X and Y coordinates.', value=coordFlip_cache())
#   }
# })
# 
# 
# ## additional aggregation by options
# # output$plotAddAggByCtrl <- renderUI({
# #   if (is.null(displayPlotAddAggBy())) return()
# #   if (displayPlotAddAggBy()) {
# #     selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
# #                 choices=plotAddAggByOpts(), multiple=T)
# #   }
# # })
# 
# ## xlim control
# output$xlimCtrl <- renderUI({
#   if (is.null(displayXlim())) return()
#   if (displayXlim()) {
#     if (input$x %in% finalDFNumericVars()) {
#       if (is.null(xRange())) return()
#       sliderInput("xlim", label="X Range",
#                   min=xRange()[1], max=xRange()[2], value=xRange(), round=FALSE)
#     } else if (input$x %in% finalDFFactorVars()) {
#       selectInput('xlim', label='X Value', 
#                   choices=xFactorVarUniqVals(), 
#                   #selected=xFactorVarUniqVals(),
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
#                   #selected=yFactorVarUniqVals(),
#                   multiple=T)
#     }
#   }
# })
# 




