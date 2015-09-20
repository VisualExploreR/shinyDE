
## x selected
x_sel <- reactive({
  if (is.null(input$dataset)) return()
  
  if (!is.null(x()) & !is.null(xOpts()))
    if (x() %in% xOpts())
      return(x())
  
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
  
  if (!is.null(yOrig()) & !is.null(yOpts()))
    if (yOrig() %in% yOpts())
      return(yOrig())
  
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
  if (!is.null(colorOrig()) & !is.null(colOpts()))
    if (colorOrig() %in% colOpts())
      return(colorOrig())      
})

# ## treat-as-a-factor-variable option for color
# output$treatAsFacVarColCtrl <- renderUI({
#   if (is.null(displayTreatAsFacVarColCond())) return()
#   if (displayTreatAsFacVarColCond()) {
#     checkboxInput('treatAsFacVarCol', 'Treat as a factor variable.', value=treatAsFacVarColOrig())
#   }
# })

## fill selected
fill_sel <- reactive({
  if (!is.null(fillOrig()) & !is.null(fillOpts()))
    if (fillOrig() %in% fillOpts()) 
      return(fillOrig())
})

## size selected
size_sel <- reactive({
  if (!is.null(sizeOrig()) & !is.null(sizeOpts()))
    if (sizeOrig() %in% sizeOpts())
      return(sizeOrig())
})

## shape selected
shape_sel <- reactive({
  if (!is.null(shapeOrig()) & !is.null(shapeOpts()))
    if (shapeOrig() %in% shapeOpts())
      return(shapeOrig())
})

## facet row selected
facetRow_sel <- reactive({
  if (!is.null(facetRowOrig()) & !is.null(facetOpts()))
    if (facetRowOrig() %in% facetOpts())
      return(facetRowOrig())
})

## facet col selected
facetCol_sel <- reactive({
  if (!is.null(facetColOrig()) & !is.null(facetOpts()))
    if (facetColOrig() %in% facetOpts())
      return(facetColOrig())
})

## facet wrap selected
facetWrap_sel <- reactive({
  if (!is.null(facetWrapOrig()) & !is.null(facetOpts()))
    if (facetWrapOrig() %in% facetOpts())
      return(facetWrapOrig())
})

# 
# ## label font family selected
# labelFontFamily_sel <- reactive({
#   if (is.null(labelFontFamily())) return()
#   labelFontFamily()
# })
# 
# ## label font face selected
# labelFontFace_sel <- reactive({
#   if (is.null(labelFontFace())) return()
#   labelFontFace()
# })
# 
# ## label font size value
# labelFontSize_val <- reactive({
#   
# })
# 
# ## label font color value
# labelFontColor_val <- reactive({
#   
# })
# 
# ## hjust value
# # hjust_val <- reactive({
# #   if (is.n)  
# # })
# 
# ## vjust value
# vjust_val <- reactive({
#   if (is.null(vjust())) return()
#   vjust()
# })