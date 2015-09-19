
#### current widget values

## dataset name
datasetName <- reactive({
  if (is.null(input$dataset)) return()
  inpu$datasetName
})

## plot type 
plotType <- reactive({
  if (is.null(input$plotType)) return()
  input$plotType
})

## plot agg method
plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) return('none')
  input$plotAggMeth
})

## raw or man agg dataset type
rawOrManAgg <- reactive({
  if (is.null(input$rawVsManAgg)) return()
  input$rawVsManAgg
})

## x
x <- reactive({
  if (is.null(input$x)) return()
  input$x
})

## y 
y <- reactive({
  if (is.null(input$y)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(finalDF())) return()
  if (is.null(semiAutoAggOn())) return()
  retval <- ensureProperVarName(colnames=colnames(finalDF()), var=input$y, aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
  retval
})

## color original 
colorOrig <- reactive({
  if (is.null(input$color)) return()
  input$color
})

## color 
color <- reactive({
  if (is.null(plotDF())) return()
  if (is.null(input$color)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(semiAutoAggOn())) return()
  col <- ensureProperVarName(colnames=colnames(plotDF()), var=input$color, aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())  
  col <- convertNoneToNULL(col)
  col
})

## color as factor
colorAsFactor <- reactive({
  if (is.null(color())) return()
  varNameAsFactorOrNULL(color())
})

## treat as factor variable (for color)
treatAsFacVarCol <- reactive({
  if (is.null(input$treatAsFacVarCol)) return(FALSE)
  input$treatAsFacVarCol
})

## size original 
sizeOrig <- reactive({
  if (is.null(input$size)) return()
  input$size
})

## size 
size <- reactive({
  if (is.null(plotDF())) return()
  if (is.null(input$size)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(semiAutoAggOn())) return()
  sz <- ensureProperVarName(colnames=colnames(plotDF()), var=input$size, aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
  sz <- convertNoneToNULL(sz)
  sz
})

## fill 
fill <- reactive({
  if (is.null(input$fill)) return()
  convertNoneToNULL(input$fill)
})

## fill as factor
fillAsFactor <- reactive({
  if (is.null(fill())) return()
  varNameAsFactorOrNULL(fill())  
})

## position 
position <- reactive({
  if (is.null(input$position)) return()
  convertNoneToNULL(input$position)
})

## jitter 
## should return either "jitter" or NULL
jitter <- reactive({
  if (is.null(input$jitter)) return()
  jit <- input$jitter
  if (jit) 
    jit <- 'jitter' 
  else 
    jit <- NULL
  jit
})

## alpha 
alpha <- reactive({
  alp <- input$alpha
  if (is.null(alp)) alp <- 1
  alp
})

## size magnifier 
sizeMag <- reactive({
  sm <- input$sizeMag
  if (is.null(sm)) sm <- 4
  sm
})

#### FINISH HERE!!!!

## density black line condition
densBlkLineCond <- reactive({
  if (is.null(input$densBlkLineCond)) return(FALSE)
  input$densBlkLineCond
})

## shape
shape <- reactive({
  if (is.null(input$shape)) return()
  convertNoneToNULL(input$shape)
})

## shape as factor
shapeAsFactor <- reactive({
  if (is.null(shape())) return()
  varNameAsFactorOrNULL(shape())
})

## smooth
smooth <- reactive({
  if (is.null(input$smooth)) return()
  convertNoneToNULL(input$smooth)
})

## coordinate flip
coordFlip <- reactive({
  if (is.null(input$coordFlip)) return(FALSE)
  input$coordFlip
})

## bin width
binWidth <- reactive({
  if (is.null(input$binWidth)) return()
  input$binWidth
})

## points overlay condition
ptsOverlayCond <- reactive({
  if (is.null(input$ptsOverlayCond)) return(FALSE)
  input$ptsOverlayCond
})

## facet row
facetRow <- reactive({
  if (is.null(input$facetRow)) return('.')
  ifelse(input$facetRow=='None', '.', input$facetRow)
})

## facet column
facetCol <- reactive({
  if (is.null(input$facetCol)) return('.')
  ifelse(input$facetCol=='None', '.', input$facetCol)
})

## facet wrap
facetWrap <- reactive({
  if (is.null(input$facetWrap)) return('.')
  ifelse(input$facetWrap=='None', '.', input$facetWrap)
})

## facet scale
facetScale <- reactive({
  if (is.null(input$facetScale)) return('none')
  input$facetScale
})

## facet grids
facetGrids <- reactive({
  if (is.null(facetRow()) | is.null(facetCol())) return('. ~ .')
  paste(facetRow(), '~', facetCol())
})

## xlim
xlim <- reactive({
  if (is.null(input$xlim)) return()
  input$xlim
})

## ylim
ylim <- reactive({
  if (is.null(input$ylim)) return()
  input$ylim
})





#### cached widget values

## dataset name cache (unnecessary since it's never hidden)
datasetName_cache <- reactive({
  if (is.null(input$dataset)) return()
  input$dataset
})

## plot type cache (unnecessary since it's never hidden)
plotType_cache <- reactive({
  if (is.null(input$plotType)) return()
  input$plotType
})

## plot agg method cache
plotAggMeth_cache <- reactive({
  if (is.null(input$plotAggMeth)) return()
  input$plotAggMeth
})

## x cache (unnecessary since it's never hidden)
x_cache <- reactive({
  if (is.null(input$x)) return()
  input$x
})

## y cache (unnecessary since it's never hidden)
y_cache <- reactive({
  if (is.null(input$y)) return()
  input$y
})

## color cache
color_cache <- reactive({
  if (is.null(input$color)) return()
  input$color
})

## treat as factor variable (for color) cache
treatAsFacVarCol_cache <- reactive({
  if (is.null(input$treatAsFacVarCol)) return()
  input$treatAsFacVarCol
})

## size cache
size_cache <- reactive({
  if (is.null(input$size)) return()
  input$size
})

## fill cache
fill_cache <- reactive({
  if (is.null(input$fill)) return()
  input$fill
})

## position cache
position_cache <- reactive({
  if (is.null(input$position)) return()
  input$position
})

## jitter cache
jitter_cache <- reactive({
  if (is.null(input$jitter)) return()
  input$jitter
})

## alpha cache
alpha_cache <- reactive({
  if (is.null(input$alpha)) return()
  input$alpha
})

## size magnifier cache
sizeMag_cache <- reactive({
  if (is.null(input$sizeMag)) return()
  input$sizeMag
})

## density black line condition cache
densBlkLineCond_cache <- reactive({
  if (is.null(input$densBlkLineCond)) return()
  input$densBlkLineCond
})

## shape cache
shape_cache <- reactive({
  if (is.null(input$shape)) return()
  input$shape
})

## smooth cache
smooth_cache <- reactive({
  if (is.null(input$smooth)) return()
  input$smooth
})

## coordinate flip cache
coordFlip_cache <- reactive({
  if (is.null(input$coordFlip)) return()
  input$coordFlip
})

## bin width cache
binWidth_cache <- reactive({
  if (is.null(input$binWidth)) return()
  input$binWidth
})

## points overlay condition cache
ptsOverlayCond_cache <- reactive({
  if (is.null(input$ptsOverlayCond)) return()
  input$ptsOverlayCond
})

## facet row cache
facetRow_cache <- reactive({
  if (is.null(input$facetRow)) return()
  input$facetRow
})

## facet column cache
facetCol_cache <- reactive({
  if (is.null(input$facetCol)) return()
  input$facetCol
})

## facet wrap cache
facetWrap_cache <- reactive({
  if (is.null(input$facetWrap)) return()
  input$facetWrap
})

## facet scale cache
facetScale_cache <- reactive({
  if (is.null(input$facetScale)) return()
  input$facetScale
})

## xlim cache
xlim_cache <- reactive({
  if (is.null(input$xlim)) return()
  input$xlim
})

## ylim cache
ylim_cache <- reactive({
  if (is.null(input$ylim)) return()
  input$ylim
})

