
## plot agg method
plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) return('none')
  input$plotAggMeth
})

## y
y <- reactive({
  if (is.null(input$y)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(finalDF())) return()
  if (is.null(semiAutoAggOn())) return()
#   dget(input$y, file='')
#   dput(input$y, file='./_input_cache/hi')
  retval <- ensureProperVarName(colnames=colnames(finalDF()), var=input$y, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())
  retval
})

## color
color <- reactive({
  dataset <- plotDF()
  if (is.null(dataset)) return()
  if (is.null(input$color)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(semiAutoAggOn())) return()
  col <- ensureProperVarName(colnames=colnames(dataset), var=input$color, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())  
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

## size
size <- reactive({
  dataset <- plotDF()
  if (is.null(dataset)) return()
  if (is.null(input$size)) return()
  if (is.null(input$plotAggMeth)) return()
  if (is.null(semiAutoAggOn())) return()
  sz <- ensureProperVarName(colnames=colnames(dataset), var=input$size, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())
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
  if (is.null(input$position)) return('dodge')
  convertNoneToNULL(input$position)
})

## jitter (should return either "jitter" or NULL)
jitter <- reactive({
  jit <- input$jitter
  if (!is.null(input$jitter)) {
    if (input$jitter) 
      jit <- 'jitter' 
    else 
      jit <- NULL
  }
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
  if (is.null(input$facetRow)) rturn('.')
  ifelse(input$facetRow=='None', '.', input$facetRow)
})

## facet column
facetCol <- reactive({
  if (is.null(input$facetCol)) rturn('.')
  ifelse(input$facetCol=='None', '.', input$facetCol)
})

## facet wrap
facetWrap <- reactive({
  if (is.null(input$facetWrap)) rturn('.')
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