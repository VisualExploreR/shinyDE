## THIS METHOD OF "TEMPLATING" DOES NOT WORK;
## POSSIBLY DUE TO LIMITATIONS OF SHINY
#      
#       conditionalPanel(
#         condition = 'input.plotType=="line"',
#         #source('./views/linePlotCtrlsUI.R')$value
#         uiOutput('susie')
#       ),
#       
#       conditionalPanel(
#         condition = 'input.plotType=="scatter"',
#         #source('./views/scatterPlotCtrlsUI.R')$value
#         uiOutput('tan')
#       ),
# 
#       conditionalPanel(
#         condition = 'input.plotType=="graph"',
#         source('./views/graphPlotCtrlsUI.R')$value
#       ),
#       
#       conditionalPanel(
#         condition = 'input.plotType=="bar"',
#         source('./views/barPlotCtrlsUI.R')$value
#       ),
#       
#       conditionalPanel(
#         condition = 'input.plotType=="histogram"',
#         source('./views/histCtrlsUI.R')$value
#       ),
#       
#       conditionalPanel(
#         condition = 'input.plotType=="density"',
#         source('./views/densityPlotCtrlsUI.R')$value
#       ),
#       
#       conditionalPanel(
#         condition = 'input.plotType=="box"',
#         source('./views/boxPlotCtrlsUI.R')$value
#       ),
# 
#       conditionalPanel(
#         condition = 'input.plotType=="path"',
#         source('./views/pathPlotCtrlsUI.R')$value
#       )



#   observeEvent(input$submit, { 
#     brush <- input$zoom_brush 
#     if (!is.null(brush)) {
#       ranges$x <- c(brush$xmin, brush$xmax)
#       ranges$y <- c(brush$ymin, brush$ymax)
#     } else {
#       ranges$x <- NULL
#       ranges$y <- NULL
#     }
#   }) 





## base universal widgets loaded
generalBaseWidgetsLoaded <- reactive({
  wgtCtrls <- c('plotType', 'x')
  checkWidgetsLoaded(input, wgtCtrls)
})

# ## advanced universal widgets loaded
# generalAdvWidgetsLoaded <- reactive({
#   wgtCtrls <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale', 'alpha', 'coordFlip', 'xlim')
#   checkWidgetsLoaded(input, wgtCtrls)
# })
# 
# ## universal widgets fully loaded (base and advanced)
# generalWidgetsFullyLoaded <- reactive({
#   if (is.null(generalBaseWidgetsLoaded())) return(FALSE)
#   if (is.null(generalAdvWidgetsLoaded())) return(FALSE)
#   generalBaseWidgetsLoaded() & generalAdvWidgetsLoaded()
# })
# 
# ## universal widgets loaded
# generalWidgetsLoaded <- reactive({
#   if (is.null(input$showAdvCtrlWgts)) return()
#   if (input$showAdvCtrlWgts)
#     generalBaseWidgetsLoaded()
#   else
#     generalWidgetsFullyLoaded()
# })

