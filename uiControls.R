## file input select control
output$fileInputSelectCtrl <- renderUI({
  fileInput('file', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  )  
})

## file input header control
output$fileInputHeaderCtrl <- renderUI({
  checkboxGroupInput('header', 'Header Options', 
                     choices = c('Header'=TRUE),
                     selected = c(TRUE))  
})

## file input quote control
output$fileInputQuoteCtrl <- renderUI({
  radioButtons('quote', 'Quote',
               c(None='',
                 'Double Quote'='"',
                 'Single Quote'="'"),
               '"')  
})

## file input separator control
output$fileInputSepCtrl <- renderUI({
  radioButtons('sep', 'Separator',
               c(Comma=',',
                 Semicolon=';',
                 Tab='\t'),
               ',')  
})

## dataset drop-down options
output$datasetCtrl <- renderUI({
  selectInput("dataset", "Choose a dataset:", 
              choices = rawDatasetNames())
})

## aggregation-by options
output$aggByCtrl <- renderUI({
  aggByOpts <- origVars()
  selectInput('aggBy', 'Aggregate By', choices=aggByOpts, multiple=T)
})

## aggregation target options
output$aggTargetCtrl <- renderUI({
  aggTargetOpts <- origNumericVars()
  selectInput('aggTarget', 'Aggregation Target', choices=aggTargetOpts, multiple=T)
})

## aggregation method options
output$aggMethCtrl <- renderUI({
  aggMethOpts <- c('mean', 'sum', 'count')
  selectInput('aggMeth', 'Aggregation Method', choices=aggMethOpts, multiple=T)
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  aggMethOpts <- c('sum', 'mean', 'count')
  selectInput('plotAggMeth', 'Aggregation Method', aggMethOpts)
  #selectInput('plotAggMeth', 'Aggregation Method', choices=c(aggMethOpts, 'None'))
})

## aggregation share of
# output$shareOfCtrl <- renderUI({
#   shareOfOpts <- factorVars()
#   selectInput('shareOf', 'Share Of', choices=c('None', shareOfOpts))
# })

## aggregation share target
# output$shareTargetCtrl <- renderUI({
#   shareTargetOpts <- numericVars()
#   selectInput('shareTarget', 'Share Target', choices=c('None', shareTargetOpts))
# })

## x-axis options
output$xCtrl <- renderUI({
  selectInput('x', 'X', xOpts())
})

## y-axis options
output$yCtrl <- renderUI({
  selectInput('y', 'Y', yOpts())
})

## color control options
output$colCtrl <- renderUI({
  selectInput('color', 'Color', colOpts())
})

## row-wise facet-options
output$facetRowCtrl <- renderUI({
  selectInput('facetRow', 'Facet Row', c(None='.', facetOpts()))
})

## column-wise facet options
output$facetColCtrl <- renderUI({
  selectInput('facetCol', 'Facet Column', c(None='.', facetOpts()))
})

## facet wrap options
output$facetWrapCtrl <- renderUI({
  selectInput('facetWrap', 'Facet Wrap', c(None='.', facetOpts()))
})

## plot type options
output$plotTypeCtrl <- renderUI({
  dataset <- dataset(); if (is.null(dataset)) {return(NULL)}
  selectInput(inputId = "plotType", label = "Plot Type", 
              choices = c('Line'='line', 'Scatter'='scatter', 'Bar'='bar'),
              multiple = FALSE)
})

## dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  radioButtons("rawVsManAgg", label = tags$strong(h5("Dataset Type")),
               choices = list("Raw Dataset" = 'raw', "Manually Aggregated Dataset" = 'manAgg'), 
               selected = 'raw', inline=TRUE)
})

## raw vs. semi-automatic aggregation 
output$semiAutoAggCtrl <- renderUI({
  radioButtons('semiAutoAgg', label = tags$strong(h5("Semi-auto Aggregation")),
               choices = list("Allowed" = 'allowed', "Disabled" = 'disabled'),
               selected = 'disabled', inline = TRUE)
})

## CSV download button (for UI)
output$dlBtnCSV <- renderUI({
  downloadButton('dlCSV', 'Download')
})

## image download button (for UI)
output$dlBtnPlot <- renderUI({
  downloadButton('dlPlot', 'Export Plot')
})

