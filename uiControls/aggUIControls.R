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
  aggMethOpts <- c('mean', 'sum', 'count', 'min', 'max', 'median')
  selectInput('aggMeth', 'Aggregation Method', choices=aggMethOpts, multiple=T)
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

