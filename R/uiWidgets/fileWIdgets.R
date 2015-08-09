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

## CSV download button (for UI)
output$dlBtnCSV <- renderUI({
  downloadButton('dlCSV', 'Download')
})

## image download button (for UI)
output$dlBtnPlot <- renderUI({
  downloadButton('dlPlot', 'Export Plot')
})


## user-defined factor variables control options
# output$usrDefFacVarsCtrl <- renderUI({
#   if (is.null(dataset()) | is.null(factorVars())) return()  
#   selectInput('usrDefFacVars', 'User-Defined Factor Variables',
#               choices=colnames(dataset()), 
#               selected=factorVars(),
#               multiple=T)
# })
