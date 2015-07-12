## CSV download handler
output$dlCSV <- downloadHandler(
  filename = function() { 
    ts <- gsub(' |-|:', '', as.character(Sys.time()))
    paste0('output_', ts, '.csv') 
  },
  content = function(file) {
    write.csv(finalDF(), file, row.names=F)
    #write.csv(dataset(), file, row.names=F)
  }
)

## image download handler
output$dlPlot <- downloadHandler(
  filename = function() { 
    ts <- gsub(' |-|:', '', as.character(Sys.time()))
    paste0('output_', ts, '.png') 
  },
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = plotInput(), device = device)
  }
)

## register the final dataset to be used upon AJAX call from UI
action <- reactive({DT::dataTableAjax(session, finalDF())})

