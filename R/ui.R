shinyUI(pageWithSidebar(
  headerPanel("Data Explorer"),
  sidebarPanel(
    
    ## dataset selection
    uiOutput('datasetCtrl'),
    hr(),
    
    ## file input/upload panel
    conditionalPanel(
      condition = 'input.conditionedPanels=="importTab"',
      source('./views/dataImportCtrlsUI.R', local=TRUE)$value
    ),  # end of file input/upload panel
    
    ## aggregation options
    conditionalPanel(
      condition = 'input.conditionedPanels=="tableTab"',
      source('./views/manAggCtrlsUI.R', local=TRUE)$value
    ),  # end of conditionalPanel for aggregation options
    
    ## plot options
    conditionalPanel(
      condition = 'input.conditionedPanels=="plotTab"',
      source('./views/plotCtrlsUI.R', local=TRUE)$value      
    )  # end of conditionalPanel for plot options
  ),  # end of sidebarPanel
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", 
                         br(),
                         uiOutput('dlBtnPlot'), 
                         br(),
                         plotOutput("plot"),
                         value='plotTab'
                ),
                tabPanel("Table", 
                         br(),
                         uiOutput('dlBtnCSV'),
                         br(),
                         DT::dataTableOutput("displayTable"),
                         value='tableTab'
                         ),
                tabPanel('Import',
                         value='importTab'
                ),
                id = "conditionedPanels"
    )    
  )
))