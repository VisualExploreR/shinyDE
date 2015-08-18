shinyUI(pageWithSidebar(
  headerPanel("Data Explorer"),
  sidebarPanel(
    
    ## dataset selection
    uiOutput('datasetCtrl'),
    
    ## enable reactive option
    uiOutput('reactiveCtrl'),
    
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
    ),  # end of conditionalPanel for plot options
    
    ## reactive vs. upon-manual-submit calculations
    uiOutput('submitCtrl')
    
  ),  # end of sidebarPanel
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", 
                         br(),
                         uiOutput('dlBtnPlot'), 
                         br(),
                         plotOutput("plot", brush=brushOpts(id="zoom_brush", resetOnNew=TRUE)),
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