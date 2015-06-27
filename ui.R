shinyUI(pageWithSidebar(
  headerPanel("Data Explorer"),
  sidebarPanel(
    
    ## file input/upload panel
    uiOutput('datasetCtrl'),    
    uiOutput('fileInputSelectCtrl'),
    fluidRow(
      column(4,
             uiOutput('fileInputHeaderCtrl')
      ),
      column(4,
             uiOutput('fileInputQuoteCtrl')
      ),
      column(4,
             uiOutput('fileInputSepCtrl')
      )
    ),   # end of fluidRow for data import options        
    
    #checkboxInput("hideImportOpts", 'Hide data import options.', FALSE), 
    hr(),
    ## end of file input/upload panel
    
    ## aggregation options
    conditionalPanel(
      condition = 'input.conditionedPanels == "tableTab"',
      fluidRow(
        column(6,
               uiOutput('aggByCtrl')
        ),
        column(6,
               uiOutput('aggTargetCtrl')
        )
      ),
      fluidRow(
        column(6, 
               uiOutput('aggMethCtrl')
        )
      ),
      fluidRow(
        column(6,
               uiOutput('shareOfCtrl')
        ),
        column(6,
               uiOutput('shareTargetCtrl')
        )        
      )
    ),  # end of conditionalPanel for aggregation options
    
    ## plot options
    conditionalPanel(
      condition = 'input.conditionedPanels == "plotTab"',
      uiOutput('plotTypeCtrl'),
      
      uiOutput('rawVsManAggCtrl'),
      uiOutput('semiAutoAggCtrl'),

      conditionalPanel(
        condition = 'input.semiAutoAgg == "allowed"',
        uiOutput('plotAggMethCtrl')
      ),
      
      fluidRow(
        column(6,
               uiOutput('xCtrl')             
        ),
        column(6,
               uiOutput('yCtrl')             
        )
      ),
      fluidRow(
        column(6,
               uiOutput('facetRowCtrl')             
        ),
        column(6,
               uiOutput('facetColCtrl')             
        )
      ),
      fluidRow(
        column(6,
               uiOutput('colCtrl')               
        )
      ),
      
      fluidRow(
        column(6, 
               uiOutput('sizeCtrl')
        ),
        column(6,
               uiOutput('shapeCtrl')
        )
      ),
      
      fluidRow(
        column(6, 
               uiOutput('jitCtrl')
        ),
        column(6,
               uiOutput('smthCtrl')
        )
      ),
      
      uiOutput('alphaCtrl')
      
      #         column(6,
      #                uiOutput('facetWrapCtrl')
      #         )
      
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
                id = "conditionedPanels"
    )    
  )
))