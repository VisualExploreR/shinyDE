
shinyUI(pageWithSidebar(
  
  headerPanel("Data Explorer"),
  
  sidebarPanel(
    checkboxInput("showImportOpts", 'Show/hide data import options.', TRUE), 
    checkboxInput("showAggOpts", "Show/hide data aggregation options.", TRUE),
    checkboxInput("showPlotOpts", 'Show/hide plot options.', TRUE), 
    
    hr(),
    
    ## file input/upload button
    conditionalPanel(
      condition = 'input.showImportOpts == true',
      
      fileInput('file', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      
      fluidRow(
        column(4,
               checkboxGroupInput('header', 'Header Options', 
                                  choices = c('Header'=TRUE),
                                  selected = c(TRUE))
        ),
        column(4,
               radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"')
        ),
        column(4,
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ',')
        )
      ),   # end of fluidRow for data import options
      hr()
    ),  # end of conditionalPanel for data import options
    
    ## aggregation options
    conditionalPanel(
      condition = 'input.showAggOpts == true',
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
      ),
      hr()
    ),
    
    ## plot options
    conditionalPanel(
      condition = 'input.showPlotOpts == true',
      uiOutput('plotTypeCtrl'),
      
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
        ), 
        column(6, 
               uiOutput('facetWrapCtrl'))
      )
      
    )  # end of conditionalPanel for plot options
  ),  # end of sidebarPanel
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Table", 
                         br(),
                         uiOutput('dlBtnCSV'),
                         br(),
                         DT::dataTableOutput("table")
                ),
                tabPanel("Plot", 
                         br(),
                         uiOutput('dlBtnPlot'),
                         br(),
                         plotOutput("plot"))                
    )    
  )
))