## plot options
verticalLayout(
  uiOutput('plotTypeCtrl'),
  uiOutput('ptsOverlayCondCtrl'),
  
  fluidRow(
    column(6,
           uiOutput('rawVsManAggCtrl')               
    ),
    column(6,
           uiOutput('semiAutoAggCtrl')        
    )
  ),
  
  conditionalPanel(
    condition = 'input.semiAutoAgg=="allowed"',
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
           uiOutput('colCtrl'),
           uiOutput('treatAsFacVarColCtrl')
    )
  ),
  
  fluidRow(
    column(6,
           uiOutput('fillCtrl')           
    ),
    column(6,
           uiOutput('posCtrl')
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
           uiOutput('facetWrapCtrl')
    ),
    column(6,
           uiOutput('facetScaleCtrl')       
    )
  ),
  
  uiOutput('plotAddAggByCtrl'),
  uiOutput('binWidthCtrl'),
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('xlimCtrl'),
  uiOutput('ylimCtrl'),
  
  uiOutput('densBlkLineCondCtrl'),
  uiOutput('coordFlipCtrl'),
  
  ## enable reactive option
  uiOutput('reactiveCtrl'),
  
  ## reactive vs. upon-manual-submit calculations
  uiOutput('submitCtrl')
  
  
)