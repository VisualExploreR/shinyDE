## plot options
verticalLayout(
  uiOutput('plotTypeCtrl'),
  
  fluidRow(
    column(6,
           uiOutput('rawVsManAggCtrl')
    ),
    column(6,
           uiOutput('plotAggMethCtrl')
    )
  ),
  
  br(),
  br(),

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
  
  uiOutput('ptsOverlayCondCtrl'),
  
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
           uiOutput('smthCtrl')
    )
  ),

  uiOutput('jitCtrl'),
  uiOutput('binWidthCtrl'),
  uiOutput('plotAddAggByCtrl'),
  uiOutput('showAdvCtrlWgtsCtrl'),
  
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
  
  uiOutput('densBlkLineCondCtrl'),
  
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('xlimCtrl'),
  uiOutput('ylimCtrl'),
  uiOutput('coordFlipCtrl')
)