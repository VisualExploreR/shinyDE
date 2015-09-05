## import libraries
# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(DT)
# library(stringr)
# library(shinyBS)
# library(shinyjs)


## import functions
source('./functions/helper.R')
source('./functions/plot.R')
source('./functions/aggregate.R')


## file size options
# by default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10GB.
#old<-
options(shiny.maxRequestSize = 10000*1024^2)
#on.exit(options(old), add = TRUE)

shinyServer(function(input, output, session) {
  ## reactive variables
  source('./reactives/reactives.R', local=TRUE)  # general/miscellaneous
  source('./reactives/dataset.R', local=TRUE)  # dataset variables
  source('./reactives/plotWidgetsDisplayCond.R', local=TRUE)  # plot-related reactives
  source('./reactives/plotWidgetsLoadedCond.R', local=TRUE)  # plot-related reactives
  source('./reactives/plot.R', local=TRUE)  # plot-related reactives
  
  ## UI controls
  source('./uiWidgets/generalWidgets.R', local=TRUE)
  source('./uiWidgets/fileWidgets.R', local=TRUE)
  source('./uiWidgets/manAggWidgets.R', local=TRUE)
  source('./uiWidgets/plotWidgets.R', local=TRUE)
  
  ## download handlers
  source('./reactives/download.R', local=TRUE)
  
  ## observed events
  source('./observeEvents.R', local=TRUE)
  
})
