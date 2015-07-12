#### useful links
## dplyr aggregation
# http://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr
# http://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions
# http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input

## shiny reactive variable persistence
# https://github.com/jcheng5/shiny-resume/pull/1
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs
# http://stackoverflow.com/questions/25749749/shiny-ui-save-the-changes-in-the-inputs
# https://github.com/trestletech/shinyStore/blob/master/inst/examples/01-persist/ui.R

## datatable
# http://rstudio.github.io/DT/server.html

## progress bar
# http://shiny.rstudio.com/articles/progress.html




## this function renames column names of aggregate df
renameAggColNames <- function(df, aggBy, aggTarget, aggMeth) {
  cntInAggMeth <- 'count' %in% aggMeth
  aggMeth <- setdiff(aggMeth, 'count')
  
  colnames <- c(aggBy)
  for (meth in aggMeth) {
    for (targ in aggTarget) {
      colname <- paste(targ, meth, sep='_')
      colnames <- c(colnames, colname)
    }
  }

  if (cntInAggMeth) {
    colnames <- c(colnames, 'count')
  }
  
  colnames(df) <- colnames
  return(df)
}

## this function aggregates raw data using functions from dplyr
aggregate <- function(df, aggBy, aggTarget, aggMeth, nRndDeci=2) {
  
  ## aggBy can contain duplicates when x and facet variables are the same
  aggBy <- unique(aggBy)
  
  ## select independent/dependent variables
  df <- df[, c(aggBy, aggTarget)]

  ## conditional to perform count later
  cntInAggMeth <- 'count' %in% aggMeth
  
  ## conditional to perform median later
  medInAggMeth <- 'median' %in% aggMeth
  
  ## select non-problematic aggregation methods
  aggMeth <- setdiff(aggMeth, c('count', 'median'))
  
  ## convert character vector to list of symbols
  dots <- lapply(aggBy, as.symbol)

  ## group data
  grp <- dplyr::group_by_(df, .dots=dots)

  ## perform non-problematic aggregation by column
  agg <- dplyr::summarise_each(grp, funs_(aggMeth))

  ## convert to data frame
  agg <- as.data.frame(agg)
  
  ## rename column names
  agg <- renameAggColNames(agg, aggBy, aggTarget, aggMeth)

  ## attach aggregate counts if requested
  if (cntInAggMeth) {
    cnt <- dplyr::summarise(grp, count=n())
    agg$count <- cnt$count
  }
  
  ## perform median aggregation by column
  if (medInAggMeth) {
    medAgg <- dplyr::summarise_each(grp, ~ median)
    nMedAggCol <- length(aggBy)
    ncol <- ncol(medAgg)
    colnames(medAgg) <- c(aggBy, paste0(colnames(medAgg)[(nMedAggCol+1):ncol], '_median'))
    agg <- merge(agg, medAgg, by=aggBy)
  }

  ## find numeric columns and round
  numericVars <- getNumericVarNames(agg)
  #agg[numericVars] <- sapply(agg[numericVars], function(x) {round(x, nRndDeci)})
  
  for (numericVar in numericVars) {
    agg[[numericVar]] <- round(agg[[numericVar]], nRndDeci)
  }
    
  ## return
  agg
}

## this function calculates share percentage (or relative frequency)
calcShare <- function(df, shareOf, shareTarget, nRndDeci=2, displayPerc=TRUE) {
  ## calculate share if necessary
  if (is.null(df) | is.null(shareOf) | is.null(shareTarget)) {return(df)}
  
  colnames <- colnames(df)
  if (!(shareOf %in% colnames) | !(shareTarget %in%colnames)) {return(df)}

  dots <- lapply(shareOf, as.symbol)
  grp <- group_by_(df, .dots=dots)
  agg <- summarise_(grp, sum = sprintf('sum(%s)', shareTarget))
  agg <- as.data.frame(agg)
  df <- merge(df, agg, by=shareOf)

  df$share <- df[[shareTarget]] / df$sum
  df$sum <- NULL
  
  if (displayPerc) {
    df$share <- round(df$share * 100, nRndDeci)
    df$share <- paste(as.character(df$share), '%')
  } else {
    df$share <- round(df$share, nRndDeci)
  }

  return(df)
}





