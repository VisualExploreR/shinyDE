
## note: the functions below will find year (YYYY), month (YYYY-MM), 
## and date (YYYY-MM-DD) between 1800-01-01 and 2099-12-31

## this functions determines the name of the year column (in YYYY format)
getYearColumnName <- function(df) {
  firstRow <- df[1, ]
  yearPtrn <- '^(18|19|20)[0-9]{2}$'
  potentialYearCol <- colnames(df)[grepl(yearPtrn, firstRow)]
  yearCol <- c()
  for (col in potentialYearCol) {
    if (all(grepl(yearPtrn, df[[col]]))) {
      yearCol <- c(yearCol, col)
    }
  }
  return(yearCol)
}


## this function determines the name of the month column (in YYYY-MM format)
getMonthColumnName <- function(df) {
  firstRow <- df[1, ]
  yearMonthPtrn <- '^(18|19|20)[0-9]{2}[- /.](0[1-9]|1[012])$'
  potentialYearMonthCol <- colnames(df)[grepl(yearMonthPtrn, firstRow)]
  yearMonthCol <- c()
  for (col in potentialYearMonthCol) {
    if (all(grepl(yearMonthPtrn, df[[col]]))) {
      yearMonthCol <- c(yearMonthCol, col)
    }
  }
  return(yearMonthCol)
}


## this function determines the name of the day column (in YYYY-MM-DD format)
getDateColumnName <- function(df) {
  firstRow <- as.character(df[1, ])
  datePtrn <- '^(18|19|20)[0-9]{2}[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$'
  potentialDateCol <- colnames(df)[grepl(datePtrn, firstRow)]
  dateCol <- c()  
  for (col in potentialDateCol) {
    if (all(grepl(datePtrn, as.character(df[[col]])))) {
      dateCol <- c(dateCol, col)
    }
  }
  return(dateCol)
}


## this function grabs the names of factor variables
getFactorVarNames <- function(df) {
  factorVars <- colnames(df)[sapply(df, is.factor)]
  return(factorVars)
}


## this function grabs the names of numeric variables
getNumericVarNames <- function(df) {
  numericVars <- colnames(df)[sapply(df, is.numeric)]
  return(numericVars)
}


## this function grabs the names variables of whose number of unique values does not exceed 
## a specified threshold (LOE: less than or equal to)
getVarNamesUniqValsCntLOEN <- function(df, n=100) {
  vars <- colnames(df)[sapply(df, function(x) {length(unique(x)) <= n})]
  return(vars)
}


## this function gets all variable names of data frame objects that are loaded into memory
getLoadedDataFrameNames <- function(env=.GlobalEnv) {
  objNames <- ls(env)
  dfNames <- c()
  for (objName in objNames) {
    obj <- get(objName)
    if(class(obj)=='data.frame') {
      dfNames <- c(dfNames, objName)
    } 
  }
  return(dfNames)
}


## this function modifies and ensures proper variable name
## for semi-automatic aggregation dataset column names
ensureProperVarName <- function(colnames, var, y) {  
  if (tolower(var) %in% c('none', '.')) return(var)
  origVar <- var
  
  ## only if original variable is not found in dataset's column names
  if (!(var %in% colnames)) {
    
    ## this step will do the following: e.g. v=='mpg' to y=='mpg_mean'
    var <- colnames[grepl(var, colnames)]

    ## if no match found
    if (length(var)==0L) {
      if (('count' %in% colnames))
        var <- 'count'
      else 
        var <- origVar      
    }
  }
  return(var)
}


## function to convert 'None' to NULL
convertNoneToNULL <- function(var) {
  if (tolower(var)=='none') {var <- NULL}; return(var)
}


## function to check if specified widgets are loaded on shiny UI
checkWidgetsLoaded <- function(input, widgets) {
  for (widget in widgets) {
    if (is.null(input[[widget]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}


## function for cleaning (removing duplicates or "None" values, etc.)
cleanPlotAggBy <- function(x, y, aggBy) {
  aggBy <- c(x, aggBy)
  aggBy <- unique(aggBy)
  nonAggBy <- c('None', 'none', '.')
  aggBy <- setdiff(aggBy, nonAggBy)
  
  if (x != y)
    aggBy <- setdiff(aggBy, y)
  
  return(aggBy)
}

