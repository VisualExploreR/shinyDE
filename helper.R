
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

