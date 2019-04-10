.ldSummaryContinuousTableMain <- function(jaspResults, variable, options) {
  summaryTable <- createJaspTable(title = "Summary Statistics")
  
  summaryTable$dependOnOptions(c("variable"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "variable",   title = "Variable",       type = "string", combine = TRUE)
  summaryTable$addColumnInfo(name = "modus",      title = "Modus",          type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "mean",       title = "Mean",           type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "var",        title = "Variance",       type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "sd",         title = "Std. deviation", type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "min",        title = "Minimum",        type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile25", title = "25% quantile",   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "median",     title = "Median",         type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile75", title = "75% quantile",   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "max",        title = "Maximum",        type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "skew",       title = "Skewness",       type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "kurt",       title = "Kurtosis",       type = "number", format = "sf:4")
  
  summaryTable$setExpectedRows(1)
  
  jaspResults[['dataContainer']][['summary']] <- summaryTable
  
  if(options[['variable']] == "")
    return()
  
  .ldFillSummaryContinuousTableMain(summaryTable, variable, options)
  
  return()
}

.ldFillSummaryContinuousTableMain <- function(summaryTable, variable, options){
  
  summaryTable$addRows(list(variable = options[['variable']],
                       modus    = mean(variable),
                       mean     = mean(variable),
                       var      = var(variable),
                       sd       = sd(variable),
                       min      = min(variable),
                       quantile25 = quantile(variable, 0.25),
                       median   = median(variable),
                       quantile75 = quantile(variable, 0.75),
                       max      = max(variable),
                       skew     = 0,
                       kurt     = 0))
  
  return()
}