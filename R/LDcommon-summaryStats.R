.ldSummaryContinuousTableMain <- function(jaspResults, variable, options) {
  summaryTable <- createJaspTable(title = "Summary Statistics")
  
  summaryTable$dependOn(c("variable"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "variable",   title = "Variable",       type = "string", combine = TRUE)
  summaryTable$addColumnInfo(name = "mean",       title = "Mean",           type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "var",        title = "Variance",       type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "sd",         title = "Std. deviation", type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "min",        title = "Minimum",        type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile25", title = "25% Quantile",   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "median",     title = "Median",         type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile75", title = "75% Quantile",   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "max",        title = "Maximum",        type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "skew",       title = "Skewness",       type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "kurt",       title = "Kurtosis",       type = "number", format = "sf:4")
  
  #summaryTable$setExpectedRows(1)
  
  jaspResults[['dataContainer']][['summary']] <- summaryTable
  
  if(options[['variable']] == "")
    return()
  
  .ldFillSummaryContinuousTableMain(summaryTable, variable, options)
  
  return()
}

.ldFillSummaryContinuousTableMain <- function(summaryTable, variable, options){
  
  summaryTable$addRows(list(variable = options[['variable']],
                       mean     = mean(variable),
                       var      = var(variable),
                       sd       = sd(variable),
                       min      = min(variable),
                       quantile25 = quantile(variable, 0.25),
                       median   = median(variable),
                       quantile75 = quantile(variable, 0.75),
                       max      = max(variable),
                       skew     = .summarySkewness(variable),
                       kurt     = .summaryKurtosis(variable)))
  
  return()
}

.summarySkewness <- function(x) {
  
  # Skewness function as in SPSS (for samlpes spaces):
  # http://suite101.com/article/skew-and-how-skewness-is-calculated-in-statistical-software-a231005
  
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  z <- (x - m) / s  # z scores
  a <- n / ((n - 1) * (n - 2))
  
  skewness <- sum(z^3) * a
  
  return(skewness)
}

.summaryKurtosis <- function(x) {
  
  # Kurtosis function as in SPSS:
  # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
  # http://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis
  
  n   <- length(x)
  s4  <- sum((x - mean(x))^4)
  s2  <- sum((x - mean(x))^2)
  v   <- s2 / (n-1)
  a   <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  b   <- s4 / (v^2)
  c   <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))
  
  kurtosis <- a * b + c
  
  return(kurtosis)
}


.computeObservedMoments <- function(x, max.moment = 2, about.mean = FALSE){
  n <- length(x)
  moments <- numeric(length = max.moment)
  moments[1] <- mean(x)
  
  if(max.moment < 2)
    return(moments)
  
  if(about.mean)
    x <- x-moments[1]
  
  
  for(i in 2:max.moment){
    moments[i] <- sum(x^i) / n
  }
  
  return(moments)
}
