.ldFillPlotDistribution <- function(plot, options, fun){
  p <- ggplot2::ggplot(data = data.frame(x = c(-options[['range']], options[['range']])), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = fun, n = 101, args = options[['pars']], size = 1)  +
    ggplot2::ylab("Density")
  
  p <- JASPgraphs::themeJasp(p)
  
  plot[['plotObject']] <- p
  
  return()
}

.ldPlotHistogram <- function(jaspResults, options, variable, range){
  histPlot <- createJaspPlot(title = "Histogram", width = 600, height = 320)
  
  #histPlot$dependsOnOptions(c("variable"))
  
  jaspResults[['dataContainer']][['histogram']] <- histPlot
  
  .ldFillPlotHistogram(histPlot, options, variable, range)
  
  return()
}

.ldFillPlotHistogram <- function(plot, options, variable, range){
  #breaksCustom <- seq(range[1], range[2], length.out = options[['histogramBins']]+1)
  breaksCustom <- seq(range[1], range[2], length.out = options[['histogramBins']]+1)
  #p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = cut(variable, breaksCustom))) +
  #  ggplot2::geom_histogram(stat = 'count') +
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::stat(..width..*..density..)),
                            breaks = breaksCustom, fill = "steelblue") +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range) + 
    ggplot2::xlab("") +
    ggplot2::ylab(paste0("Frequency(", options[['variable']], " in bin)"))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  
  return()
}

.ldPlotECDF <- function(jaspResults, options, variable, range){
  ecdfPlot <- createJaspPlot(title = "Empirical cumulative distribution", width = 600, height = 320)
  
  #histPlot$dependsOnOptions(c("variable"))
  
  jaspResults[['dataContainer']][['ecdf']] <- ecdfPlot
  
  .ldFillPlotECDF(ecdfPlot, options, variable, range)
  
  return()
}

.ldFillPlotECDF <- function(plot, options, variable, range){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range) +
    ggplot2::xlab("x") +
    ggplot2::ylab(paste0("Frequency(", options[['variable']], "< x)"))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  
  return()
}