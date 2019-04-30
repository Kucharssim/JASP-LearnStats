.ldFillQQPlot <- function(qqplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(sample = variable)) +
    ggplot2::stat_qq(distribution = options[['qFun']], dparams = estParameters) +
    ggplot2::stat_qq_line(distribution = options[['qFun']], dparams = estParameters)
  
  p <- JASPgraphs::themeJasp(p)
  
  qqplot$plotObject <- p
  
  return()
}

.ldFillEstPDFPlot <- function(pdfplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), fill = "steelblue") +
    ggplot2::stat_function(fun = options[['pdfFun']], args = as.list(estParameters), size = 1.5) + 
    ggplot2::scale_x_continuous(limits = options[['rangeVariable']]) +
    ggplot2::ylab("Density") + ggplot2::xlab(options[['variable']])
  
  p <- JASPgraphs::themeJasp(p)
  
  pdfplot$plotObject <- p
  
  return()
}

.ldFillPPPlot <- function(ppplot, estParameters, options, variable){
  n <- length(variable)
  ObservedProp <- (1:n)/n - 0.5/n
  
  args <- as.list(estParameters)
  args[['q']] <- variable
  TheoreticalProp <- sort(do.call(options[['cdfFun']], args))
  
  p <- ggplot2::ggplot(data = NULL) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    #JASPgraphs::geom_point(x = TheoreticalProp, y = ObservedProp) +
    ggplot2::geom_point(ggplot2::aes(x = TheoreticalProp, y = ObservedProp)) +
    ggplot2::xlab("Theoretical") + ggplot2::ylab("Sample") +
    ggplot2::scale_x_continuous(limits = 0:1) + ggplot2::scale_y_continuous(limits = 0:1)
  
  p <- JASPgraphs::themeJasp(p)
  
  ppplot$plotObject <- p
  
  return()
}

.ldFillEstCDFPlot <- function(cdfplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_rug() +
    ggplot2::stat_function(fun = options[['cdfFun']], args = as.list(estParameters), size = 1.5) + 
    ggplot2::scale_x_continuous(limits = options[['rangeVariable']]) +
    ggplot2::scale_y_continuous(limits = 0:1) + 
    ggplot2::ylab("Probability (X \u2264 x)") + ggplot2::xlab(options[['variable']])
  
  p <- JASPgraphs::themeJasp(p)
  
  cdfplot$plotObject <- p
  
  return()
}