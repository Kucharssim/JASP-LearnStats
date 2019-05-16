.ldPlotContinuousDistributionFunctions <- function(jaspResults, options, pdfFormula){
  
  if(is.null(jaspResults[['pdfContainer']])){
    jaspResults[['pdfContainer']] <- createJaspContainer(title = "Probability Density Function")
    jaspResults[['pdfContainer']]$position <- 3 
    jaspResults[['pdfContainer']]$dependOn(c(options[['parValNames']], "parametrization"))
  }
  .ldExplanationPDF(jaspResults, options)
  .ldPlotPDF(jaspResults, options)
    

  if(is.null(jaspResults[['pdfContainer']][['formula']]))
    pdfFormula(jaspResults, options)

  if(is.null(jaspResults[['cdfContainer']])){
    jaspResults[['cdfContainer']] <- createJaspContainer(title = "Cumulative Distribution Function")
    jaspResults[['cdfContainer']]$position <- 4
    jaspResults[['cdfContainer']]$dependOn(c(options[['parValNames']], "parametrization"))
  }
  
  .ldExplanationCDF(jaspResults, options)
  .ldPlotCDF(jaspResults, options)
    
  
  # if(is.null(jaspResults[['cdfContainer']][['formula']]) && options$formulaCDF && options$plotCDF){
  #   .ldFormulaGaussianCDF(jaspResults, options)
  # }

  if(is.null(jaspResults[['qfContainer']])){
    jaspResults[['qfContainer']] <- createJaspContainer(title = "Quantile Function")
    jaspResults[['qfContainer']]$position <- 5
    jaspResults[['qfContainer']]$dependOn(c("parametrization"))
  }
  
  .ldExplanationQF(jaspResults, options)
  .ldPlotQF(jaspResults, options)

}


### Plot PDF ----
.ldExplanationPDF <- function(jaspResults, options){
  if(!options$explanatoryText) return()
  if(!options$plotPDF) return()
  if(!is.null(jaspResults[['pdfContainer']][['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotPDF", "explanatoryText"))
  explanation$position <- 1
  text <- "The probability density function (PDF), usually denoted as f(x), is a function of a random variable X. The value of f(x) provides the relative likelihood that a realization of the random variable X yields a value equal to x.
  The density plot displays the probability density function of a random variable. The y-axis displays the value of the density function for a particular value of the random variable (displayed on the x-axis)."
  explanation[['text']] <- text
  jaspResults[['pdfContainer']][['explanation']] <- explanation
  

}

.ldPlotPDF <- function(jaspResults, options){
  if(!is.null(jaspResults[['pdfContainer']][['pdfPlot']])) return()
  if(!options$plotPDF) return()
  
  
  pdfPlot <- createJaspPlot(title = "Density Plot", width = 600, height = 320)
  pdfPlot$dependOn(c('plotPDF', 'range', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min', options[['parValNames']]))
  jaspResults[['pdfContainer']][['pdfPlot']] <- pdfPlot
  
  .ldGeomLayersPDF(jaspResults, options)
  .ldFillPlotPDF(pdfPlot, jaspResults, options)
  
}

.ldGeomLayersPDF <- function(jaspResults, options){
  # create state for the basic curve
  if(is.null(jaspResults[['pdfContainer']][['curve']])){
    curve <- createJaspState()
    jaspResults[['pdfContainer']][['curve']] <- curve
    curve$dependOn(c(options[['parValNames']], "range"))

    plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], size = 1)

    curve$object <- plot
  }

  # create state for highlight density
  if(is.null(jaspResults[['pdfContainer']][['highlightDensity']])){
    highlightDensity <- createJaspState()
    jaspResults[['pdfContainer']][['highlightDensity']] <- highlightDensity
    highlightDensity$dependOn(c(options[['parValNames']], 'highlightType', 'min', 'max', 'lower_max', 'upper_min'))

    # determine plotting region
    args <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['x']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['x']] <- options[['highlightmax']]
    } else if(options[['highlightType']] == "upper"){
      args[['x']] <- options[['highlightmin']]
    }
    
    
    pdfValue <- do.call(options[['pdfFun']], args)

    segment_data <- data.frame(x = options[['range_x']][1] + (options[['range_x']][2]-options[['range_x']][1])/15,
                               xend = args[['x']], y = pdfValue)
    layers <- list()
    layers[[1]] <- ggplot2::geom_segment(data = segment_data,
                                         mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = y), linetype = 2)
    layers[[2]] <- ggplot2::geom_text(data = data.frame(x = options[['range_x']][1], y = pdfValue, label = round(pdfValue, 2)),
                                      ggplot2::aes(x = x, y = y, label = label), size = 6)
    layers[[3]] <- ggplot2::geom_linerange(x = args[['x']], ymin = 0, ymax = pdfValue, linetype = 2)
    layers[[4]] <- JASPgraphs::geom_point(x = args[['x']], y = pdfValue)
    
    
    highlightDensity$object <- layers
  }

  # create state for highlight probability
  if(is.null(jaspResults[['pdfContainer']][['highlightProbability']])){
    highlightProbability <- createJaspState()
    jaspResults[['pdfContainer']][['highlightProbability']] <- highlightProbability
    highlightProbability$dependOn(c(options[['parValNames']], 'highlightType', 'min', 'max', 'lower_max', 'upper_min'))

    # determine plotting region
    args <- options[['pars']]
    argsPDF <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['q']] <- c(-Inf, options[['highlightmax']])
    } else if(options[['highlightType']] == "upper"){
      args[['q']] <- c(options[['highlightmin']], Inf)
    }
    
    # calculate value under the curve
    cdfValue <- do.call(options[['cdfFun']], args)
    cdfValue <- cdfValue[2] - cdfValue[1]
    
    # round value under the curve for plotting
    cdfValueRound <- round(cdfValue, 2)
    if(c(0, 1) %in% cdfValueRound){
      cdfValueRound <- round(cdfValue, 3)
    }
    
    # calculate position of the geom_text
    args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    argsPDF[['x']] <- seq(args[['q']][1], args[['q']][2], length.out = 20)
    x <- weighted.mean(argsPDF[['x']], do.call(options[['pdfFun']], argsPDF))
    argsPDF[['x']] <- x
    y <- do.call(options[['pdfFun']], argsPDF)/3
    
    layers <- list()
    layers[[1]] <- ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], geom = "area",
                                          xlim = args[['q']], fill = "steelblue")
    layers[[2]] <- ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                                      mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
    highlightProbability$object <- layers
  }


}

.ldFillPlotPDF <- function(pdfPlot, jaspResults, options){
  pp <- jaspResults[['pdfContainer']][['curve']]$object

  if(options[['highlightProbability']]){
    pp <- pp + jaspResults[['pdfContainer']][['highlightProbability']]$object
  }
  
  if(options[['highlightDensity']]){
    pp <- pp + jaspResults[['pdfContainer']][['highlightDensity']]$object
  }
  
  
  pp <- pp + ggplot2::ylab("Density") + ggplot2::scale_x_continuous(limits = options[['range_x']], 
                                                                    breaks = JASPgraphs::axesBreaks(options[['range_x']]))
  
  pp <- JASPgraphs::themeJasp(pp)
  pdfPlot[['plotObject']] <- pp


}

### Plot CDF ----
.ldExplanationCDF <- function(jaspResults, options){
  if(!options$explanatoryText) return()
  if(!options$plotCDF) return()
  if(!is.null(jaspResults[['cdfContainer']][['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotCDF", "explanatoryText"))
  explanation$position <- 1
  text <- "The cumulative distribution function (CDF), usually denoted as F(x), is a function of a random variable X. The value of F(x) provides the probability that a realization of the random variable X yields a value that is equal to or smaller than x.
  The cumulative probability plot displays the cumulative distribution of a random variable. The y-axis displays the value of the cumulative distribution function for a particular value of the random variable (displayed on the x-axis).
  
  "
  explanation[['text']] <- text
  jaspResults[['cdfContainer']][['explanation']] <- explanation
  

}

.ldPlotCDF <- function(jaspResults, options){
  if(!is.null(jaspResults[['cdfContainer']][['cdfPlot']])) return()
  if(!options$plotCDF) return()
  
  cdfPlot <- createJaspPlot(title = "Cumulative Probability Plot", width = 600, height = 320)
  
  cdfPlot$dependOn(c('plotCDF', 'range', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min', options[['parValNames']]))
  
  jaspResults[['cdfContainer']][['cdfPlot']] <- cdfPlot
  
  .ldGeomLayersCDF(jaspResults, options)
  .ldFillPlotCDF(cdfPlot, jaspResults, options)
  

}

.ldGeomLayersCDF <- function(jaspResults, options){
  # create state for the basic curve
  if(is.null(jaspResults[['cdfContainer']][['curve']])){
    curve <- createJaspState()
    jaspResults[['cdfContainer']][['curve']] <- curve
    curve$dependOn(c(options[['parValNames']], "range"))
    
    plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = options[['cdfFun']], n = 101, args = options[['pars']], size = 1)
    
    curve$object <- plot
  }
  
  # create state for highlight density
  if(is.null(jaspResults[['cdfContainer']][['highlightDensity']])){
    highlightDensity <- createJaspState()
    jaspResults[['cdfContainer']][['highlightDensity']] <- highlightDensity
    highlightDensity$dependOn(c(options[['parValNames']], 'highlightType', 'min', 'max', 'lower_max', 'upper_min'))
    
    # determine plotting region
    args <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['q']] <- options[['highlightmax']]
    } else if(options[['highlightType']] == "upper"){
      args[['q']] <- options[['highlightmin']]
    }
    pdfArgs <- args
    pdfArgs[['x']] <- pdfArgs[['q']]
    pdfArgs[['q']] <- NULL
    
    pdfValue <- do.call(options[['pdfFun']], pdfArgs)
    cdfValue <- do.call(options[['cdfFun']], args)
    intercept <- cdfValue - args[['q']]*pdfValue
    slopeText <-  round(pdfValue, 2) #bquote(paste(beta, " = ", .(round(slope, 2))))
    
    line_data <- data.frame(slope = pdfValue, intercept = intercept)
    layers <- list()
    layers[[1]] <- ggplot2::geom_abline(slope = pdfValue, intercept = intercept, linetype = 1)
    layers[[2]] <- ggplot2::geom_text(data = data.frame(x = args[['q']], y = 1.1*cdfValue, label = round(pdfValue, 2)),
                                      ggplot2::aes(x = x, y = y, label = label), size = 6)
    
    highlightDensity$object <- layers
  }
  
  # create state for highlight probability
  if(is.null(jaspResults[['cdfContainer']][['highlightProbability']])){
    highlightProbability <- createJaspState()
    jaspResults[['cdfContainer']][['highlightProbability']] <- highlightProbability
    highlightProbability$dependOn(c(options[['parValNames']], 'highlightType', 'min', 'max', 'lower_max', 'upper_min'))
    
    # determine plotting region
    args <- options[['pars']]
    # argsPDF <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['q']] <- c(options[['highlightmax']])
    } else if(options[['highlightType']] == "upper"){
      args[['q']] <- c(options[['highlightmin']])
    }
    
    # calculate value under the curve
    cdfValue <- do.call(options[['cdfFun']], args)
    
    # round value under the curve for plotting
     cdfValueRound <- round(cdfValue, 2)
    # if(cdfValueRound %in% c(0, 1)){
    #   cdfValueRound <- round(cdfValue, 3)
    # }
    
    segment_data <- data.frame(xoffset = options[['range_x']][1] + (options[['range_x']][2]-options[['range_x']][1])/15,
                               x = options[['range_x']][1], xend = args[['q']], y = cdfValue, label = cdfValueRound)
    layers <- list()
    layers[[1]] <- ggplot2::geom_segment(data = segment_data,
                                         mapping = ggplot2::aes(x = xoffset, xend = xend, y = y, yend = y), linetype = 2)
    layers[[2]] <- ggplot2::geom_text(data = segment_data,
                                      ggplot2::aes(x = x, y = y, label = label), size = 6)
    layers[[3]] <- ggplot2::geom_linerange(x = args[['q']], ymin = 0, ymax = cdfValue, linetype = 2)
    layers[[4]] <- JASPgraphs::geom_point(x = args[['q']], y = cdfValue)

    highlightProbability$object <- layers
  }
  

}

.ldFillPlotCDF <- function(cdfPlot, jaspResults, options){
  pp <- jaspResults[['cdfContainer']][['curve']]$object
  
  if(options[['highlightDensity']]){
    pp <- pp + jaspResults[['cdfContainer']][['highlightDensity']]$object
  }
  
  if(options[['highlightProbability']]){
    pp <- pp + jaspResults[['cdfContainer']][['highlightProbability']]$object
  }
  
  
  
  pp <- pp + ggplot2::ylab("Probability (X \u2264 x)") +
    ggplot2::scale_x_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::axesBreaks(options[['range_x']])) +
    ggplot2::scale_y_continuous(limits = c(0, 1))
  
  pp <- JASPgraphs::themeJasp(pp)
  cdfPlot[['plotObject']] <- pp
  

}

### Plot QF ----
.ldExplanationQF <- function(jaspResults, options){
  if(!options$explanatoryText) return()
  if(!options$plotQF) return()
  if(!is.null(jaspResults[['qfContainer']][['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotQF", "explanatoryText"))
  explanation$position <- 1
  text <- "The quantile function, usually denoted as Q(p), is the inverse of the cumulative distribution function.
  The function gives the quantile such that the probability of the random variable being less than or equal to that value equals the given probability p.   
  The quantile plot displays the quantile function. The y-axis displays the quantile of which the probability that the random variable is less or equal to that value is equal to p (plotted on the x-axis).
  
  "
  explanation[['text']] <- text
  jaspResults[['qfContainer']][['explanation']] <- explanation
  
}

.ldPlotQF <- function(jaspResults, options){
  if(!is.null(jaspResults[['qfContainer']][['qfPlot']])) return()
  if(!options$plotQF) return()
  
  qfPlot <- createJaspPlot(title = "Quantile Plot", width = 600, height = 320)
  
  qfPlot$dependOn(c('plotQF', 'range', options[['parValNames']]))
  
  jaspResults[['qfContainer']][['qfPlot']] <- qfPlot
  
  .ldFillPlotQF(qfPlot, options)

}

.ldFillPlotQF <- function(qfPlot, options){
  args <- options[['pars']]
  args[['q']] <- options[['range_x']]
  prange <- do.call(options[['cdfFun']], args)
  args[['q']] <- NULL
  
  plot <- ggplot2::ggplot(data = data.frame(x = prange), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['qFun']], n = 151, args = args, size = 1)  +
    ggplot2::ylab("x") + ggplot2::xlab("Probability(X \u2264 x)") +
    ggplot2::scale_x_continuous(limits = 0:1) +
    ggplot2::scale_y_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::axesBreaks(options[['range_x']]))
  
  
  plot <- JASPgraphs::themeJasp(plot)
  
  qfPlot[['plotObject']] <- plot
  

}


# .ldFillPlotDistribution <- function(plot, options, fun){
#   p <- ggplot2::ggplot(data = data.frame(x = c(-options[['range']], options[['range']])), ggplot2::aes(x = x)) +
#     ggplot2::stat_function(fun = fun, n = 101, args = options[['pars']], size = 1)  +
#     ggplot2::ylab("Density")
#   
#   p <- JASPgraphs::themeJasp(p)
# 
#   plot[['plotObject']] <- p
# 
#   return()
# }

#### Plot empirical ----
.ldPlotHistogram <- function(jaspResults, options, variable, ready){
  if(!options$histogram) return()
  if(!is.null(jaspResults[['dataContainer']][['histogram']])) return()
  
  histPlot <- createJaspPlot(title = "Histogram", width = 600, height = 320)
  
  histPlot$dependOn(c("variable", "histogramBins", "histogram", "simulateNow"))
  histPlot$position <- 2
  
  
  jaspResults[['dataContainer']][['histogram']] <- histPlot
  
  if(!ready) return()
  
  .ldFillPlotHistogram(histPlot, options, variable)
  
}

.ldFillPlotHistogram <- function(plot, options, variable){
  range <- options[['rangeVariable']]
  
  breaksCustom <- seq(range[1], range[2], length.out = options[['histogramBins']]+1)

  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::stat(..width..*..density..)),
                            breaks = breaksCustom, fill = "steelblue") +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range) + 
    ggplot2::xlab("") +
    ggplot2::ylab(paste0("Freq(", options[['variable']], " in bin)"))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  

}

.ldPlotECDF <- function(jaspResults, options, variable, ready){
  if(!options[['ecdf']]) return()
  if(!is.null(jaspResults[['dataContainer']][['ecdf']])) return()
  
  
  ecdfPlot <- createJaspPlot(title = "Empirical Cumulative Distribution", width = 600, height = 320)
  
  ecdfPlot$dependOn(c("variable", "ecdf", "simulateNow"))
  ecdfPlot$position <- 3
  
  jaspResults[['dataContainer']][['ecdf']] <- ecdfPlot
  
  if(!ready) return()
  
  .ldFillPlotECDF(ecdfPlot, options, variable)
  

}

.ldFillPlotECDF <- function(plot, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = options[['rangeVariable']]) +
    ggplot2::xlab("x") +
    ggplot2::ylab(paste0("Freq(", options[['variable']], " \u2264 x)"))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  

}