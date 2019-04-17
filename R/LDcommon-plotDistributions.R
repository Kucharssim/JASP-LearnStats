.ldPlotPDF <- function(jaspResults, options){
  pdfPlot <- createJaspPlot(title = "", width = 600, height = 320)
  
  pdfPlot$dependOn(c("sd", "mu", "range"))
  
  jaspResults[['pdfContainer']][['pdfPlot']] <- pdfPlot
  
  .ldGeomLayersPDF(jaspResults, options)
  .ldFillPlotPDF(pdfPlot, jaspResults, options)
  
  return()
}

.ldGeomLayersPDF <- function(jaspResults, options){

  # create state for the basic curve
  if(is.null(jaspResults[['pdfContainer']][['curve']])){
    curve <- createJaspState()
    jaspResults[['pdfContainer']][['curve']] <- curve
    curve$dependOn(c("pars", "range"))

    plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], size = 1)

    curve$object <- plot
  }

  # create state for highlight density
  if(is.null(jaspResults[['pdfContainer']][['highlightDensity']])){
    highlightDensity <- createJaspState()
    jaspResults[['pdfContainer']][['highlightDensity']] <- highlightDensity
    highlightDensity$dependOn(c("pars", 'highlightmin', 'highlightmax', "highlightType"))

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
    highlightProbability$dependOn(c("pars", 'highlightmin', 'highlightmax',  "highlightType"))

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
    if(cdfValueRound %in% c(0, 1)){
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

  return()
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

  return()
}

.ldPlotCDF <- function(jaspResults, options){
  cdfPlot <- createJaspPlot(title = "", width = 600, height = 320)
  
  cdfPlot$dependOn(c("sd", "mu", "range"))
  
  jaspResults[['cdfContainer']][['cdfPlot']] <- cdfPlot
  
  .ldGeomLayersCDF(jaspResults, options)
  .ldFillPlotCDF(cdfPlot, jaspResults, options)
  
  return()
}

.ldGeomLayersCDF <- function(jaspResults, options){
  # create state for the basic curve
  if(is.null(jaspResults[['cdfContainer']][['curve']])){
    curve <- createJaspState()
    jaspResults[['cdfContainer']][['curve']] <- curve
    curve$dependOn(c("pars", "range"))
    
    plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], size = 1)
    
    curve$object <- plot
  }
  
  # create state for highlight density
  if(is.null(jaspResults[['cdfContainer']][['highlightDensity']])){
    highlightDensity <- createJaspState()
    jaspResults[['cdfContainer']][['highlightDensity']] <- highlightDensity
    highlightDensity$dependOn(c("pars", 'highlightmin', 'highlightmax', "highlightType"))
    
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
  if(is.null(jaspResults[['cdfContainer']][['highlightProbability']])){
    highlightProbability <- createJaspState()
    jaspResults[['cdfContainer']][['highlightProbability']] <- highlightProbability
    highlightProbability$dependOn(c("pars", 'highlightmin', 'highlightmax',  "highlightType"))
    
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
    if(cdfValueRound %in% c(0, 1)){
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
  
  return()
}

.ldFillPlotCDF <- function(cdfPlot, jaspResults, options){
  pp <- jaspResults[['cdfContainer']][['curve']]$object
  
  if(options[['highlightProbability']]){
    pp <- pp + jaspResults[['cdfContainer']][['highlightProbability']]$object
  }
  
  if(options[['highlightDensity']]){
    pp <- pp + jaspResults[['cdfContainer']][['highlightDensity']]$object
  }
  
  
  pp <- pp + ggplot2::ylab("Probability (X<x)") +
    ggplot2::scale_x_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::axesBreaks(options[['range_x']]))
  
  pp <- JASPgraphs::themeJasp(pp)
  cdfPlot[['plotObject']] <- pp
  
  return()
}

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