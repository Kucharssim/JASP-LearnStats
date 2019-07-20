#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.ldGetPlotContainer <- function(jaspResults, options, name, title, position){
  if(!is.null(jaspResults[[name]])){
    plotsContainer <- jaspResults[[name]]
  } else{
    plotsContainer <- createJaspContainer(title = title)
    plotsContainer$position <- position
    
    if("parametrization" %in% names(options)){
      plotsContainer$dependOn(c(options$parValNames, "parametrization"))
    } else{
      plotsContainer$dependOn(c(options$parValNames))
    }
    
    jaspResults[[name]] <- plotsContainer
  }
  
  return(plotsContainer)
}

.ldFormulaPlot <- function(container, options, formulaText = NULL, depend = NULL){
  if(!options$formulas) return()
  if(!is.null(container[['formula']])) return()  
  
  formula <- createJaspHtml(title = "Formula", elementType = "h1")
  formula$position <- 3
  formula$dependOn(c("formulas", depend))
  
  if(is.function(formulaText)){
    formula[['text']] <- formulaText(options)
  } else if(is.character(formulaText)){
    formula[['text']] <- formulaText
  }
  
  container[['formula']] <- formula
}

### Plot PDF ----
.ldFillPDFContainer <- function(pdfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotPDF) return()
  
  .ldExplanationPDF(pdfContainer, options, explanationText)
  .ldPlotPDF(pdfContainer, options)
  .ldFormulaPlot(pdfContainer, options, formulaText, "plotPDF")
  
  return()
}

.ldExplanationPDF <- function(pdfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(pdfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotPDF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList$explanations$pdf
  }
  
  explanation[['text']] <- explanationText
  pdfContainer[['explanation']] <- explanation

}

.ldPlotPDF <- function(pdfContainer, options){
  if(!is.null(pdfContainer[['pdfPlot']])) return()
  
  pdfPlot <- createJaspPlot(title = "Density Plot", width = 600, height = 320)
  pdfPlot$position <- 2 # after explanation, before formula
  pdfPlot$dependOn(c('plotPDF', 'range', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  pdfContainer[['pdfPlot']] <- pdfPlot
  
  .ldFillPlotPDF(pdfPlot, options)

  return()
}

.ldFillPlotPDF <- function(pdfPlot, options){
  # basic density curve
  plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], size = 1)
  
  # highlight probability
  if(options$highlightProbability){
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
    
    plot <- plot + 
      ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], geom = "area", 
                             xlim = args[['q']], fill = "steelblue") +
      ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                         mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
  }
  
  # highlight density
  if(options$highlightDensity){
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
    
    # plot density
    plot <- plot + 
      ggplot2::geom_segment(data = segment_data, 
                            mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = data.frame(x = options[['range_x']][1], y = pdfValue, label = round(pdfValue, 2)),
                         ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = args[['x']], ymin = 0, ymax = pdfValue, linetype = 2) +
      JASPgraphs::geom_point(x = args[['x']], y = pdfValue)
  }
  
  plot <- plot + ggplot2::ylab("Density") + 
    ggplot2::scale_x_continuous(limits = options[['range_x']], breaks = JASPgraphs::axesBreaks(options[['range_x']]))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  pdfPlot[['plotObject']] <- plot
}

### Plot CDF ----
.ldFillCDFContainer <- function(cdfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotCDF) return()
  
  .ldExplanationCDF(cdfContainer, options, explanationText)
  .ldPlotCDF(cdfContainer, options)
  .ldFormulaPlot(cdfContainer, options, formulaText, "plotCDF")
}

.ldExplanationCDF <- function(cdfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(cdfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotCDF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  cdfContainer[['explanation']] <- explanation
}

.ldPlotCDF <- function(cdfContainer, options){
  if(!is.null(cdfContainer[['cdfPlot']])) return()
  
  cdfPlot <- createJaspPlot(title = "Cumulative Probability Plot", width = 600, height = 320)
  cdfPlot$position <- 2 # after explanation, before formula
  cdfPlot$dependOn(c('plotCDF', 'range', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  cdfContainer[['cdfPlot']] <- cdfPlot
  
  .ldFillPlotCDF(cdfPlot, options)
  
  return()
}

.ldFillPlotCDF <- function(cdfPlot, options){
  
  plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['cdfFun']], n = 101, args = options[['pars']], size = 1)
  

  if(options$highlightDensity){
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

    plot <- plot + 
      ggplot2::geom_abline(slope = pdfValue, intercept = intercept, linetype = 1) +
      ggplot2::geom_text(data = data.frame(x = args[['q']], y = 1.1*cdfValue, label = round(pdfValue, 2)),
                         ggplot2::aes(x = x, y = y, label = label), size = 6)
  }
  
  if(options$highlightProbability){
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
    
    
    plot <- plot + 
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xoffset, xend = xend, y = y, yend = y), linetype = 2) +
      ggplot2::geom_text(data = segment_data,
                         ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = args[['q']], ymin = 0, ymax = cdfValue, linetype = 2) + 
      JASPgraphs::geom_point(x = args[['q']], y = cdfValue)
  }
  
  plot <- plot + 
    ggplot2::ylab("Probability (X \u2264 x)") +
    ggplot2::scale_x_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::axesBreaks(options[['range_x']])) +
    ggplot2::scale_y_continuous(limits = c(0, 1))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  cdfPlot[['plotObject']] <- plot
}

### Plot QF ----
.ldFillQFContainer <- function(qfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotQF) return()
  
  .ldExplanationQF(qfContainer, options, explanationText)
  .ldPlotQF(qfContainer, options)
  .ldFormulaPlot(qfContainer, options, formulaText, "plotQF")
}

.ldExplanationQF <- function(qfContainer, options, explanationText){
  if(!options$explanatoryText) return()
  if(!is.null(qfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotQF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  qfContainer[['explanation']] <- explanation
}

.ldPlotQF <- function(qfContainer, options){
  if(!is.null(qfContainer[['qfPlot']])) return()
  
  qfPlot <- createJaspPlot(title = "Quantile Plot", width = 600, height = 320)
  qfPlot$position <- 2 # after explanation, before formula
  qfPlot$dependOn(c('plotQF', 'range'))
  qfContainer[['qfPlot']] <- qfPlot
  
  .ldFillPlotQF(qfPlot, options)
  
  return()

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

#### Plot empirical ----
.ldPlotHistogram <- function(dataContainer, variable, options, ready){
  if(!options$histogram) return()
  if(!is.null(dataContainer[['histogram']])) return()
  
  histPlot <- createJaspPlot(title = "Histogram", width = 500, height = 320)
  
  histPlot$dependOn(c("histogramBins", "histogram"))
  histPlot$position <- 3
  
  dataContainer[['histogram']] <- histPlot
  
  if(!ready) return()
  
  .ldFillPlotHistogram(histPlot, options, variable)
  
}

.ldFillPlotHistogram <- function(histPlot, options, variable){
  range <- range(variable)*1.1
  
  breaksCustom <- seq(range[1], range[2], length.out = options[['histogramBins']]+1)

  plot <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::stat(..width..*..density..)),
                            breaks = breaksCustom, fill = "steelblue") +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range) + 
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(paste0("Freq(", options[['variable']], " in bin)"))
  
  plot <- JASPgraphs::themeJasp(plot)
  histPlot[['plotObject']] <- plot
}

.ldPlotECDF <- function(dataContainer, variable, options, ready){
  if(!options[['ecdf']]) return()
  if(!is.null(dataContainer[['ecdf']])) return()
  
  ecdfPlot <- createJaspPlot(title = "Empirical Cumulative Distribution", width = 500, height = 320)
  
  ecdfPlot$dependOn(c("ecdf"))
  ecdfPlot$position <- 4
  
  dataContainer[['ecdf']] <- ecdfPlot
  
  if(!ready) return()
  
  .ldFillPlotECDF(ecdfPlot, options, variable)
}

.ldFillPlotECDF <- function(plot, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step", size = 1.5) +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range(variable)*1.1) +
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(paste0("Freq(", options[['variable']], " \u2264 x)"))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  

}