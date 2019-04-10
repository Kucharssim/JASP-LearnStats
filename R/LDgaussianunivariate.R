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

# LDgaussianunivariate <- function(jaspResults, dataset, options, state=NULL){
# }

LDgaussianunivariate <- function(jaspResults, dataset, options, state=NULL){

  jaspResults$title <- "Normal distribution"
  
  if(options[['variable']] != ""){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    variable <- dataset[[.v(options[['variable']])]]
    
    rangeVariable <- range(variable)
    #rangeVariable[1] <- floor(rangeVariable[1])
    #rangeVariable[2] <- ceiling(rangeVariable[2])
  }
  
  options <- .recodeOptionsLDGaussianUnivariate(options)

  
  jaspResults[['pdfContainer']] <- createJaspContainer(title = "Probability Density Function")
  if(is.null(jaspResults[['pdfContainer']][['pdf']]) && options$plotPDF){
    .ldPlotGaussianPDF(jaspResults, options)
  }
  if(is.null(jaspResults[['pdfContainer']][['formula']]) && options$formulaPDF && options$plotPDF){
    .ldFormulaGaussianPDF(jaspResults, options)
  }
  
  jaspResults[['cdfContainer']] <- createJaspContainer(title = "Cumulative Distribution Function")
  if(is.null(jaspResults[['cdfContainer']][['cdf']]) && options$plotCDF){
    .ldPlotGaussianCDF(jaspResults, options)
  }
  if(is.null(jaspResults[['cdfContainer']][['formula']]) && options$formulaCDF && options$plotCDF){
    .ldFormulaGaussianCDF(jaspResults, options)
  }
  
  jaspResults[['qfContainer']] <- createJaspContainer(title = "Quantile Function")
  if(is.null(jaspResults[['qfContainer']][['qf']]) && options$plotQF){
    .ldPlotGaussianQF(jaspResults, options)
  }
  
  jaspResults[['dataContainer']] <- createJaspContainer(title = options[['variable']])
  
  if(is.null(jaspResults[['dataContainer']][["summary"]]) && options$summary){
    .ldSummaryContinuousTableMain(jaspResults, variable, options)
  }
      
  if(is.null(jaspResults[['dataContainer']][['histogram']]) && options$histogram){
    .ldPlotHistogram(jaspResults, options, variable, rangeVariable)
  }
  
  if(is.null(jaspResults[['dataContainer']][['ecdf']]) && options$ecdf){
    .ldPlotECDF(jaspResults, options, variable, rangeVariable)
  }
  
  return()
}

.recodeOptionsLDGaussianUnivariate <- function(options){
  if(options$parametrization == "sigma2"){
    options$sd <- sqrt(options$varValue)
  } else if(options$parametrization == "sigma"){
    options$sd <- options$varValue
  } else if(options$parametrization == "tau2"){
    options$sd <- sqrt(1/options$varValue)
  } else if(options$parametrization == "tau"){
    options$sd <- 1/options$varValue
  }
  
  options[['pars']] <- list(mean = options[['mu']], sd = options[['sd']])
  
  options[['pdfFun']] <- dnorm
  options[['cdfFun']] <- pnorm
  
  if(options[['highlightIntervalsType']] == "minmax"){
    options[['highlightmin']] <- options[['min']]
    options[['highlightmax']] <- options[['max']]
  } else if(options[['highlightIntervalsType']] == "lower"){
    options[['highlightmin']] <- -options[['range']]
    options[['highlightmax']] <- options[['lower_max']]
  } else if(options[['highlightIntervalsType']] == "upper"){
    options[['highlightmin']] <- options[['upper_min']]
    options[['highlightmax']] <- options[['range']]
  } else{
    options[['highlightmin']] <- options[['highlightmax']] <- NULL
  }
  
  options
}


.ldFormulaGaussianPDF <- function(jaspResults, options){
  pdfFormula <- createJaspHtml(title = "PDF formula", elementType = "h1")

  pdfFormula$dependOnOptions("parametrization")

  .ldFillFormulaGaussianPDF(pdfFormula, options)

  jaspResults[['pdfContainer']][['formula']] <- pdfFormula
  return()
}

.ldFillFormulaGaussianPDF <- function(pdfFormula, options){
  if(options[['parametrization']] == "sigma2"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>) = 
(2&pi;<span style='color:blue'>&sigma;&sup2;</span>)<sup>-&frac12;</sup> 
exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;&sup2;</span>]
    </MATH>"
  } else if(options[['parametrization']] == "sigma"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>) = 
    (2&pi;<span style='color:blue'>&sigma;</span>&sup2;)<sup>-&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;</span>&sup2;]
    </MATH>"
  } else if(options[['parametrization']] == "tau2"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>) = 
    (<span style='color:blue'>&tau;&sup2;</span> &frasl; 2&pi;)<sup>&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;&sup2;</span> &frasl; 2]
    </MATH>"
  } else if(options[['parametrization']] == "tau"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>) = 
    <span style='color:blue'>&tau;</span> &frasl; (2&pi;)<sup>&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;</span>&sup2; &frasl; 2]
    </MATH>"
  }

  pdfFormula[['text']] <- gsub(pattern = "\n", replacement = " ", x = text)

  return()
}

.ldPlotGaussianQF <- function(jaspResults, options){
  qfPlot <- createJaspPlot(title = "", width = 600, height = 320)
  
  qfPlot$dependOnOptions(c("sd", "mu", "range"))
  
  jaspResults[['qfContainer']][['qf']] <- qfPlot
  
  .ldFillPlotGaussianQF(qfPlot, options)
  
  return()
}

.ldFillPlotGaussianQF <- function(qfPlot, options){
  range <- c(-options[['range']], options[['range']])
  prange <- pnorm(range, options[['mu']], options[['sd']])
  
  plot <- ggplot2::ggplot(data = data.frame(x = prange), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = qnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), size = 1)  +
    ggplot2::ylab("x") + ggplot2::xlab("Probability(X<x)") +
    ggplot2::scale_x_continuous(limits = 0:1) +
    ggplot2::scale_y_continuous(limits = range)
  
  
  plot <- JASPgraphs::themeJasp(plot)
  
  qfPlot[['plotObject']] <- plot
  
  return()
}

.ldPlotGaussianPDF <- function(jaspResults, options){
  pdfPlot <- createJaspPlot(title = "", width = 600, height = 320)

  pdfPlot$dependOnOptions(c("sd", "mu", "range", 
                            "highlightIntervals", "highlightmin", "highlightmax"))

  jaspResults[['pdfContainer']][['pdf']] <- pdfPlot

  #.ldFillPlotGaussianPDF(pdfPlot, options)
  .ldFillPlotDistribution(pdfPlot, options, dnorm)
  return()
}

.ldFillPlotGaussianPDF <- function(pdfPlot, options){

  plot <- ggplot2::ggplot(data = data.frame(x = c(-options[['range']], options[['range']])), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = dnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), size = 1)  +
    ggplot2::ylab("Density")
  
  if(options[['highlightIntervals']]){
    plot <- plot +
      ggplot2::stat_function(fun = dnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), geom = "area",
                             xlim = c(options[['highlightmin']], options[['highlightmax']]), fill = "steelblue")
  }

  if(options[['highlightPoint']]){
    at <- options[['highlightPointAt']]
    value <- dnorm(at, options[['mu']], options[['sd']])
    area <- pnorm(at, options[['mu']], options[['sd']])
    
    if(options[['highlightPointTangent']]){
      plot <- plot +
        ggplot2::stat_function(fun = dnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), geom = "area",
                               xlim = c(-options[['range']], at), fill = "steelblue") + 
        ggplot2::geom_text(data = data.frame(x = at-1, y = value/2, label = round(area, 2)), ggplot2::aes(x = x, y = y, label = label), size = 10)
    }
    
    if(options[['highlightPointValue']]){
      plot <- plot +
        ggplot2::geom_segment(ggplot2::aes(x = -options[['range']]+options[['range']]/9, xend = at, y = value, yend = value), linetype = 2) +
        ggplot2::geom_text(data = data.frame(x = -options[['range']], y = value, label = round(value, 2)), ggplot2::aes(x = x, y = y, label = label), size = 6)
    }
    
    plot <- plot + 
      ggplot2::geom_linerange(x = at, ymin = 0, ymax = value, linetype = 2) +
      JASPgraphs::geom_point(x = at, y = value)
  }
  plot <- JASPgraphs::themeJasp(plot)

  pdfPlot[['plotObject']] <- plot

  return()
}


.ldPlotGaussianCDF <- function(jaspResults, options){
  cdfPlot <- createJaspPlot(title = "", width = 600, height = 320)
  
  cdfPlot$dependOnOptions(c("sd", "mu", "range",
                            "highlightPoint", "highlightPointValue", "highlightPointTangent", "highlightPointAt"))
  
  jaspResults[['cdfContainer']][['cdf']] <- cdfPlot
  
  .ldFillPlotGaussianCDF(cdfPlot, options)
  
  return()
}

.ldFillPlotGaussianCDF <- function(pdfPlot, options){
  
  plot <- ggplot2::ggplot(data = data.frame(x = c(-options[['range']], options[['range']])), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = pnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), size = 1) +
    ggplot2::ylab("Probability (X<x)")
    
  if(options[['highlightPoint']]){
    at <- options[['highlightPointAt']]
    value <- pnorm(at, options[['mu']], options[['sd']])
    
    if(options[['highlightPointTangent']]){
      slope <- dnorm(at, options[['mu']], options[['sd']])
      intercept <- value - at*slope
      slopeText <-  round(slope, 2) #bquote(paste(beta, " = ", .(round(slope, 2))))

      plot <- plot +
        ggplot2::geom_abline(data = NULL, slope = slope, intercept = intercept, color = "steelblue", size = 1) +
        ggplot2::geom_text(data = data.frame(x = (at-options[['range']])/2, y = 0.1+ intercept + slope*(at-options[['range']])/2),
                           ggplot2::aes(x = x, y = y), label = slopeText, size = 6, parse = FALSE)
    }
    
    if(options[['highlightPointValue']]){
      plot <- plot +
        ggplot2::geom_segment(ggplot2::aes(x = -options[['range']]+options[['range']]/9, xend = at, y = value, yend = value), linetype = 2) +
        ggplot2::geom_text(data = data.frame(x = -options[['range']], y = value, label = round(value, 2)), ggplot2::aes(x = x, y = y, label = label), size = 6)
    }
    
    plot <- plot + 
      ggplot2::geom_linerange(x = at, ymin = 0, ymax = value, linetype = 2) +
      JASPgraphs::geom_point(x = at, y = value)
  }
  
  plot <- JASPgraphs::themeJasp(plot)
  plot <- plot + ggplot2::scale_y_continuous(limits = c(0,1))
  pdfPlot[['plotObject']] <- JASPgraphs::themeJasp(plot)
  
  return()
}
