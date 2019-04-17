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

  #jaspResults$title <- "Normal distribution"
  
  if(options[['variable']] != ""){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    variable <- dataset[[.v(options[['variable']])]]
    
    rangeVariable <- range(variable)
    #rangeVariable[1] <- floor(rangeVariable[1])
    #rangeVariable[2] <- ceiling(rangeVariable[2])
  }
  
  options <- .recodeOptionsLDGaussianUnivariate(options)

  
  jaspResults[['pdfContainer']] <- createJaspContainer(title = "Probability Density Function")
  if(is.null(jaspResults[['pdfContainer']][['pdfPlot']]) && options$plotPDF){
    .ldPlotPDF(jaspResults, options)
  }
  if(is.null(jaspResults[['pdfContainer']][['formula']]) && options$formulaPDF && options$plotPDF){
    .ldFormulaGaussianPDF(jaspResults, options)
  }
  
  jaspResults[['cdfContainer']] <- createJaspContainer(title = "Cumulative Distribution Function")
  if(is.null(jaspResults[['cdfContainer']][['cdfPlot']]) && options$plotCDF){
    .ldPlotCDF(jaspResults, options)
  }
  if(is.null(jaspResults[['cdfContainer']][['formula']]) && options$formulaCDF && options$plotCDF){
    .ldFormulaGaussianCDF(jaspResults, options)
  }
  
  jaspResults[['qfContainer']] <- createJaspContainer(title = "Quantile Function")
  if(is.null(jaspResults[['qfContainer']][['qf']]) && options$plotQF){
    .ldPlotGaussianQF(jaspResults, options)
  }
  
  jaspResults[['dataContainer']] <- createJaspContainer(title = paste0("Overview -- ", options[['variable']]))
  
  if(is.null(jaspResults[['dataContainer']][["summary"]]) && options$summary){
    .ldSummaryContinuousTableMain(jaspResults, variable, options)
  }
      
  if(is.null(jaspResults[['dataContainer']][['histogram']]) && options$histogram){
    .ldPlotHistogram(jaspResults, options, variable, rangeVariable)
  }
  
  if(is.null(jaspResults[['dataContainer']][['ecdf']]) && options$ecdf){
    .ldPlotECDF(jaspResults, options, variable, rangeVariable)
  }
  
  jaspResults[['estimatesContainer']] <- createJaspContainer(title = "Estimated Parameters")
  if(is.null(jaspResults[['estimatesContainer']][['methodMoments']]) && options$methodMoments){
    .ldGaussianMethodMomentsTableMain(jaspResults, options, variable)
  }
  
  jaspResults[['fitContainer']] <- createJaspContainer(title = "Fit Assessment")
  if(is.null(jaspResults[['fitContainer']][['qqplot']]) && options$qqplot){
    .ldPlotQQ(jaspResults, options, variable)
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
  options[['qdFun']]  <- qnorm
  
  options[['range_x']] <- c(-options[['range']], options[['range']])
  
  if(options[['highlightType']] == "minmax"){
    options[['highlightmin']] <- options[['min']]
    options[['highlightmax']] <- options[['max']]
  } else if(options[['highlightType']] == "lower"){
    options[['highlightmin']] <- options[['range_x']][1]
    options[['highlightmax']] <- options[['lower_max']]
  } else if(options[['highlightType']] == "upper"){
    options[['highlightmin']] <- options[['upper_min']]
    options[['highlightmax']] <- options[['range_x']][2]
  } else{
    options[['highlightmin']] <- options[['highlightmax']] <- NULL
  }
  
  
  options
}


.ldFormulaGaussianPDF <- function(jaspResults, options){
  pdfFormula <- createJaspHtml(title = "PDF formula", elementType = "h1")

  pdfFormula$dependOn("parametrization")

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
  
  qfPlot$dependOn(c("sd", "mu", "range"))
  
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



#### Estimating methods ----

.ldGaussianMethodMomentsTableMain <- function(jaspResults, options, variable){
  jaspResults[['estimatesContainer']][['methodMoments']] <- createJaspContainer(title = "Method of Moments")
  
  # observed moments
  obsMomentsTable <- createJaspTable(title = "Observed Moments")
  
  obsMomentsTable$dependOn(c("variable"))
  obsMomentsTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  noOfNeededMoments <- length(options[['pars']])
  
  obsMomentsTable$addColumnInfo(name = "moment",     title = "Moment",       type = "integer")
  obsMomentsTable$addColumnInfo(name = "aboutorigin",       title = "Raw",           type = "number", format = "sf:4")
  obsMomentsTable$addColumnInfo(name = "aboutmean",        title = "Centered",       type = "number", format = "sf:4")
  
  obsMomentsTable$setExpectedRows(noOfNeededMoments)
  obsMomentsTable$addFootnote(message = "Raw k<sup>th</sup> moment is calculated as 1/n \u2211 x<sup>k</sup>.",
                              col_names="aboutorigin")
  obsMomentsTable$addFootnote(message = "For k > 1, centered k<sup>th</sup> moment is calculated as 1/n \u2211 (x-x\u0305)<sup>k</sup>.",
                              col_names="aboutmean")
  
  jaspResults[['estimatesContainer']][['methodMoments']][['obsMomentsTable']] <- obsMomentsTable
  
  obsMomentsTable[['moment']]      <- 1:noOfNeededMoments
  obsMomentsTable[['aboutorigin']] <- .computeMoments(x = variable, max.moment = noOfNeededMoments, about.mean = FALSE)
  obsMomentsTable[['aboutmean']]   <- .computeMoments(x = variable, max.moment = noOfNeededMoments, about.mean = TRUE)
  
  
  
  # est Parameters
  estParametersTable <- createJaspTable(title = "Estimated Parameters")
  
  estParametersTable$dependOn(c("variable", "parametrization"))
  estParametersTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

  estParametersTable$addColumnInfo(name = "par1", title = "\u03BC\u0302", type = "number", format = "sf:4")
  
  # state
  if(is.null(jaspResults[['methodMomentsResults']])){
    methodMomentsResults <- createJaspState()
    jaspResults[['methodMomentsResults']] <- methodMomentsResults
    methodMomentsResults$dependOn(c("variable"))
    methodMomentsResults$object <- list(.computeMoments(x = variable, max.moment = noOfNeededMoments, about.mean = TRUE))
  }
  
  # fill
  par <- jaspResults[['methodMomentsResults']]$object[[1]]

  if(options$parametrization == "sigma2"){
    
    estParametersTable$addColumnInfo(name = "par2", title = "\u03C3\u0302<sup>2</sup>", type = "number", format = "sf:4")
  } else if(options$parametrization == "sigma"){
    
    estParametersTable$addColumnInfo(name = "par2", title = "\u03C3\u0302",  type = "number", format = "sf:4")
    par[2] <- sqrt(par[2])
    
  } else if(options$parametrization == "tau2"){
    
    estParametersTable$addColumnInfo(name = "par2", title = "\u03C4\u0302<sup>2</sup>",   type = "number", format = "sf:4")
    par[2] <- 1/par[2]
    
  } else if(options$parametrization == "tau"){
    
    estParametersTable$addColumnInfo(name = "par2", title = "\u03C4\u0302",    type = "number", format = "sf:4")
    par[2] <- 1/sqrt(par[2])
    
  }
  
  estParametersTable$setExpectedRows(1)
  
  jaspResults[['estimatesContainer']][['methodMoments']][['estParametersTable']] <- estParametersTable
  
  estParametersTable$addRows(list(par1 = par[1], par2 = par[2]))
  
  
  return()
}