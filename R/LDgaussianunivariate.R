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
  ready <- FALSE
  if(options[['variable']] != ""){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    variable <- dataset[[.v(options[['variable']])]]
    ready <- TRUE
    options[['rangeVariable']] <- range(variable)
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
    .ldPlotQF(jaspResults, options)
  }
  
  jaspResults[['dataContainer']] <- createJaspContainer(title = paste0("Overview -- ", options[['variable']]))
  
  if(is.null(jaspResults[['dataContainer']][["summary"]]) && options$summary){
    #.ldSummaryContinuousTableMain(jaspResults, variable, options)
  }
      
  if(is.null(jaspResults[['dataContainer']][['histogram']]) && options$histogram){
    .ldPlotHistogram(jaspResults, options, variable, rangeVariable)
  }
  
  if(is.null(jaspResults[['dataContainer']][['ecdf']]) && options$ecdf){
    .ldPlotECDF(jaspResults, options, variable, rangeVariable)
  }
  
  # jaspResults[['estimatesContainer']] <- createJaspContainer(title = "Estimated Parameters")
  if(is.null(jaspResults[['methodMoments']]) && options$methodMoments && ready){
    jaspResults[['methodMoments']] <- createJaspContainer(title = "Method of Moments")
    
    .ldGaussianMethodMomentsResults(jaspResults, options, variable)
    .ldGaussianMethodMomentsTable(jaspResults[['methodMoments']], options)
    .ldFitAssessment(jaspResults[['methodMoments']], options, variable)
  }
  
  # jaspResults[['fitContainer']] <- createJaspContainer(title = "Fit Assessment")
  # if(is.null(jaspResults[['fitContainer']][['qqplot']]) && options$qqplot){
  #   .ldPlotQQ(jaspResults, options, variable)
  # }
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
  options[['qFun']]  <- qnorm
  
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

#### Estimating methods ----
# .ldFillMainResults <- function(methodContainer, options, variable){
#   # state
#   # if(is.null(methodContainer[['estParameters']])){
#   #   .ldGaussianMethodMomentsTableMain(jaspResults, options, variable)
#   # }
#   
#   if(is.null(jaspResults[['estimatesContainer']][['methodMoments']][['fitAssessment']])){
#     .ldFitAssessment(jaspResults[['estimatesContainer']][['methodMoments']], options, variable)
#   }
# 
# }

.ldGaussianMethodMomentsTable <- function(methodContainer, options){
  estParametersTable <- createJaspTable(title = "Estimated Parameters")
  
  estParametersTable$dependOn(c("variable", "parametrization"))
  estParametersTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  estParametersTable$addColumnInfo(name = "par1", title = "\u03BC\u0302", type = "number", format = "sf:4")
  
  if(options$parametrization == "sigma2"){

    estParametersTable$addColumnInfo(name  = options$parametrization,
                                     title = "\u03C3\u0302<sup>2</sup>", type = "number", format = "sf:4")
  } else if(options$parametrization == "sigma"){

    estParametersTable$addColumnInfo(name  = options$parametrization,
                                     title = "\u03C3\u0302",  type = "number", format = "sf:4")

  } else if(options$parametrization == "tau2"){

    estParametersTable$addColumnInfo(name  = options$parametrization,
                                     title = "\u03C4\u0302<sup>2</sup>",   type = "number", format = "sf:4")

  } else if(options$parametrization == "tau"){

    estParametersTable$addColumnInfo(name  = options$parametrization,,
                                     title = "\u03C4\u0302",    type = "number", format = "sf:4")
  }
  #browser()
  #estParametersTable$addRow(as.list(methodContainer[['results']]$object[['table']]))
  methodContainer[['estParametersTable']] <- estParametersTable
  
  return()
}


.ldGaussianMethodMomentsResults <- function(jaspResults, options, variable){
  jaspResults[['methodMoments']][['results']] <- createJaspState()
  
  if(is.null(jaspResults[['methodMoments']][['results']])){
    results <- list()
    results$par <- .computeObservedMoments(x = variable, max.moment = 2, about.mean = TRUE)
    results$par[2] <- sqrt(results$par[2])
    names(results$par) <- c("mean", "sd")
    
    results$table <- c(mean = results$par[1],
                       sigma = results$par[2], sigma2 = results$par[2]^2, tau = results$par[2], tau2 = 1/results$par[2]^2)
    jaspResults[['methodMoments']][['results']]$object <- results
  }
  
  return()
}

.ldFitAssessment <- function(methodContainer, options, variable){
  methodContainer[['fitAssessment']] <- createJaspContainer(title = "Fit Assessment")
  estParameters <- methodContainer[['results']]$object[['par']]
  
  if(is.null(methodContainer[['fitAssessment']][['estPDF']]) && options$estPDF){
    pdfplot <- createJaspPlot(title = "Something")
    pdfplot$dependOn(c("variable"))
    methodContainer[['fitAssessment']][['estPDF']] <- pdfplot
    
    .ldFillEstPDFPlot(pdfplot, estParameters, options, variable)
  }
  
  if(is.null(methodContainer[['fitAssessment']][['qqplot']]) && options$qqplot){
    qqplot <- createJaspPlot(title = "Q-Q plot")
    qqplot$dependOn("variable")
    methodContainer[['fitAssessment']][['qqplot']] <- qqplot
    
    .ldFillQQPlot(qqplot, estParameters, options, variable)
  }
  
  if(is.null(methodContainer[['fitAssessment']][['estCDF']]) && options$estCDF){
    cdfplot <- createJaspPlot(title = "Something")
    cdfplot$dependOn("variable")
    methodContainer[['fitAssessment']][['estCDF']] <- cdfplot
    
    .ldFillEstCDFPlot(cdfplot, estParameters, options, variable)
  }
  
  if(is.null(methodContainer[['fitAssessment']][['ppplot']]) && options$ppplot){
    ppplot <- createJaspPlot(title = "P-P plot")
    ppplot$dependOn("variable")
    methodContainer[['fitAssessment']][['ppplot']] <- ppplot
    
    .ldFillPPPlot(ppplot, estParameters, options, variable)
  }
}

# .ldGaussianMethodMomentsTableMain <- function(jaspResults, options, variable){
#   jaspResults[['estimatesContainer']][['methodMoments']] <- createJaspContainer(title = "Method of Moments")
#   
#   # # observed moments
#   # obsMomentsTable <- createJaspTable(title = "Observed Moments")
#   # 
#   # obsMomentsTable$dependOn(c("variable"))
#   # obsMomentsTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
#   # 
#   # noOfNeededMoments <- length(options[['pars']])
#   # 
#   # obsMomentsTable$addColumnInfo(name = "moment",     title = "Moment",       type = "integer")
#   # obsMomentsTable$addColumnInfo(name = "aboutorigin",       title = "Raw",           type = "number", format = "sf:4")
#   # obsMomentsTable$addColumnInfo(name = "aboutmean",        title = "Centered",       type = "number", format = "sf:4")
#   # 
#   # obsMomentsTable$setExpectedRows(noOfNeededMoments)
#   # obsMomentsTable$addFootnote(message = "Raw k<sup>th</sup> moment is calculated as 1/n \u2211 x<sup>k</sup>.",
#   #                             col_names="aboutorigin")
#   # obsMomentsTable$addFootnote(message = "For k > 1, centered k<sup>th</sup> moment is calculated as 1/n \u2211 (x-x\u0305)<sup>k</sup>.",
#   #                             col_names="aboutmean")
#   # 
#   # jaspResults[['estimatesContainer']][['methodMoments']][['obsMomentsTable']] <- obsMomentsTable
#   # 
#   # obsMomentsTable[['moment']]      <- 1:noOfNeededMoments
#   # obsMomentsTable[['aboutorigin']] <- .computeMoments(x = variable, max.moment = noOfNeededMoments, about.mean = FALSE)
#   # obsMomentsTable[['aboutmean']]   <- .computeMoments(x = variable, max.moment = noOfNeededMoments, about.mean = TRUE)
#   # 
#   
#   
#   # est Parameters
#   estParametersTable <- createJaspTable(title = "Estimated Parameters")
#   
#   estParametersTable$dependOn(c("variable", "parametrization"))
#   estParametersTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
# 
#   estParametersTable$addColumnInfo(name = "par1", title = "\u03BC\u0302", type = "number", format = "sf:4")
#   
#   # state
#   if(is.null(jaspResults[['methodMomentsResults']])){
#     methodMomentsResults <- createJaspState()
#     jaspResults[['methodMomentsResults']] <- methodMomentsResults
#     methodMomentsResults$dependOn(c("variable"))
#     methodMomentsResults$object <- list(.computeMoments(x = variable, max.moment = noOfNeededMoments, about.mean = TRUE))
#   }
#   
#   # fill
#   par <- jaspResults[['methodMomentsResults']]$object[[1]]
# 
#   if(options$parametrization == "sigma2"){
#     
#     estParametersTable$addColumnInfo(name = "par2", title = "\u03C3\u0302<sup>2</sup>", type = "number", format = "sf:4")
#   } else if(options$parametrization == "sigma"){
#     
#     estParametersTable$addColumnInfo(name = "par2", title = "\u03C3\u0302",  type = "number", format = "sf:4")
#     par[2] <- sqrt(par[2])
#     
#   } else if(options$parametrization == "tau2"){
#     
#     estParametersTable$addColumnInfo(name = "par2", title = "\u03C4\u0302<sup>2</sup>",   type = "number", format = "sf:4")
#     par[2] <- 1/par[2]
#     
#   } else if(options$parametrization == "tau"){
#     
#     estParametersTable$addColumnInfo(name = "par2", title = "\u03C4\u0302",    type = "number", format = "sf:4")
#     par[2] <- 1/sqrt(par[2])
#     
#   }
#   
#   estParametersTable$setExpectedRows(1)
#   
#   jaspResults[['estimatesContainer']][['methodMoments']][['estParametersTable']] <- estParametersTable
#   
#   estParametersTable$addRows(list(par1 = par[1], par2 = par[2]))
#   
#   
#   return()
# }