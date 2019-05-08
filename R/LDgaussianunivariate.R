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
  
  .simulateData(jaspResults, options)
  
  #jaspResults$title <- "Normal distribution"
  ready <- FALSE
  variable <- NULL
  if(options[['variable']] != ""){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    variable <- dataset[[.v(options[['variable']])]]
    ready <- TRUE
    options[['rangeVariable']] <- range(variable)
  }
  
  options <- .recodeOptionsLDGaussianUnivariate(options)

  .ldPlotContinuousDistributionFunctions(jaspResults, options)
  
  # jaspResults[['pdfContainer']] <- createJaspContainer(title = "Probability Density Function")
  # if(is.null(jaspResults[['pdfContainer']][['pdfPlot']]) && options$plotPDF){
  #   .ldPlotPDF(jaspResults, options)
  # }
  # if(is.null(jaspResults[['pdfContainer']][['formula']]) && options$formulaPDF && options$plotPDF){
  #   .ldFormulaGaussianPDF(jaspResults, options)
  # }
  # 
  # jaspResults[['cdfContainer']] <- createJaspContainer(title = "Cumulative Distribution Function")
  # if(is.null(jaspResults[['cdfContainer']][['cdfPlot']]) && options$plotCDF){
  #   .ldPlotCDF(jaspResults, options)
  # }
  # if(is.null(jaspResults[['cdfContainer']][['formula']]) && options$formulaCDF && options$plotCDF){
  #   .ldFormulaGaussianCDF(jaspResults, options)
  # }
  # 
  # jaspResults[['qfContainer']] <- createJaspContainer(title = "Quantile Function")
  # if(is.null(jaspResults[['qfContainer']][['qf']]) && options$plotQF){
  #   .ldPlotQF(jaspResults, options)
  # }

  
  if(is.null(jaspResults[['dataContainer']]) && ready){
    jaspResults[['dataContainer']] <- createJaspContainer(title = paste0("Overview - ", options[['variable']]))
  }
  
  if(is.null(jaspResults[['dataContainer']][["summary"]]) && options$summary && ready){
    .ldSummaryContinuousTableMain(jaspResults, variable, options)
  }
      
  if(is.null(jaspResults[['dataContainer']][['histogram']]) && options$histogram && ready){
    .ldPlotHistogram(jaspResults, options, variable)
  }
  
  if(is.null(jaspResults[['dataContainer']][['ecdf']]) && options$ecdf && ready){
    .ldPlotECDF(jaspResults, options, variable)
  }
  
  if(options[['methodUnbiased']]){
    if(is.null(jaspResults[['methodUnbiased']])){
      jaspResults[['methodUnbiased']] <- createJaspContainer(title = "Minimum variance unbiased estimator")
      jaspResults[['methodUnbiased']]$dependOn(c("methodUnbiased", "variable"))
      jaspResults[['methodUnbiased']]$position <- 100
    }
    
    .ldGaussianMethodUnbiasedResults(jaspResults, options, variable, ready)
    .ldGaussianEstimatesTable(jaspResults[['methodUnbiased']], options, ready, TRUE)
    .ldFitAssessment(jaspResults[['methodUnbiased']], options, variable, ready)
  }
  
  if(options[['methodMoments']]){
    if(is.null(jaspResults[['methodMoments']])){
      jaspResults[['methodMoments']] <- createJaspContainer(title = "Method of Moments")
      jaspResults[['methodMoments']]$dependOn(c("methodMoments", "variable"))
      jaspResults[['methodMoments']]$position <- 101
    }
    
   .ldGaussianMethodMomentsResults(jaspResults, options, variable, ready)
   .ldGaussianEstimatesTable(jaspResults[['methodMoments']], options, ready, FALSE)
   .ldFitAssessment(jaspResults[['methodMoments']], options, variable, ready)
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

.ldGaussianEstimatesTable <- function(methodContainer, options, ready, ci.possible){
  if(!is.null(methodContainer[['estParametersTable']])) return()
  
  if(!options$outputEstimates) return()
  
  estParametersTable <- createJaspTable(title = "Estimated Parameters", dependencies = "variable")
  
  estParametersTable$dependOn(c("variable", "parametrization", "outputEstimates", "ciInterval", "ciIntervalInterval"))
  estParametersTable$position <- 1
  estParametersTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  estParametersTable$showSpecifiedColumnsOnly <- TRUE
  
  estParametersTable$addColumnInfo(name = "mu", title = "\u03BC\u0302", type = "number", format = "sf:4")
  
  if(options$ciInterval && ci.possible){
    estParametersTable$addColumnInfo(name = "mu.lower", title = "Lower", type = "number", format = "sf:4",
                                     overtitle = sprintf("%s%% CI for \u03BC", options[['ciIntervalInterval']]*100))
    estParametersTable$addColumnInfo(name = "mu.upper", title = "Upper", type = "number", format = "sf:4",
                                     overtitle = sprintf("%s%% CI for \u03BC", options[['ciIntervalInterval']]*100))
  } else if(options$ciInterval){
    estParametersTable$addFootnote("Confidence intervals are unavailable with this method.")
  }
  
  par2 <- c(sigma2 = "\u03C3\u0302<sup>2</sup>", sigma = "\u03C3\u0302", 
            tau2 = "\u03C4\u0302<sup>2</sup>", tau = "\u03C4\u0302")[[options[['parametrization']]]]
  
  estParametersTable$addColumnInfo(name = options[['parametrization']],
                                   title = par2, type = "number", format = "sf:4")

  if(options$ciInterval && ci.possible){
    estParametersTable$addColumnInfo(name = paste0(options[['parametrization']], ".lower"),
                                     title = "Lower", type = "number", format = "sf:4",
                                     overtitle = sprintf("%s%% CI for %s", 
                                                         options[['ciIntervalInterval']]*100,
                                                         par2))
    estParametersTable$addColumnInfo(name = paste0(options[['parametrization']], ".upper"),
                                     title = "Upper", type = "number", format = "sf:4",
                                     overtitle = sprintf("%s%% CI for %s", 
                                                         options[['ciIntervalInterval']]*100, 
                                                         par2))
  }
  methodContainer[['estParametersTable']] <- estParametersTable
  
  if(!ready)
    return()
  
  # fill
  estParametersTable$addRows(as.list(methodContainer[['results']]$object[['table']]))
  return()
}


.ldGaussianMethodMomentsResults <- function(jaspResults, options, variable, ready){
  if(!ready || !options[['methodMoments']])
    return()
  
  jaspResults[['methodMoments']][['results']] <- createJaspState()
  
  if(is.null(jaspResults[['methodMoments']][['results']]$object)){
    results <- list()
    results$par <- .computeObservedMoments(x = variable, max.moment = 2, about.mean = TRUE)
    results$par[2] <- sqrt(results$par[2])
    names(results$par) <- c("mean", "sd")
    
    results$table <- c(mu = results$par[[1]],
                       sigma = results$par[[2]],
                       sigma2 = results$par[[2]]^2,
                       tau = results$par[[2]],
                       tau2 = 1/results$par[[2]]^2)
    jaspResults[['methodMoments']][['results']]$object <- results
  }
  
  return()
}

.ldGaussianMethodUnbiasedResults <- function(jaspResults, options, variable, ready){
  if(!ready || !options[['methodUnbiased']])
    return()
  
  
  jaspResults[['methodUnbiased']][['results']] <- createJaspState()
  jaspResults[['methodUnbiased']][['results']]$dependOn(c("variable"))
  
  if(is.null(jaspResults[['methodUnbiased']][['results']]$object)){
    results <- list()
    results$par <- c(mean = mean(variable), sd = .sdGaussianUnbiased(variable))
    names(results$par) <- c("mean", "sd")
    
    results$table <- c(mu = results$par[[1]],
                       sigma = results$par[[2]],
                       sigma2 = var(variable),
                       tau = 1/results$par[[2]],
                       tau2 = 1/var(variable))
    
    if(options[['ciInterval']]){
      res <- t.test(variable, conf.level = options[['ciIntervalInterval']])
      resvar <- ci.GaussianVar(variable, conf.level = options[['ciIntervalInterval']])
      ressd  <- ci.GaussianSD (variable, conf.level = options[['ciIntervalInterval']])
      
      results$table <- c(results$table, mu.lower = res[['conf.int']][[1]], mu.upper = res[['conf.int']][[2]],
                         sigma2.lower = resvar[1], sigma2.upper = resvar[2],
                         sigma.lower  = ressd[1], sigma.upper = ressd[2],
                         tau2.lower = 1/resvar[1], tau2.upper = 1/resvar[2],
                         tau.lower = 1/ressd[1], tau.upper = 1/ressd[2])
      
    }
    jaspResults[['methodUnbiased']][['results']]$object <- results
  }
  
  return()
}

.ldFitAssessment <- function(methodContainer, options, variable, ready){
  if(is.null(methodContainer[['fitAssessment']])){
    methodContainer[['fitAssessment']] <- createJaspContainer(title = "Fit Assessment")
    methodContainer[['fitAssessment']]$dependOn(c("variable"))
  }
  
  
  estParameters <- methodContainer[['results']]$object[['par']]
  
  .ldFillAssessmentTable(methodContainer, estParameters, options, variable, ready)
  
  
  if(is.null(methodContainer[['fitAssessment']][['estPDF']]) && options$estPDF){
    pdfplot <- createJaspPlot(title = "Histogram vs. Theoretical PDF")
    pdfplot$dependOn(c("estPDF"))
    pdfplot$position <- 2
    methodContainer[['fitAssessment']][['estPDF']] <- pdfplot
    
    if(ready)
      .ldFillEstPDFPlot(pdfplot, estParameters, options, variable)
  }
  
  if(is.null(methodContainer[['fitAssessment']][['qqplot']]) && options$qqplot){
    qqplot <- createJaspPlot(title = "Q-Q plot")
    qqplot$dependOn(c("qqplot"))
    qqplot$position <- 3
    methodContainer[['fitAssessment']][['qqplot']] <- qqplot
    
    if(ready)
      .ldFillQQPlot(qqplot, estParameters, options, variable)
  }
  
  if(is.null(methodContainer[['fitAssessment']][['estCDF']]) && options$estCDF){
    cdfplot <- createJaspPlot(title = "ECDF vs. Theoretical CDF")
    cdfplot$dependOn(c("estCDF"))
    cdfplot$position <- 4
    methodContainer[['fitAssessment']][['estCDF']] <- cdfplot
    
    if(ready)
      .ldFillEstCDFPlot(cdfplot, estParameters, options, variable)
  }
  
  if(is.null(methodContainer[['fitAssessment']][['ppplot']]) && options$ppplot){
    ppplot <- createJaspPlot(title = "P-P plot")
    ppplot$dependOn(c("ppplot"))
    ppplot$position <-5
    methodContainer[['fitAssessment']][['ppplot']] <- ppplot
    
    if(ready)
      .ldFillPPPlot(ppplot, estParameters, options, variable)
  }
  
  return()
}

.ldFillAssessmentTable <- function(methodContainer, estParameters, options, variable, ready){
  tests <- c(options$kolmogorovSmirnov, options$cramerVonMisses, options$andersonDarling, options$shapiroWilk)
  if(!any(tests))
     return()
  
  statisticsTable <- createJaspTable(title = "Statistics")
  statisticsTable$position <- 1
  statisticsTable$dependOn(c("variable", "kolmogorovSmirnov",
                             "cramerVonMisses", "andersonDarling",
                             "shapiroWilk"))
  
  statisticsTable$addColumnInfo(name = "test", title = "Test", type = "string")
  statisticsTable$addColumnInfo(name = "statistic", title = "Statistic", type = "number", format = "sf:4")
  statisticsTable$addColumnInfo(name = "p.value", title = "p-value", type = "number", format = "sf:4")
    
  methodContainer[['fitAssessment']][['statisticsTable']] <- statisticsTable
  
  
  
  if(ready){
    .ldAssessmentStatisticsResults(methodContainer, estParameters, options, variable)
    statisticsTable$addRows(methodContainer[['fitAssessment']][['statisticsResults']]$object)
  } else{
    test <- c("Kolmogorov-Smirnov", "Cramer-von Misses", "Anderson-Darling", "Shapiro-Wilk")[tests]
    statisticsTable[['test']] <- test
  }
  
  return()
}

.ldAssessmentStatisticsResults <- function(methodContainer, estParameters, options, variable){
  if(!is.null(methodContainer[['fitAssessment']][['statisticsResults']])) return()
  
  if(is.null(estParameters)) return()
  
  methodContainer[['fitAssessment']][['statisticsResults']] <- createJaspState()
  methodContainer[['fitAssessment']][['statisticsResults']]$dependOn(c("variable", "kolmogorovSmirnov",
                                                                       "cramerVonMises", "andersonDarling",
                                                                       "shapiroWilk"))
  
  allTests <- c("kolmogorovSmirnov", "cramerVonMises", "andersonDarling", "shapiroWilk")
  whichTests <- allTests[c(options$kolmogorovSmirnov,
                           options$cramerVonMisses,
                           options$andersonDarling,
                           options$shapiroWilk)]
  estParameters <- methodContainer[['results']]$object[['par']]
  
  results <- list()
  for(test in whichTests){
    if(test == "kolmogorovSmirnov"){
      res <- ks.test(variable, options[['cdfFun']], estParameters)
      results[[test]] <- list(test      = "Kolmogorov-Smirnov", 
                              statistic = res$statistic,
                              p.value   = res$p.value)
    } else if(test == "cramerVonMises"){
      # https://www.rdocumentation.org/packages/goftest/versions/1.0-4/topics/cvm.test
      res <- goftest::cvm.test(variable, null = options[['cdfFun']], estParameters)
      results[[test]] <- list(test = "Cramer-von Misses",
                              statistic = res$statistic,
                              p.value   = res$p.value)
    } else if(test == "andersonDarling"){
      res <- goftest::ad.test(variable, null = options[['cdfFun']], estParameters)
      results[[test]] <- list(test = "Anderson-Darling",
                              statistic = res$statistic,
                              p.value   = res$p.value)
    } else if(test == "shapiroWilk"){
      res <- shapiro.test(variable)
      results[[test]] <- list(test = "Shapiro-Wilk",
                              statistic = res$statistic,
                              p.value = res$p.value)
    }
  }
  
  methodContainer[['fitAssessment']][['statisticsResults']]$object <- results
  
  return()
}

.simulateData <- function(jaspResults, options){
  if(is.null(jaspResults[['simdata']])){
    jaspResults[['simdata']] <- createJaspState()
    jaspResults[['simdata']]$dependOn(c("simulateNow"))
    .setColumnDataAsScale(options[["newVariable"]], rnorm(100))
  }
  
  return()
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

.sdGaussianUnbiased <- function(x){
  # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
  x <- na.omit(x)
  n <- length(x)
  sdBiased <- sd(x)
  
  logCorrectionFactor <- 0.5*log(2) - 0.5*log(n-1) + lgamma(n/2) - lgamma((n-1)/2)
  correctionFactor <- exp(logCorrectionFactor)
  
  return(sdBiased/correctionFactor)
}


ci.GaussianVar <- function(x, conf.level = options[['ciIntervalInterval']]){
  x <- na.omit(x)
  df <- length(x) - 1
  v <- var(x)
  
  alpha <- 1-conf.level
  perc <- c(1-alpha/2, alpha/2)
  res <- v * df / qchisq(p = perc, df = df)
  
  return(res)
}

ci.GaussianSD <- function(variable, conf.level = options[['ciIntervalInterval']]){
  sqrt(ci.GaussianVar(variable, conf.level))
}