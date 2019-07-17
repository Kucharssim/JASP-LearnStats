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

LDgaussianunivariate <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDGaussianUnivariate(options)
  
  #### Show distribution section ----
  #.ldIntroText(jaspResults, options, .ldGaussianIntro)
  .ldGaussianParsSupportMoments(jaspResults, options)
   
  
  pdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotPDF", "Probability Density Function", 3)
  .ldFillPDFContainer(pdfContainer, options, NULL, .ldFormulaGaussianPDF)
  
  cdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotCDF", "Cumulative Distribution Function", 4)
  .ldFillCDFContainer(cdfContainer, options, NULL, .ldFormulaGaussianCDF)
  
  qfContainer  <- .ldGetPlotContainer(jaspResults, options, "plotQF", "Quantile Function", 5)
  .ldFillQFContainer(qfContainer,   options, NULL, .ldFormulaGaussianQF)
  
  #### Generate and Display data section ----
  #.simulateData(jaspResults, options)
  
  ready <- options[['variable']] != ""

  if(ready && is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    
    variable <- dataset[[.v(options[['variable']])]]
    variable <- variable[!is.na(variable)]
  }
  
  # overview of the data
  dataContainer <- .ldGetDataContainer(jaspResults, options)

  .ldSummaryContinuousTableMain(dataContainer, variable, options, ready)
  .ldObservedMomentsTableMain  (dataContainer, variable, options, ready)
  .ldPlotHistogram             (dataContainer, variable, options, ready)
  .ldPlotECDF                  (dataContainer, variable, options, ready)
  
  return()
  
  #### Fit data and assess fit ----
  if(options[['methodUnbiased']]){
    if(is.null(jaspResults[['methodUnbiased']])){
      jaspResults[['methodUnbiased']] <- createJaspContainer(title = "Unbiased estimator")
      jaspResults[['methodUnbiased']]$dependOn(c("methodUnbiased", "variable"))
      jaspResults[['methodUnbiased']]$position <- 7
    }
    
    .ldGaussianMethodUnbiasedResults(jaspResults, options, variable, ready)
    .ldGaussianEstimatesTable(jaspResults[['methodUnbiased']], options, ready, TRUE)
    .ldFitAssessment(jaspResults[['methodUnbiased']], options, variable, ready)
  }
  
  if(options[['methodMoments']]){
    if(is.null(jaspResults[['methodMoments']])){
      jaspResults[['methodMoments']] <- createJaspContainer(title = "Method of Moments")
      jaspResults[['methodMoments']]$dependOn(c("methodMoments", "variable"))
      jaspResults[['methodMoments']]$position <- 8
    }
    
   .ldGaussianMethodMomentsResults(jaspResults, options, variable, ready)
   .ldGaussianEstimatesTable(jaspResults[['methodMoments']], options, ready, FALSE)
   .ldFitAssessment(jaspResults[['methodMoments']], options, variable, ready)
  }
  
    
  return()
}

### options ----
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
  
  options[['parValNames']] <- c("mu", "varValue")
  
  options[['pars']]   <- list(mean = options[['mu']], sd = options[['sd']])
  options[['pdfFun']] <- dnorm
  options[['cdfFun']] <- pnorm
  options[['qFun']]   <- qnorm
  options[['rFun']]   <- rnorm
  
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

### text fill functions -----
.ldIntroText <- function(jaspResults, options, introText){
  if(!options$explanatoryText) return()
  
  intro <- createJaspHtml()
  intro$dependOn(c("explanatoryText"))
  intro$position <- 1
  intro[['text']] <- introText()
  jaspResults[['exp_Intro']] <- intro
  
  
  return()  
}

.ldGaussianIntro <- function(){
  intro <- "<h3> Demonstration of the Normal Distribution </h3>
This demonstration is divided into four parts. The first part displays the Normal distribution, its probability density function, 
cumulative distribution function, and quantile function. The second part allows to generate data from the Normal distribution and compute
descriptive statistics and display descriptive plots. In the third part, the parameters of the Normal distribution can be estimated.
The fourth part allows to check the fit of the Normal distribution to the data.
  "
  
  return(intro)
}

.ldGaussianParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    formulas <- createJaspHtml(title = "Parameters, Support, and Moments")
    formulas$dependOn(c("parsSupportMoments", "parametrization"))
    formulas$position <- 2
    
    text <- "<b>Parameters</b>
    mean: &mu; \u2208 \u211D
    "
    
    text2 <- "<b>Support</b>
    x \u2208 \u211D"
    
    text3 <- "<b>Moments</b> 
    E(X) = &mu;
    Var(X) = "
    
    if(options[['parametrization']] == "sigma2"){
      text <- paste(text,
                    "variance: &sigma;<sup>2</sup> \u2208 \u211D<sup>+</sup>
                    ")
      text3 <- paste0(text3, "&sigma;<sup>2</sup>")
    } else if(options[['parametrization']] == "sigma"){
      text <- paste(text,
                    "standard deviation: &sigma; \u2208 \u211D<sup>+</sup>")
      text3 <- paste0(text3, "&sigma;<sup>2</sup>")
    } else if(options[['parametrization']] == "tau2"){
      text <- paste(text,
                    "precision: &tau;<sup>2</sup> \u2208 \u211D<sup>+</sup>")
      text3 <- paste0(text3, "1/&tau;<sup>2</sup>")
    } else{
      text <- paste(text,
                    "square root of precision: &tau; \u2208 \u211D<sup>+</sup>")
      text3 <- paste0(text3, "1/&tau;<sup>2</sup>")
    }
    
    formulas$text <- paste(text, text2, text3, sep = "<br><br>")
    
    jaspResults[['parsSupportMoments']] <- formulas
  }
}

.ldFormulaGaussianPDF <- function(options){
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

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaGaussianCDF <- function(options){
  if(options$parametrization == "sigma2"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "sigma"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else if(options$parametrization == "tau2"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaGaussianQF <- function(options){
  if(options$parametrization == "sigma2"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "sigma"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else if(options$parametrization == "tau2"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Estimation functions ----
.ldGaussianEstimatesTable <- function(methodContainer, options, ready, ci.possible){
  if(!is.null(methodContainer[['estParametersTable']])) return()
  
  if(!options$outputEstimates) return()
  
  estParametersTable <- createJaspTable(title = "Estimated Parameters", dependencies = "variable")
  
  estParametersTable$dependOn(c("variable", "parametrization", "outputEstimates", "ciInterval", "ciIntervalInterval", "simulateNow"))
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
  
  # par2 <- c(sigma2 = "\u03C3<sup>2</sup>", sigma = "\u03C3", 
  #           tau2 = "\u03C4<sup>2</sup>", tau = "\u03C4")[[options[['parametrization']]]]
  
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
  
  
  if(is.null(jaspResults[['methodMoments']][['results']]$object)){
    jaspResults[['methodMoments']][['results']] <- createJaspState()
    jaspResults[['methodMoments']][['results']]$dependOn(c("variable", "simulateNow"))
    
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
  
  
  
  if(is.null(jaspResults[['methodUnbiased']][['results']]$object)){
    jaspResults[['methodUnbiased']][['results']] <- createJaspState()
    jaspResults[['methodUnbiased']][['results']]$dependOn(c("variable", "simulateNow", "ciInterval"))
    
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
    methodContainer[['fitAssessment']]$dependOn(c("variable", "simulateNow"))
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
    cdfplot <- createJaspPlot(title = "Empirical vs. Theoretical CDF")
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
  statisticsTable$position <- 6
  statisticsTable$dependOn(c("variable", "kolmogorovSmirnov",
                             "cramerVonMisses", "andersonDarling",
                             "shapiroWilk", "simulateNow"))
  
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
                                                                       "cramerVonMisses", "andersonDarling",
                                                                       "shapiroWilk", "simulateNow"))
  
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk")
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
    } else if(test == "cramerVonMisses"){
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
    sample <- do.call(options[['rFun']], c(options[['pars']], options[['sampleSize']]))
    jaspResults[['simdata']] <- createJaspState(sample)
    jaspResults[['simdata']]$dependOn(c("newVariableName", "simulateNow"))
    .setColumnDataAsScale(options[["newVariableName"]], sample)
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