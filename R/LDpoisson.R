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

LDpoisson <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsPoisson(options)
  
  #### Show poisson section ----
  .ldIntroText(jaspResults, options, "Poisson distribution")
  .ldPoissonParsSupportMoments(jaspResults, options)
  
  
  pmfContainer <- .ldGetPlotContainer(jaspResults, options, "plotPMF", "Probability Mass Function", 3)
  .ldFillPMFContainer(pmfContainer, options, .ldFormulaPoissonPMF)
  
  cmfContainer <- .ldGetPlotContainer(jaspResults, options, "plotCMF", "Cumulative Distribution Function", 4)
  .ldFillCMFContainer(cmfContainer, options, .ldFormulaPoissonCDF)
  
  
  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options)
  
  ready <- options[['variable']] != ""
  errors <- FALSE
  if(ready && is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    
    variable <- dataset[[.v(options[['variable']])]]
    variable <- variable[!is.na(variable)]
    errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
                         observations.amount = "<2",
                         limits.min = options$support$min, limits.max = options$support$max, 
                         exitAnalysisIfErrors = FALSE)
    errors <- .ldCheckInteger(variable, errors)
  }
  
  # overview of the data
  dataContainer <- .ldGetDataContainer(jaspResults, options, errors)
  
  readyDesc <- ready && (isFALSE(errors) || (is.null(errors$infinity) && is.null(errors$observations)))
  .ldSummaryContinuousTableMain(dataContainer, variable, options, readyDesc)
  .ldObservedMomentsTableMain  (dataContainer, variable, options, readyDesc)
  .ldPlotHistogram             (dataContainer, variable, options, readyDesc, "discrete")
  .ldPlotECDF                  (dataContainer, variable, options, readyDesc)
  
  
  #### Fit data and assess fit ----
  
  readyFit <- ready && isFALSE(errors)
  #### Maximum Likelihood ----
  if(options$methodMLE){
    mleContainer <- .ldGetFitContainer(jaspResults, options, "mleContainer", "Maximum likelihood", 7, errors)
    
    # parameter estimates
    mleEstimatesTable  <- .ldEstimatesTable(mleContainer, options, TRUE, TRUE, "methodMLE")
    mleResults   <- .ldMLEResults(mleContainer, variable, options, readyFit, options$distNameInR,
                                  .ldPoissonMethodMLEStructureResults)
    .ldFillPoissonEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    # fit assessment
    mleFitContainer    <- .ldGetFitContainer(mleContainer, options, "mleFitAssessment", "Fit Assessment", 8)

    # fit statistics
    mleFitStatistics   <- .ldFitStatisticsTable(mleFitContainer, options, "methodMLE")
    mleFitStatisticsResults <- .ldFitStatisticsResults(mleContainer, mleResults$fitdist, variable, options, readyFit)
    .ldFillFitStatisticsTable(mleFitStatistics, mleFitStatisticsResults, options, readyFit)
    #return()
    # fit plots
    .ldFitPlots(mleFitContainer, mleResults$fitdist$estimate, options, variable, readyFit)
    
  }
  
  #### Method of moments ----
  
  #### Unbiased estimate ----
  
  return()
}

### options ----
.ldRecodeOptionsPoisson <- function(options){
  
  options[['parValNames']] <- c("lambda")
  
  options[['pars']]   <- list(lambda = options[['lambda']])
    
  options[['pdfFun']] <- dpois
  options[['cdfFun']] <- ppois
  options[['qFun']]   <- qpois
  options[['rFun']]   <- rpois
  options[['distNameInR']] <- "pois"
  
  options[['range_x']] <- c(options[['min_x']], options[['max_x']])
  
  options[['highlightmin']] <- options[['min']]
  options[['highlightmax']] <- options[['max']]
 
  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0)
  options$upperBound <- c(Inf)
  
  options
}

### text fill functions -----
.ldPoissonParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettext("rate: \u03BB \u2208 \u211D: \u03BB \u003E 0")
    
    support <- gettext("x \u2208 \u2124: x \u2265 0")
    
    moments <- list()
    moments$expectation <- gettext("\u03BB")
    moments$variance <- gettext("\u03BB")
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaPoissonPMF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>\u03BB</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaPoissonCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>\u03BB</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaPoissonQF <- function(options){
  text <- "<MATH>
    Q(x; <span style='color:red'>\u03BB</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillPoissonEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  res <- results$structured
  res$parName <- c("\u03BB")
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}

.ldPoissonMethodMLEStructureResults <- function(fit, options){
  if(is.null(fit)) return()
  
  transformations <- c(lambda = "lambda")
  
  res <- sapply(transformations, function(tr) car::deltaMethod(fit$estimate, tr, fit$vcov, level = options$ciIntervalInterval))
  rownames(res) <- c("estimate", "se", "lower", "upper")
  res <- t(res)
  res <- cbind(par = rownames(res), res)
  res <- as.data.frame(res)
  
  return(res)
}