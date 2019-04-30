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

LDfDist <- function(jaspResults, dataset, options, state=NULL){
  
  ready <- FALSE
  if(options[['variable']] != ""){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    variable <- dataset[[.v(options[['variable']])]]
    ready <- TRUE
    options[['rangeVariable']] <- range(variable)
  }
  
  options <- .recodeOptionsLDfDist(options)
  
  
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
  
  if(!ready) return()
  
  jaspResults[['dataContainer']] <- createJaspContainer(title = paste0("Overview - ", options[['variable']]))
  
  if(is.null(jaspResults[['dataContainer']][["summary"]]) && options$summary){
    .ldSummaryContinuousTableMain(jaspResults, variable, options)
  }
  
  if(is.null(jaspResults[['dataContainer']][['histogram']]) && options$histogram){
    .ldPlotHistogram(jaspResults, options, variable)
  }
  
  if(is.null(jaspResults[['dataContainer']][['ecdf']]) && options$ecdf){
    .ldPlotECDF(jaspResults, options, variable)
  }
  
  
  
  if(options$methodUnbiased){
    if(is.null(jaspResults[['methodUnbiased']]))
      jaspResults[['methodUnbiased']] <- createJaspContainer(title = "Minimum variance unbiased estimate")
    
    .ldGaussianMethodUnbiasedResults(jaspResults, options, variable)
    .ldGaussianMethodMomentsTable(jaspResults[['methodUnbiased']], options)
    .ldFitAssessment(jaspResults[['methodUnbiased']], options, variable)
  }
  
  
  if(options$methodMoments){
    if(is.null(jaspResults[['methodMoments']]))
      jaspResults[['methodMoments']] <- createJaspContainer(title = "Method of Moments")
    
    .ldGaussianMethodMomentsResults(jaspResults, options, variable)
    .ldGaussianMethodMomentsTable(jaspResults[['methodMoments']], options)
    .ldFitAssessment(jaspResults[['methodMoments']], options, variable)
  }
  
  return()
}

.recodeOptionsLDfDist <- function(options){
  options[['pars']] <- list(df1 = options[['df1']], df2 = options[['df2']], ncp = options[['ncp']])
  
  options[['pdfFun']] <- df
  options[['cdfFun']] <- pf
  options[['qFun']]  <- qf
  
  options[['range_x']] <- c(0, options[['range']])
  
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