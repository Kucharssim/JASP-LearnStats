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

.ldFitStatisticsTable <- function(fitContainer, options, method){
  if(!is.null(fitContainer[['fitStatisticsTable']])) return()
  
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk")
  optionsTests <- allTests %in% names(options)
  whichTests <- unlist(options[allTests[optionsTests]])
  
  if(all(!whichTests)) return()
  
  tab <- createJaspTable(title = "Fit Statistics")
  tab$position <- 1
  tab$dependOn(c(method, allTests[optionsTests]))
  tab$setExpectedSize(rows = sum(whichTests))
  
  
  tab$addColumnInfo(name = "test", title = "Test", type = "string")
  tab$addColumnInfo(name = "statistic", title = "Statistic", type = "number")
  tab$addColumnInfo(name = "p.value", title = "<i>p</i>", type = "pvalue")
  
  fitContainer[['fitStatisticsTable']] <- tab

  return(tab)  
}

.ldFitStatisticsResults <- function(fitContainer, fit, variable, options, ready){
  if(!ready) return()
  if(is.null(fit)) return()
  if(!is.null(fitContainer[['fitStatisticsResults']])) return(fitContainer[['fitStatisticsResults']]$object)
  
  #browser()
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk")
  tests <- allTests[allTests %in% names(options)]
  
  res <- data.frame(test = tests, statistic = numeric(length = length(tests)), p.value = numeric(length = length(tests)))
  for(test in tests){
    arg <- switch (test,
                   "kolmogorovSmirnov" = c(list(x = variable, y = options$cdfFun), as.list(fit$estimate)),
                   "shapiroWilk" = list(x = variable),
                   c(list(x = variable, null = options$cdfFun), as.list(fit$estimate))
    )
    fun <- switch (test,
                   "kolmogorovSmirnov" = ks.test,
                   "cramerVonMisses"   = goftest::cvm.test,
                   "andersonDarling"   = goftest::ad.test,
                   "shapiroWilk"       = shapiro.test,
    )
    compute <- do.call(fun, arg)
    res[res$test == test, "statistic"] <- as.numeric(compute$statistic)
    res[res$test == test, "p.value"]   <- as.numeric(compute$p.value)
  }
  
  fitContainer[['fitStatisticsResults']] <- createJaspState(object = res)
  
  return(res)
}

.ldFillFitStatisticsTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk")
  tests <- allTests[allTests %in% names(options)]
  testNames <- c("Kolmogorov-Smirnov", "Cramer von Misses", "Anderson-Darling", "Shapiro-Wilk")[allTests %in% names(options)]
  
  whichTests <- unlist(options[tests])
  
  results$test <- testNames
  res <- results[whichTests,]
  
  table$setData(res)
  
  return()
}
### Fill plots----
.ldFitPlots <- function(fitContainer, estimates, options, variable, ready){
  if(is.null(fitContainer[['estPDF']]) && options$estPDF){
    pdfplot <- createJaspPlot(title = "Histogram vs. Theoretical PDF")
    pdfplot$dependOn(c("estPDF"))
    pdfplot$position <- 2
    fitContainer[['estPDF']] <- pdfplot
    
    if(ready)
      .ldFillEstPDFPlot(pdfplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['qqplot']]) && options$qqplot){
    qqplot <- createJaspPlot(title = "Q-Q plot")
    qqplot$dependOn(c("qqplot"))
    qqplot$position <- 3
    fitContainer[['qqplot']] <- qqplot
    
    if(ready)
      .ldFillQQPlot(qqplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['estCDF']]) && options$estCDF){
    cdfplot <- createJaspPlot(title = "Empirical vs. Theoretical CDF")
    cdfplot$dependOn(c("estCDF"))
    cdfplot$position <- 4
    fitContainer[['estCDF']] <- cdfplot
    
    if(ready)
      .ldFillEstCDFPlot(cdfplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['ppplot']]) && options$ppplot){
    ppplot <- createJaspPlot(title = "P-P plot")
    ppplot$dependOn(c("ppplot"))
    ppplot$position <-5
    fitContainer[['ppplot']] <- ppplot
    
    if(ready)
      .ldFillPPPlot(ppplot, estimates, options, variable)
  }
  
  return()
}

.ldFillQQPlot <- function(qqplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(sample = variable)) +
    ggplot2::stat_qq(distribution = options[['qFun']], dparams = estParameters) +
    ggplot2::stat_qq_line(distribution = options[['qFun']], dparams = estParameters) +
    ggplot2::xlab("Theoretical") + ggplot2::ylab("Sample")
  
  p <- JASPgraphs::themeJasp(p)
  
  qqplot$plotObject <- p
  
  return()
}

.ldFillEstPDFPlot <- function(pdfplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), fill = "steelblue") +
    ggplot2::stat_function(fun = options[['pdfFun']], args = as.list(estParameters), size = 1.5) + 
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range(variable)) +
    ggplot2::ylab("Density") + ggplot2::xlab(options[['variable']])
  
  p <- JASPgraphs::themeJasp(p)
  
  pdfplot$plotObject <- p
  
  return()
}

.ldFillPPPlot <- function(ppplot, estParameters, options, variable){
  n <- length(variable)
  ObservedProp <- (1:n)/n - 0.5/n
  
  args <- as.list(estParameters)
  args[['q']] <- variable
  TheoreticalProp <- sort(do.call(options[['cdfFun']], args))
  
  p <- ggplot2::ggplot(data = NULL) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::geom_point(ggplot2::aes(x = TheoreticalProp, y = ObservedProp)) +
    ggplot2::xlab("Theoretical") + ggplot2::ylab("Sample") +
    ggplot2::scale_x_continuous(limits = 0:1) + ggplot2::scale_y_continuous(limits = 0:1)
  
  p <- JASPgraphs::themeJasp(p)
  
  ppplot$plotObject <- p 
  
  return()
}

.ldFillEstCDFPlot <- function(cdfplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_rug() +
    ggplot2::stat_function(fun = options[['cdfFun']], args = as.list(estParameters), size = 1.5) + 
    ggplot2::scale_x_continuous(limits = range(variable)) +
    ggplot2::scale_y_continuous(limits = 0:1) + 
    ggplot2::ylab("Probability (X \u2264 x)") + ggplot2::xlab(options[['variable']])
  
  p <- JASPgraphs::themeJasp(p)
  
  cdfplot$plotObject <- p
  
  return()
}