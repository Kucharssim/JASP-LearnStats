.ldGetFitContainer <- function(jaspResults, options, name, title, position){
  if(!is.null(jaspResults[[name]])){
    fitContainer <- jaspResults[[name]]
  } else{
    fitContainer <- createJaspContainer(title = title)
    fitContainer$position <- position
    
    fitContainer$dependOn(c("variable", "simulateNow"))
    
    jaspResults[[name]] <- fitContainer
  }
  
  return(fitContainer)
}

### MLE stuff ----
.ldMLEResults <- function(mleContainer, variable, options, ready, distName, structureFun){
  if(!ready) return()
  if(!is.null(mleContainer[['mleResults']])) return(mleContainer[['mleResults']]$object)
  
  results <- list()
  results$fitdist <- fitdistrplus::fitdist(data = variable, distr = distName, method = "mle", start = options$pars,
                                           keepdata = FALSE, discrete = FALSE)
  
  results$structured <- structureFun(results$fitdist, options)
  
  mleContainer[['mleResults']] <- createJaspState(object = results, dependencies = c(options$parValNames, "ciIntervalInterval"))
  
  return(results)
}

