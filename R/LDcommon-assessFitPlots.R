.ldPlotQQ <- function(jaspResults, options, variable){
  par <- jaspResults[['methodMomentsResults']]$object[[1]]
  par[2] <- sqrt(par[2])
  names(par) <- c("mean", "sd")
  par <- as.list(par)
  
  plot <- createJaspPlot(title = "Q-Q plot", width = 400, height = 400)
  
  jaspResults[['fitContainer']][['qqplot']][['methodMoments']] <- plot
  
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(sample = variable)) +
    ggplot2::stat_qq(distribution = options[['qfFun']], dparams = par) +
    ggplot2::stat_qq_line(distribution = options[['qfFun']], dparams = par)
  
  p <- JASPgraphs::themeJasp(p)
  
  plot$plotObject <- p
  
  return()
}