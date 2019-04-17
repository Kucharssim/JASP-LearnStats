# .ldPlotGaussianPDF <- function(jaspResults, options){
#   pdfPlot <- createJaspPlot(title = "", width = 600, height = 320)
# 
#   pdfPlot$dependOn(c("sd", "mu", "range", 
#                             "highlightIntervals", "highlightmin", "highlightmax"))
# 
#   jaspResults[['pdfContainer']][['pdf']] <- pdfPlot
# 
#   #.ldFillPlotGaussianPDF(pdfPlot, options)
#   .ldFillPlotDistribution(pdfPlot, options, dnorm)
#   return()
# }
# 
# .ldFillPlotGaussianPDF <- function(pdfPlot, options){
# 
#   plot <- ggplot2::ggplot(data = data.frame(x = c(-options[['range']], options[['range']])), ggplot2::aes(x = x)) +
#     ggplot2::stat_function(fun = dnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), size = 1)  +
#     ggplot2::ylab("Density")
#   
#   if(options[['highlightIntervals']]){
#     plot <- plot +
#       ggplot2::stat_function(fun = dnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), geom = "area",
#                              xlim = c(options[['highlightmin']], options[['highlightmax']]), fill = "steelblue")
#   }
# 
#   if(options[['highlightPoint']]){
#     at <- options[['highlightPointAt']]
#     value <- dnorm(at, options[['mu']], options[['sd']])
#     area <- pnorm(at, options[['mu']], options[['sd']])
#     
#     if(options[['highlightPointTangent']]){
#       plot <- plot +
#         ggplot2::stat_function(fun = dnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), geom = "area",
#                                xlim = c(-options[['range']], at), fill = "steelblue") + 
#         ggplot2::geom_text(data = data.frame(x = at-1, y = value/2, label = round(area, 2)), ggplot2::aes(x = x, y = y, label = label), size = 10)
#     }
#     
#     if(options[['highlightPointValue']]){
#       plot <- plot +
#         ggplot2::geom_segment(ggplot2::aes(x = -options[['range']]+options[['range']]/9, xend = at, y = value, yend = value), linetype = 2) +
#         ggplot2::geom_text(data = data.frame(x = -options[['range']], y = value, label = round(value, 2)), ggplot2::aes(x = x, y = y, label = label), size = 6)
#     }
#     
#     plot <- plot + 
#       ggplot2::geom_linerange(x = at, ymin = 0, ymax = value, linetype = 2) +
#       JASPgraphs::geom_point(x = at, y = value)
#   }
#   plot <- JASPgraphs::themeJasp(plot)
# 
#   pdfPlot[['plotObject']] <- plot
# 
#   return()
# }
# .ldPlotGaussianCDF <- function(jaspResults, options){
#   cdfPlot <- createJaspPlot(title = "", width = 600, height = 320)
#   
#   cdfPlot$dependOn(c("sd", "mu", "range",
#                      "highlightPoint", "highlightPointValue", "highlightPointTangent", "highlightPointAt"))
#   
#   jaspResults[['cdfContainer']][['cdf']] <- cdfPlot
#   
#   .ldFillPlotGaussianCDF(cdfPlot, options)
#   
#   return()
# }
# 
# .ldFillPlotGaussianCDF <- function(pdfPlot, options){
#   
#   plot <- ggplot2::ggplot(data = data.frame(x = c(-options[['range']], options[['range']])), ggplot2::aes(x = x)) +
#     ggplot2::stat_function(fun = pnorm, n = 101, args = list(mean = options[['mu']], sd = options[['sd']]), size = 1) +
#     ggplot2::ylab("Probability (X<x)")
#   
#   # if(options[['highlightPoint']]){
#   #   at <- options[['highlightPointAt']]
#   #   value <- pnorm(at, options[['mu']], options[['sd']])
#   #   
#   #   if(options[['highlightPointTangent']]){
#   #     slope <- dnorm(at, options[['mu']], options[['sd']])
#   #     intercept <- value - at*slope
#   #     slopeText <-  round(slope, 2) #bquote(paste(beta, " = ", .(round(slope, 2))))
#   # 
#   #     plot <- plot +
#   #       ggplot2::geom_abline(data = NULL, slope = slope, intercept = intercept, color = "steelblue", size = 1) +
#   #       ggplot2::geom_text(data = data.frame(x = (at-options[['range']])/2, y = 0.1+ intercept + slope*(at-options[['range']])/2),
#   #                          ggplot2::aes(x = x, y = y), label = slopeText, size = 6, parse = FALSE)
#   #   }
#   #   
#   #   if(options[['highlightPointValue']]){
#   #     plot <- plot +
#   #       ggplot2::geom_segment(ggplot2::aes(x = -options[['range']]+options[['range']]/9, xend = at, y = value, yend = value), linetype = 2) +
#   #       ggplot2::geom_text(data = data.frame(x = -options[['range']], y = value, label = round(value, 2)), ggplot2::aes(x = x, y = y, label = label), size = 6)
#   #   }
#   #   
#   #   plot <- plot + 
#   #     ggplot2::geom_linerange(x = at, ymin = 0, ymax = value, linetype = 2) +
#   #     JASPgraphs::geom_point(x = at, y = value)
#   # }
#   
#   plot <- JASPgraphs::themeJasp(plot)
#   plot <- plot + ggplot2::scale_y_continuous(limits = c(0,1))
#   pdfPlot[['plotObject']] <- JASPgraphs::themeJasp(plot)
#   
#   return()
# }