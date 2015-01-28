#' @title elasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity <- function(dataset, samp = 10){
  dataset <- subset(dataset, ipuc>0)
  clg <- glc(dataset, samp)
  clg <- clg[-1,]
  lp <- clg$y.lg/miuc(dataset)
  p <- clg$x.lg
  spl <- sm.spline(p,lp)
  lp <- predict(spl,p, nderiv = 0)
  dlp <- predict(spl,p, nderiv = 1)
  celas <- p*dlp/lp
  celas2 <- dlp/lp
  logcelas <- log(p) + log(dlp) - log(lp)
  logcelas2 <- log(dlp) - log(lp)
  loglp <- log(lp)
  results <- data.frame(x.elas = p, loglp = loglp, celas = celas, logcelas = logcelas,
                        celas2 = celas2, logcelas2 = logcelas2)
  return(results)
}