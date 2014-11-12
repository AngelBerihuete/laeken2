#' @title elasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity <- function(dataset, samp = 10){
  dataset <- subset(dataset, ipuc>0)
  clg <- glc(dataset, samp)
  clg <- clg[-1,]
  lp <- clg$y.lg
  p <- clg$x.lg
  spl <- sm.spline(p,lp, cv =TRUE)
  lp <- predict(spl,p, nderiv = 0)
  dlp <- predict(spl,p, nderiv = 1)
  celas <- log(p) + log(dlp) - log(lp)
  results <- data.frame(x.elas = p, log.y.elas = dlogcelas)
  return(results)
}