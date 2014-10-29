#' @title elasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity <- function(dataset, samp = 10){
  dataset <- subset(dataset, ipuc>0)
  clg <- glc(dataset, samp)
  clg <- clg[-1,]
  p <- clg$x.lg
  lp <- clg$y.lg/miuc(dataset)
  spl <- splinefun(p, lp)
  dlp <- spl(p, deriv = 1)
  celas <- (p*dlp)/lp
  results <- data.frame(x.elas = p, y.elas = celas)
  return(results)
}
