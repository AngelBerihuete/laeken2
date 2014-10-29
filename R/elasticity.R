#' @title elasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity <- function(dataset, samp = 10){
  dataset <- subset(dataset, ipuc>0)
  clg <- glc(dataset, samp)
  clg <- clg[-1,]
  clg$y.lorenz <- clg$y.lg/miuc(dataset)
  p <- clg$x.lg
  Lp <- clg$y.lorenz
  spl <- smooth.spline(p, Lp)
  Lp.deriv <- predict(spl, p , deriv=1)
  celas <- (p*Lp.deriv$y)/Lp
  results <- data.frame(x.elas = p, y.elas = celas)
  return(results)
}
