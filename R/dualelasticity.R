#' @title dualelasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

dualelasticity <- function(lorenz.curve, samp = 10){
  p <- seq(0.1, 1, length = samp)
  y <- lorenz.curve
  spl <- smooth.spline(p, y)
  lorenz_1_p <- predict(spl, (1-p))
  pred <- predict(spl, (1-p), deriv=1)
  curve.delasticity <- (p*pred$y)/(1-lorenz_1_p$y)
  return(curve.delasticity)
}
