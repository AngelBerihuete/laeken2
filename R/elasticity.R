#' @title elasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity <- function(lorenz.curve, samp = 10){
  x <- seq(0.1, 0.9, length = samp)
  y <- lorenz.curve
  spl <- smooth.spline(x, y)
  pred <- predict(spl, x, deriv=1)
  curve.elasticity <- (x*pred$y)/y
  return(curve.elasticity)
}
