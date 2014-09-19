#' @title elasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity <- function(lorenz.curve){
  x <- seq(0.1, 1, by = 0.1)
  y <- lorenz.curve
  spl <- smooth.spline(x, y)
  pred <- predict(spl, x, deriv=1)
  curve.elasticity <- (x*pred$y)/y
  return(curve.elasticity)
}
