#' @title dualelasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

dualelasticity <- function(dataset, samp = 10){
  dataset <- subset(dataset, ipuc>0)
  clg <- glc(dataset, samp)
  clg <- clg[-1,]
  clg$y.lorenz <- clg$y.lg/miuc(dataset)
  p <- clg$x.lg
  Lp <- clg$y.lorenz
  spl <- smooth.spline(p, Lp)
  lorenz_1_p <- predict(spl, (1-p))
  pred <- predict(spl, (1-p), deriv=1)
  delas <- (p*pred$y)/(1-lorenz_1_p$y)
  results <- data.frame(x.delas = p, y.delas = delas)
  return(results)
}
