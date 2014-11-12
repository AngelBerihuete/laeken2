#' @title dualelasticity
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

dualelasticity <- function(dataset, samp = 10){
  
  dataset <- subset(dataset, ipuc>0)
  clg <- glc(dataset, samp)
  clg <- clg[-1,]
  lp <- clg$y.lg/miuc(dataset)
  p <- clg$x.lg
  spl <- sm.spline(p,lp)
  lp_1_p <- predict(spl, (1-p))
  dpl_1_p <- predict(spl,(1-p), nderiv = 1) 
  delas <- ((1-p)*dpl_1_p)/lp_1_p
  results <- data.frame(x.delas = p, y.delas = delas)
  return(results)
}
