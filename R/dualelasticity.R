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
  lp_1_p <- predict(spl, (1-p), nderiv = 0)
  dpl_1_p <- predict(spl,(1-p), nderiv = 1) 
  delas <- (p*dpl_1_p)/(1-lp_1_p)
  logdelas <- log(p) + log(dpl_1_p) - log(1-lp_1_p)
  delas2 <- (dpl_1_p)/(1-lp_1_p)
  logdelas2 <- log(dpl_1_p) - log(1-lp_1_p)
  results <- data.frame(x.delas = p, delas = delas, logdelas = logdelas,
                        delas2 = delas2, logdelas2 = logdelas2)
  return(results)
}
