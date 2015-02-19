#' @title elasticity22
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

elasticity2 <- function(dataset){
  dataset1 <- dataset[order(dataset[,'ipuc']), ]
  dataset1 <- subset(dataset1, ipuc >0)
  denom <- dataset1$ipuc * dataset1$wHX040
  denom <- cumsum(denom)
  acum <- cumsum(dataset1$wHX040)
  p <- acum/acum[length(acum)]
  denom <- denom/acum
  elas <- dataset1$ipuc/denom
  results <- data.frame(p = p, elas = elas)
  return(results)
}
