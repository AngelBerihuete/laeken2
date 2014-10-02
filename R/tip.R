#' @title tip
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is TIP curve function
#' @details Todo
#' @export
tip <- function(aux.data, arpt.value, norm = FALSE){
  aux.data <- aux.data[order(aux.data[, "ipuc"]), ]
  aux.data$pg <- pmax(arpt.value - aux.data$ipuc, 0) # poverty gaps
  w2xpg <- aux.data$weights2*aux.data$pg
  acum.w2xpg <- cumsum(w2xpg)
  acum.weights2 <- cumsum(aux.data$weights2)
  y.tip <- acum.w2xpg/acum.weights2[length(acum.weights2)]
  x.tip <- acum.weights2/acum.weights2[length(acum.weights2)]
  if(norm == TRUE) y.tip <- y.tip/arpt.value
  tip.curve <- data.frame(x.tip = x.tip, y.tip = y.tip)
  return(tip.curve)
}