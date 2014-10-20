#' @title tip
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is TIP curve function
#' @details Todo
#' @export
tip <- function(dataset, arpt.value, norm = FALSE){
  dataset <- dataset[order(dataset[, "ipuc"]), ]
  dataset$pg <- pmax(arpt.value - dataset$ipuc, 0) # poverty gaps
  w2xpg <- dataset$wHX040*dataset$pg
  acum.w2xpg <- cumsum(w2xpg)
  acum.wHX040 <- cumsum(dataset$wHX040)
  y.tip <- acum.w2xpg/acum.wHX040[length(acum.wHX040)]
  x.tip <- acum.wHX040/acum.wHX040[length(acum.wHX040)]
  if(norm == TRUE) y.tip <- y.tip/arpt.value
  tip.curve <- data.frame(x.tip = c(0, x.tip), y.tip = c(0, y.tip))
  return(tip.curve)
}