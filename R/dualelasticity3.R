#' @title delasticity23
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is elasticity function
#' @details Todo
#' @export

dualelasticity3 <- function(dataset){
  
  dataset1 <- dataset[order(dataset[,'ipuc']),]
  dataset1 <- subset(dataset1, ipuc > 0)
  
  dataset1$acum1 <- acum1 <- cumsum(dataset1$wHX040)
  
  dataset1$p <- dataset1$acum1/dataset1$acum1[length(dataset1$acum1)]
  
  dataset1$invp <- 1-p
  
  dataset1 <- dataset1[order(dataset1[,'invp']),]
  
  dataset1$acum2 <- cumsum(dataset1$wHX040)
  dataset1$acum3 <- cumsum(dataset1$wHX040*dataset1$ipuc)
  
  delas <- c()
  
  for(i in 1:(dim(dataset1)[1]-1)){
   index <- which(dataset1$acum2 > acum1[i])[1]
   if(index == 1){
     delas[i] <- 1
   }else{
   num.delas <- dataset1$ipuc[index]
   dem.delas <- (dataset1$acum3[index-1] + dataset1$ipuc[index]*(acum1[i]-dataset1$acum2[index-1]))/acum1[i]
   delas[i] <- num.delas/dem.delas
  }
  }
  p <- acum1/acum1[length(acum1)]
  p <- p[-length(p)]
  results <- data.frame(p = p, delas = delas)
  return(results)
}
