#' @title rmpg2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is the relative median at-risk-poverty gap function
#' @details Todo
#' @export
rmpg2 <- function(aux.data, z.index, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]),]
    rmpg.data <- aux.data[which(aux.data$ipuc < z.index),]
    rmpg.data$weights.rmpg <- rmpg.data$DB090*rmpg.data$HX040  
    rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
    rmpg.data$abscisa.rmpg <-
      rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
    rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
    rmpg <- 100*(z.index-rmpg.median)/z.index
    return(rmpg)
  }else{
    rmpg3 <- function(aux.data, i, z.index){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      rmpg.data <- aux.data.boot[which(aux.data.boot$ipuc < z.index),]
      rmpg.data$weights.rmpg <- rmpg.data$DB090*rmpg.data$HX040  
      rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
      rmpg.data$abscisa.rmpg <-
        rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
      rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
      100*(z.index-rmpg.median)/z.index
    }
    boot.rmpg <- boot(aux.data, statistic = rmpg3, R = rep,
                     sim = "ordinary", stype = "i")
    rmpg.ci <- boot.ci(boot.rmpg, type = "basic")
    if(verbose == FALSE){
      return(rmpg.ci)
    }else{
      summary(rmpg.ci)
      return(rmpg.ci)
    }
  } 
}