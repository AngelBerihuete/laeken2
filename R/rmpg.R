#' @title rmpg2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is the relative median at-risk-poverty gap function
#' @details Todo
#' @export
rmpg <- function(dataset, arpt.value, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]),]
    rmpg.data <- dataset[which(dataset$ipuc < arpt.value),]
    rmpg.data$weights.rmpg <- rmpg.data$DB090*rmpg.data$HX040  
    rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
    rmpg.data$abscisa.rmpg <-
      rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
    rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
    rmpg <- 100*(arpt.value-rmpg.median)/arpt.value
    return(rmpg)
  }else{
    rmpg3 <- function(dataset, i, arpt.value){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      rmpg.data <- dataset.boot[which(dataset.boot$ipuc < arpt.value),]
      rmpg.data$weights.rmpg <- rmpg.data$DB090*rmpg.data$HX040  
      rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
      rmpg.data$abscisa.rmpg <-
        rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
      rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
      100*(arpt.value-rmpg.median)/arpt.value
    }
    boot.rmpg <- boot(dataset, statistic = rmpg3, R = rep,
                     sim = "ordinary", stype = "i")
    rmpg.ci <- boot.ci(boot.rmpg, type = "basic")
    if(verbose == FALSE){
      return(rmpg.ci)
    }else{
      plot(boot.rmpg)
      summary(rmpg.ci)
      return(rmpg.ci)
    }
  } 
}