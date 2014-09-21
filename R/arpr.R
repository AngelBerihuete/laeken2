#' @title arpr2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is arpr2 function
#' @details Todo
#' @export

arpr <- function(aux.data, arpt.value, ci = FALSE, rep = 1000, verbose = FALSE){
  
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]), ]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    aux.data$abscisa2 <-
      aux.data$acum.weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    arpr <- 100*(aux.data$abscisa2[length(which(aux.data$ipuc < arpt.value))])
    return(arpr)
  }else{
    arpr3 <- function(aux.data, i, arpt.value){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2) # poblacional
      aux.data.boot$abscisa2 <-
      aux.data.boot$acum.weights2/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      100*(aux.data.boot$abscisa2[length(which(aux.data.boot$ipuc < arpt.value))])
    }
    boot.arpr <- boot(aux.data, statistic = arpr3, R = rep,
                       sim = "ordinary", stype = "i", arpt.value = arpt.value)
    arpr.ci <- boot.ci(boot.arpr, type = "basic")
    if(verbose == FALSE){
      return(arpr.ci)
    }else{
      summary(arpr.ci)
      plot(boot.arpr)
      return(arpr.ci)
    }
  } 
}