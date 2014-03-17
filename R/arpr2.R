#' @title arpr2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is arpr2 function
#' @details Todo
#' @export
arpr2 <- function(aux.data, z.index, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]), ]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    aux.data$abscisa2 <-
      aux.data$acum.weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    arpr <- aux.data$abscisa2[length(which(aux.data$ipuc < z.index))]
    return(arpr)
  }else{
    arpr3 <- function(aux.data, i, z.index){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2) # poblacional
      aux.data.boot$abscisa2 <-
        aux.data.boot$acum.weights2/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      aux.data.boot$abscisa2[length(which(aux.data.boot$ipuc < z.index))]
    }
    boot.arpr <- boot(aux.data, statistic = arpr3, R = rep,
                       sim = "ordinary", stype = "i", z = z.index)
    arpr.ci <- boot.ci(boot.arpr, type = "basic")
    if(verbose == FALSE){
      return(arpr.ci)
    }else{
      summary(arpr.ci)
      return(arpr.ci)
    }
  } 
}