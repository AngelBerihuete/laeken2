#' @title zIndex
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is Z index
#' @details Todo
#' @export
zIndex <- function(aux.data, pz = 0.6, ci = FALSE, rep = 1000, verbose = FALSE){ 
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]), ]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    aux.data$abscisa2 <-
      aux.data$acum.weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    uc.median <- aux.data$ipuc[which(aux.data$abscisa2 > 0.5)[1]]
    z.index <- pz*uc.median # pz is the percentage for median
    return(z.index)
  }else{
    zIndex2 <- function(aux.data, i, pz){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2) # poblacional
      aux.data.boot$abscisa2 <-
        aux.data.boot$acum.weights2/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      aux.data$ipuc[which(aux.data$abscisa2 > 0.5)[1]]
      }
    boot.z.index <- boot(aux.data, statistic = zIndex2, R = rep,
                      sim = "ordinary", stype = "i", pz = pz)
    z.index.ci <- boot.ci(boot.z.index, type = "basic")
    if(verbose == FALSE){
      return(z.index.ci)
    }else{
      summary(z.index.ci)
      return(z.index.ci)
    }
  }
}