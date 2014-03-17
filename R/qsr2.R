#' @title qsr2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is quintile share ratio function
#' @details Todo
#' @export
qsr2 <- function(aux.data, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]), ]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    aux.data$abscisa2 <-
      aux.data$acum.weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    A <- aux.data$ipuc*aux.data$weights2
    uc.S20 <- sum(A[which(aux.data$abscisa2 < 0.2)])
    uc.S80 <- sum(A[which(aux.data$abscisa2 > 0.8)])
    qsr <- uc.S80/uc.S20
    return(qsr)
  }else{
    qsr3 <- function(aux.data, i){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2)
      aux.data.boot$abscisa2 <-
        aux.data.boot$acum.weights2/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      A <- aux.data$ipuc*aux.data$weights2
      uc.S20 <- sum(A[which(aux.data$abscisa2 < 0.2)])
      uc.S80 <- sum(A[which(aux.data$abscisa2 > 0.8)])
      uc.S80/uc.S20
    }
    boot.qsr <- boot(aux.data, statistic = qsr3, R = rep,
                      sim = "ordinary", stype = "i")
    qsr.ci <- boot.ci(boot.qsr, type = "basic")
    if(verbose == FALSE){
      return(qsr.ci)
    }else{
      summary(qsr.ci)
      return(qsr.ci)
    }
  } 
}