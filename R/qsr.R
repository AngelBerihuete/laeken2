#' @title qsr2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is quintile share ratio function
#' @details Todo
#' @export
qsr <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    A <- dataset$ipuc*dataset$wHX040
    uc.S20 <- sum(A[which(dataset$abscisa2 < 0.2)])
    uc.S80 <- sum(A[which(dataset$abscisa2 > 0.8)])
    qsr <- uc.S80/uc.S20
    return(qsr)
  }else{
    qsr3 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      A <- dataset.boot$ipuc*dataset.boot$wHX040
      uc.S20 <- sum(A[which(dataset.boot$abscisa2 < 0.2)])
      uc.S80 <- sum(A[which(dataset.boot$abscisa2 > 0.8)])
      uc.S80/uc.S20
    }
    boot.qsr <- boot(dataset, statistic = qsr3, R = rep,
                      sim = "ordinary", stype = "i")
    qsr.ci <- boot.ci(boot.qsr, type = "basic")
    if(verbose == FALSE){
      return(qsr.ci)
    }else{
      plot(boot.qsr)
      summary(qsr.ci)
      return(qsr.ci)
    }
  } 
}