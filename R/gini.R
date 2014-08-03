#' @title gini2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is gini function
#' @details Todo
#' @export
gini <- function(aux.data, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]), ]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    aux.data$X <- aux.data$ipuc*aux.data$weights2
    aux.data$p_i <- aux.data$weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    aux.data$pi2 <- aux.data$p_i/2
    aux.data$acum.p_i <- cumsum(aux.data$p_i)
    aux.data$Fi <-  aux.data$acum.p_i - aux.data$pi2
    M <- sum(aux.data$X)/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    gini <- 100*(2*sum(aux.data$ipuc*aux.data$p_i*aux.data$Fi)/M-1)
    return(gini)
  }else{
    gini3 <- function(aux.data, i){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2)
      aux.data.boot$X <- aux.data.boot$ipuc*aux.data.boot$weights2
      aux.data.boot$p_i <- aux.data.boot$weights2/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      aux.data.boot$pi2 <- aux.data.boot$p_i/2
      aux.data.boot$acum.p_i <- cumsum(aux.data.boot$p_i)
      aux.data.boot$Fi <-  aux.data.boot$acum.p_i - aux.data.boot$pi2
      M <- sum(aux.data.boot$X)/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      2*sum(aux.data.boot$ipuc*aux.data.boot$p_i*aux.data.boot$Fi)/M-1
    }
    boot.gini <- boot(aux.data, statistic = gini3, R = rep,
                     sim = "ordinary", stype = "i")
    gini.ci <- boot.ci(boot.gini, type = "basic")
    if(verbose == FALSE){
      return(gini.ci)
    }else{
      summary(gini.ci)
      plot(boot.gini)
      return(gini.ci)
    }
  } 
}