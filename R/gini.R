#' @title gini2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is gini function
#' @details Todo
#' @export
#' 
gini <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$X <- dataset$ipuc*dataset$wHX040
    dataset$p_i <- dataset$wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    dataset$pi2 <- dataset$p_i/2
    dataset$acum.p_i <- cumsum(dataset$p_i)
    dataset$Fi <-  dataset$acum.p_i - dataset$pi2
    M <- sum(dataset$X)/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    gini <- 100*(2*sum(dataset$ipuc*dataset$p_i*dataset$Fi)/M-1)
    return(gini)
  }else{
    gini3 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      dataset.boot$X <- dataset.boot$ipuc*dataset.boot$wHX040
      dataset.boot$p_i <- dataset.boot$wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      dataset.boot$pi2 <- dataset.boot$p_i/2
      dataset.boot$acum.p_i <- cumsum(dataset.boot$p_i)
      dataset.boot$Fi <-  dataset.boot$acum.p_i - dataset.boot$pi2
      M <- sum(dataset.boot$X)/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      100*(2*sum(dataset.boot$ipuc*dataset.boot$p_i*dataset.boot$Fi)/M-1)
    }
    boot.gini <- boot(dataset, statistic = gini3, R = rep,
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