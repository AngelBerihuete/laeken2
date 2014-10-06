#' @title arpr2
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is arpr2 function
#' @details Todo
#' @export

arpr <- function(dataset, arpt.value, ci = FALSE, rep = 1000, verbose = FALSE){
  
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    arpr <- 100*(dataset$abscisa2[length(which(dataset$ipuc < arpt.value))])
    return(arpr)
  }else{
    arpr3 <- function(dataset, i, arpt.value){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
      dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      100*(dataset.boot$abscisa2[length(which(dataset.boot$ipuc < arpt.value))])
    }
    boot.arpr <- boot(dataset, statistic = arpr3, R = rep,
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