#' @title miuc
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is mean income person function
#' @details Todo
#' @export
miuc <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  dataset <- dataset[order(dataset[,"ipuc"]),]
  if(ci == FALSE){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    number.homes <- length(dataset$acum.wHX040)
    number.individuals <- dataset$acum.wHX040[number.homes]
    miuc <- sum(dataset$ipuc*dataset$wHX040)/number.individuals
    return(miuc)
  }else{
    miuc2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      number.homes <- length(dataset.boot$acum.wHX040)
      number.individuals <- dataset.boot$acum.wHX040[number.homes]
      sum(dataset.boot$ipuc*dataset.boot$wHX040)/number.individuals
    }
    boot.miuc <- boot(dataset, statistic = miuc2, R = rep,
                     sim = "ordinary", stype = "i")
    miuc.ci <- boot.ci(boot.miuc, type = "basic")
    if(verbose == FALSE){
      return(miuc.ci)
    }else{
      plot(boot.miuc)
      summary(miuc.ci)
      return(miuc.ci)
    }
  } 
}