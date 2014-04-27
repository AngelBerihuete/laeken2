#' @title miuc
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is mean income person function
#' @details Todo
#' @export
miuc <- function(aux.data, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]),]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    number.homes <- length(aux.data$acum.weights2)
    number.individuals <- aux.data$acum.weights2[number.homes]
    miuc <- sum(aux.data$ipuc*aux.data$weights2)/number.individuals
    return(miuc)
  }else{
    miuc2 <- function(aux.data, i){
      aux.data.boot <- aux.data[i,]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2)
      number.homes <- length(aux.data.boot$acum.weights2)
      number.individuals <- aux.data.boot$acum.weights2[number.homes]
      sum(aux.data.boot$ipuc*aux.data.boot$weights2)/number.individuals
    }
    boot.miuc <- boot(aux.data, statistic = miuc2, R = rep,
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