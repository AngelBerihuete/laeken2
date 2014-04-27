#' @title mip
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is mean income person function
#' @details Todo
#' @export
mip <- function(aux.data, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    aux.data <- aux.data[order(aux.data[,1]),]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    number.homes <- length(aux.data$acum.weights2)
    number.individuals <- aux.data$acum.weights2[number.homes]
    mip <- sum(aux.data$HX090*aux.data$HX050*aux.data$DB090)/number.individuals
    return(mip)
  }else{
    mip2 <- function(aux.data, i){
      aux.data.boot <- aux.data[i,]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2)
      number.homes <- length(aux.data.boot$acum.weights2)
      number.individuals <- aux.data.boot$acum.weights2[number.homes]
      sum(aux.data.boot$HX090*aux.data.boot$HX050*aux.data.boot$DB090)/number.individuals
    }
    boot.mip <- boot(aux.data, statistic = mip2, R = rep,
                     sim = "ordinary", stype = "i")
    mip.ci <- boot.ci(boot.mip, type = "basic")
    if(verbose == FALSE){
      return(mip.ci)
    }else{
      plot(boot.mip)
      summary(mip.ci)
      return(mip.ci)
    }
  } 
}