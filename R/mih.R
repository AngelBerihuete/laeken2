#' @title mih
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is mean income household function
#' @details Todo
#' @export
mih <- function(aux.data, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    mih <- sum(aux.data$HX090*aux.data$HX050*aux.data$DB090)/sum(aux.data$DB090)
    return(mih)
  }else{
    mih2 <- function(aux.data, i){
      aux.data.boot <- aux.data[i,]
      sum(aux.data.boot$HX090*aux.data.boot$HX050*aux.data.boot$DB090)/sum(aux.data.boot$DB090)
    }
    boot.mih <- boot(aux.data, statistic = mih2, R = rep,
                     sim = "ordinary", stype = "i")
    mih.ci <- boot.ci(boot.mih, type = "basic")
    if(verbose == FALSE){
      return(mih.ci)
    }else{
      summary(mih.ci)
      return(mih.ci)
    }
  } 
}