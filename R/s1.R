#' @title s1
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is maximun of TIP curve function
#' @details Todo
#' @export

s1 <- function(dataset, arpt.value, norm = FALSE, ci = FALSE, 
               rep = 1000, verbose = FALSE){
  if( ci == FALSE){
    maxtip <- max(tip(dataset, arpt.value, norm))
    return(maxtip)  
  }else{
    s11 <- function(dataset, i, arpt.value, norm){
      max(tip(dataset[i,], arpt.value, norm)) # s1 index
    }
    boot.s1 <- boot(dataset, statistic = s11, R = rep,
                    sim = "ordinary", stype = "i", arpt.value, norm)
    s1.ci <- boot.ci(boot.s1, type = "basic")
    if(verbose == FALSE){
      return(s1.ci)
    }else{
      plot(boot.s1)
      summary(s1.ci)
      return(s1.ci)
    } 
  }
}