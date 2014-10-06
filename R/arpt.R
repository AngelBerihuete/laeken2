#' @title arpt
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is Z index
#' @details Todo
#' @export
arpt <- function(dataset, pz = 0.6, ci = FALSE, rep = 1000, verbose = FALSE){ 
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    uc.median <- dataset$ipuc[which(dataset$abscisa2 > 0.5)[1]]
    z.index <- pz*uc.median # pz is the percentage for median
    return(z.index)
  }else{
    arpt2 <- function(dataset, i, pz){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      dataset.boot$ipuc[which(dataset.boot$abscisa2 > 0.5)[1]]
      }
    boot.z.index <- boot(dataset, statistic = arpt2, R = rep,
                      sim = "ordinary", stype = "i", pz = pz)
    z.index.ci <- boot.ci(boot.z.index, type = "basic")
    if(verbose == FALSE){
      return(z.index.ci)
    }else{
      plot(boot.z.index)
      summary(z.index.ci)
      return(z.index.ci)
    }
  }
}