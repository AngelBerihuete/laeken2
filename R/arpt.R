#' @title People at risk of poverty or social exclusion
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimate the threshold for the risk of poverty.
#' 
#' @param pz A number between 0 and 1. The default is 0.6 representing
#' the 60 percent of the median equivalized disposable income.
#' @param ci logical; if  TRUE, 95% confidente interval is given
#' for the risk of poverty.
#' @param rep A number to do the confidence interval using boostrap
#' technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#' 
#' @details People are considered to be at risk of poverty 
#' when their income is less than a particular threshold. 
#' In the EU, the standard definition for that threshold has been 
#' set at 60 percent of the median equivalized disposable income.
#' 
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:At-risk-of-poverty_rate}
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' arpt(ATdataset)
#' 
#' @export
arpt <- function(dataset, pz = 0.6, ci = FALSE, rep = 500, verbose = FALSE){ 
  
  
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    uc.median <- dataset$ipuc[which(dataset$abscisa2 > 0.5)[1]]
    arpt.value <- pz*uc.median # pz is the percentage for median
    return(arpt.value)
  }else{
    arpt2 <- function(dataset, i, pz){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      uc.median <- dataset.boot$ipuc[which(dataset.boot$abscisa2 > 0.5)[1]]
      pz*uc.median
      }
    boot.arpt.value <- boot(dataset, statistic = arpt2, R = rep,
                      sim = "ordinary", stype = "i", pz = pz)
    arpt.value.ci <- boot.ci(boot.arpt.value, type = "basic")
    if(verbose == FALSE){
      return(arpt.value.ci)
    }else{
      plot(boot.arpt.value)
      summary(arpt.value.ci)
      return(arpt.value.ci)
    }
  }
}