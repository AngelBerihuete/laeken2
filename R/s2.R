#' @title s22
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is s2 index function
#' @details Todo
#' @export
s2 <- function(dataset, arpt.value, norm = FALSE, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    # 
    # REVISAR CON CI = TRUE, argumentos de diferentes longitud
    # ---------------------
    
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    
    gap.aux <- arpt.value-dataset$ipuc
    dataset$pg <- pmax(gap.aux, 0) # poverty gaps
    
    dataset$aux.prod <- dataset$wHX040*dataset$pg
    dataset$acum.pg <- cumsum(dataset$aux.prod)
    
    if(norm == FALSE){
      dataset$tip <- dataset$acum.pg/dataset$acum.wHX040[length(
        dataset$acum.wHX040)]
    }else{
      dataset$tip <- dataset$acum.pg/dataset$acum.wHX040[length(
        dataset$acum.wHX040)]/arpt.value
    }
    
    alturas.triang <- dataset$tip[2:length(dataset$tip)] - dataset$tip[-length(dataset$tip)]
    length.x <- dataset$abscisa2[2:length(dataset$abscisa2)] - dataset$abscisa2[-length(dataset$abscisa2)]
    
    area.triang <- alturas.triang*length.x/2
    area.rectan <- length.x*dataset$tip[-length(dataset$tip)]
    areas <- area.triang + area.rectan
    areas <- c(dataset$abscisa2[1]*dataset$tip[1]/2, areas)
    cum.areas <- cumsum(areas)
    abscisas2 <- dataset$abscisa2

    s2 <- 2*cum.areas[length(cum.areas)] # s2 index
    #results <- c(s2, abscisas2, cum.areas)
    return(s2)
  
    }else{
    s23 <- function(dataset, i, arpt.value, norm){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      
      gap.aux <- arpt.value-dataset.boot$ipuc
      dataset.boot$pg <- pmax(gap.aux, 0) # poverty gaps
      
      dataset.boot$aux.prod <- dataset.boot$wHX040*dataset.boot$pg
      dataset.boot$acum.pg <- cumsum(dataset.boot$aux.prod)
      
      if(norm == FALSE){
        dataset.boot$tip <- dataset.boot$acum.pg/dataset.boot$acum.wHX040[length(
          dataset.boot$acum.wHX040)]
      }else{
        dataset.boot$tip <- dataset.boot$acum.pg/dataset.boot$acum.wHX040[length(
          dataset.boot$acum.wHX040)]/arpt.value
      }
      
      alturas.triang <- dataset.boot$tip[2:length(dataset.boot$tip)] - dataset.boot$tip[-length(dataset.boot$tip)]
      length.x <- dataset.boot$abscisa2[2:length(dataset.boot$abscisa2)] - dataset.boot$abscisa2[-length(dataset.boot$abscisa2)]
      
      area.triang <- alturas.triang*length.x/2
      area.rectan <- length.x*dataset.boot$tip[-length(dataset.boot$tip)]
      areas <- area.triang + area.rectan
      areas <- c(dataset.boot$abscisa2[1]*dataset.boot$tip[1]/2, areas)
      cum.areas <- cumsum(areas)
      abscisas2 <- dataset.boot$abscisa2
      2*cum.areas[length(cum.areas)] # s2 index
    }
    boot.s2 <- boot(dataset, statistic = s23, R = rep,
                      sim = "ordinary", stype = "i",
                    arpt.value = arpt.value, norm = norm)
    s2.ci <- boot.ci(boot.s2, type = "basic")
    if(verbose == FALSE){
      return(s2.ci)
    }else{
      plot(boot.s2)
      summary(s2.ci)
      return(s2.ci)
    }
  } 
}