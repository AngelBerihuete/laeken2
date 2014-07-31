#' @title s22
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is s2 index function
#' @details Todo
#' @export
s2 <- function(aux.data, z.index, norm = FALSE, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    # 
    # REVISAR CON CI = TRUE, argumentos de diferentes longitud
    # ---------------------
    
    aux.data <- aux.data[order(aux.data[,1]), ]
    aux.data$acum.weights2 <- cumsum(aux.data$weights2)
    aux.data$abscisa2 <-
      aux.data$acum.weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
    
    gap.aux <- z.index-aux.data$ipuc
    aux.data$pg <- pmax(gap.aux, 0) # poverty gaps
    
    aux.data$aux.prod <- aux.data$weights2*aux.data$pg
    aux.data$acum.pg <- cumsum(aux.data$aux.prod)
    
    if(norm == FALSE){
      aux.data$tip <- aux.data$acum.pg/aux.data$acum.weights2[length(
        aux.data$acum.weights2)]
    }else{
      aux.data$tip <- aux.data$acum.pg/aux.data$acum.weights2[length(
        aux.data$acum.weights2)]/z.index
    }
    
    alturas.triang <- aux.data$tip[2:length(aux.data$tip)] - aux.data$tip[-length(aux.data$tip)]
    length.x <- aux.data$abscisa2[2:length(aux.data$abscisa2)] - aux.data$abscisa2[-length(aux.data$abscisa2)]
    
    area.triang <- alturas.triang*length.x/2
    area.rectan <- length.x*aux.data$tip[-length(aux.data$tip)]
    areas <- area.triang + area.rectan
    areas <- c(aux.data$abscisa2[1]*aux.data$tip[1]/2, areas)
    cum.areas <- cumsum(areas)
    abscisas2 <- aux.data$abscisa2

    s2 <- 2*cum.areas[length(cum.areas)] # s2 index
    #results <- c(s2, abscisas2, cum.areas)
    return(s2)
  
    }else{
    s23 <- function(aux.data, i, z.index, norm){
      aux.data.boot <- aux.data[i,]
      aux.data.boot <- aux.data.boot[order(aux.data.boot[,1]), ]
      aux.data.boot$acum.weights2 <- cumsum(aux.data.boot$weights2) # poblacional
      aux.data.boot$abscisa2 <-
        aux.data.boot$acum.weights2/aux.data.boot$acum.weights2[length(aux.data.boot$acum.weights2)]
      
      gap.aux <- z.index-aux.data.boot$ipuc
      aux.data.boot$pg <- pmax(gap.aux, 0) # poverty gaps
      
      aux.data.boot$aux.prod <- aux.data.boot$weights2*aux.data.boot$pg
      aux.data.boot$acum.pg <- cumsum(aux.data.boot$aux.prod)
      
      if(norm == FALSE){
        aux.data.boot$tip <- aux.data.boot$acum.pg/aux.data.boot$acum.weights2[length(
          aux.data.boot$acum.weights2)]
      }else{
        aux.data.boot$tip <- aux.data.boot$acum.pg/aux.data.boot$acum.weights2[length(
          aux.data.boot$acum.weights2)]/z.index
      }
      
      alturas.triang <- aux.data.boot$tip[2:length(aux.data.boot$tip)] - aux.data.boot$tip[-length(aux.data.boot$tip)]
      length.x <- aux.data.boot$abscisa2[2:length(aux.data.boot$abscisa2)] - aux.data.boot$abscisa2[-length(aux.data.boot$abscisa2)]
      
      area.triang <- alturas.triang*length.x/2
      area.rectan <- length.x*aux.data.boot$tip[-length(aux.data.boot$tip)]
      areas <- area.triang + area.rectan
      areas <- c(aux.data.boot$abscisa2[1]*aux.data.boot$tip[1]/2, areas)
      cum.areas <- cumsum(areas)
      abscisas2 <- aux.data.boot$abscisa2
      2*cum.areas[length(cum.areas)] # s2 index
    }
    boot.s2 <- boot(aux.data, statistic = s23, R = rep,
                      sim = "ordinary", stype = "i", z.index, norm)
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