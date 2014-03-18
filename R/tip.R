#' @title tip
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is TIP curve function
#' @details Todo
#' @export
tip <- function(aux.data, z.index, norm = FALSE){
  G <- z.index-aux.data$ipuc
  aux.data$pg <- pmax(G, 0) # poverty gaps

  aux.data1 <- aux.data[order(aux.data[,'pg']), ] # order with pg
  aux.data1$Acum <- cumsum(aux.data1$weights1)
  aux.data1$Acum.P_i <- aux.data1$Acum/aux.data1$Acum[length(aux.data1$Acum)]
  
  number.homes <- length(aux.data1$Acum)
  number.individuals <- aux.data1$Acum[number.homes]
  
  select <- seq(0.02, 1, by = 0.02)
  min.acum.pi <- aux.data1$Acum.P_i[1]
  if(0.02 <= min.acum.pi){
    warning(paste('Cuantiles were set to seq(0.01, 1, by = 0.02) 
                  instead of seq(0.02, 1, by = 0.02), 
                  beacause min.acum.pi is )', min.acum.pi,', greater than 0.02',
                  sep = ''))
    select <- seq(0.02, 1, by = 0.02)
  }
  
  n.selected <- length(select)
  select.aux <- select*number.individuals
  select.aux <- floor(select.aux)
  
  sigma <- matrix(NA, n.selected, n.selected)
  vector.gamma.i <- c()
  acum.pi <- c()
  
  for(i in 1:n.selected){
    index.aux1 <- which(aux.data1$Acum.P_i<=select[i])
    pos.i <- length(index.aux1)
    gamma.i <- sum(aux.data1$pg[1:pos.i]*aux.data1$weights1[1:pos.i])
    gamma.i <- gamma.i/select.aux[i]
    vector.gamma.i[i] <- gamma.i
    acum.pi[i] <- aux.data1$Acum.P_i[pos.i]
    
    lambda.i <- sum((aux.data1$pg[1:pos.i]-gamma.i)^2*aux.data1$weights1[1:pos.i])
    lambda.i <- lambda.i/select.aux[i]
    
    for(j in i:n.selected){
      index.aux2 <- which(aux.data1$Acum.P_i<=select[j])
      pos.j <- length(index.aux2)
      
      gamma.j <- sum(aux.data1$pg[1:pos.j]*aux.data1$weights1[1:pos.j])
      gamma.j <- gamma.j/select.aux[j]
      sigma[j,i] <- sigma[i,j] <- aux.data1$Acum.P_i[pos.i]*(lambda.i+(1-aux.data1$Acum.P_i[pos.j])*(aux.data1$pg[pos.i]-gamma.i)*(aux.data1$pg[pos.j]-gamma.j)+(aux.data1$pg[pos.i]-gamma.i)*(gamma.j-gamma.i))
    }
  }
  
  dim.sigma <- dim(sigma)
  aux.R <- mat.or.vec(nr=(dim.sigma[1]-1),nc=(dim.sigma[1]-1))
  
  dim.aux.R <- dim(aux.R)
  for(k in 1:dim.aux.R[1]){
    aux.R[k,(dim.aux.R[1]-k+1)] <- -1
  }
  
  matrixR <- mat.or.vec(nr=dim(sigma)[1],nc=dim(sigma)[2])
  dim.matrixR <- dim(matrixR)
  matrixR[,dim.matrixR[2]] <- rep(1,length.out=dim.matrixR[1])
  matrixR[1:dim.aux.R[1], 1:dim.aux.R[2]] <- aux.R
  
  Omega.tip <- matrixR %*% sigma %*% t(matrixR)
  Omega.tip <- Omega.tip/number.individuals
  
  if(norm == FALSE){
    Omega.tip <- Omega.tip
  }else {
    Omega.tip <- Omega.tip/z.index^2
  }
  
  # Obtaining the TIP curve
  K <- length(vector.gamma.i)
  gammak <- rep(vector.gamma.i[K], times = K)
  gammak.pk <- acum.pi*vector.gamma.i
  rev.gammak.pk <- rev(gammak.pk)
  
  tip.curve <- gammak-rev.gammak.pk
  
  if(norm == FALSE){
    tip.curve <- tip.curve
  }else{
    tip.curve <- tip.curve/z.index
  }
 results <- list(Omega = Omega.tip, tip.curve = tip.curve)
 return(results)
}