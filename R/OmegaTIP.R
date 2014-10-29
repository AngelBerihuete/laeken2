#' OmegaTIP
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description ToDo
#' @return ToDo
#' @export
#' 
OmegaTIP <- function(dataset, arpt.value, normalization = FALSE, samp){
  
  G <- arpt.value-dataset$ipuc
  dataset$pg <- pmax(G, 0) # poverty gaps
  
  dataset1 <- dataset[order(dataset[,'pg']), ] # order with pg
  dataset1$Acum <- cumsum(dataset1$wHX040)
  dataset1$Acum.P_i <- dataset1$Acum/dataset1$Acum[length(dataset1$Acum)]
  
  number.homes <- length(dataset1$Acum)
  number.individuals <- dataset1$Acum[number.homes]

  select <- (1:samp)/samp
  
  n.selected <- length(select)
  select.aux <- select*number.individuals
  select.aux <- floor(select.aux)
  
  sigma <- matrix(NA, n.selected, n.selected)
  vector.gamma.i <- c()
  acum.pi <- c()
  
  for(i in 1:n.selected){
    index.aux1 <- which(dataset1$Acum.P_i<=select[i])
    pos.i <- length(index.aux1)
    gamma.i <- sum(dataset1$pg[1:pos.i]*dataset1$wHX040[1:pos.i])
    gamma.i <- gamma.i/select.aux[i]
    vector.gamma.i[i] <- gamma.i
    acum.pi[i] <- dataset1$Acum.P_i[pos.i]
    
    lambda.i <- sum((dataset1$pg[1:pos.i]-gamma.i)^2*dataset1$wHX040[1:pos.i])
    lambda.i <- lambda.i/select.aux[i]
    
    for(j in i:n.selected){
      index.aux2 <- which(dataset1$Acum.P_i<=select[j])
      pos.j <- length(index.aux2)
      
      gamma.j <- sum(dataset1$pg[1:pos.j]*dataset1$wHX040[1:pos.j])
      gamma.j <- gamma.j/select.aux[j]
      sigma[j,i] <- sigma[i,j] <- dataset1$Acum.P_i[pos.i]*(lambda.i+(1-dataset1$Acum.P_i[pos.j])*(dataset1$pg[pos.i]-gamma.i)*(dataset1$pg[pos.j]-gamma.j)+(dataset1$pg[pos.i]-gamma.i)*(gamma.j-gamma.i))
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

  # Obtaining the TIP curve
  K <- length(vector.gamma.i)
  gammak <- rep(vector.gamma.i[K], times = K)
  gammak.pk <- acum.pi*vector.gamma.i
  rev.gammak.pk <- rev(gammak.pk)
  tip.curve <- gammak-rev.gammak.pk
  
  if(normalization == TRUE){
    Omega.tip <- Omega.tip/arpt.value^2
    tip.curve <- tip.curve/arpt.value
  }

  return(list(Omega = Omega.tip, tip.curve = tip.curve))
}