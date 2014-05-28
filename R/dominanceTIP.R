#' dominanceTIP
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description ToDo
#' @return ToDo
#' @export
test.TIP <- function(tip1, tip2){
  
  # Interpolating tip curve in the same points
  spl1 <- smooth.spline(tip1$x.tip, tip1$y.tip)
  spl2 <- smooth.spline(tip2$x.tip, tip2$y.tip)
  x <- seq(0, 1 , by = 0.1)
  pred1 <- predict(spl1, x)
  pred2 <- predict(spl2, x)
  
  # 
  # G <- z.index-aux.data$ipuc
  # aux.data$pg <- pmax(G, 0) # poverty gaps
  # 
  # aux.data1 <- aux.data[order(aux.data[,'pg']), ] # order with pg
  # aux.data1$Acum <- cumsum(aux.data1$weights1)
  # aux.data1$Acum.P_i <- aux.data1$Acum/aux.data1$Acum[length(aux.data1$Acum)]
  # 
  # number.homes <- length(aux.data1$Acum)
  # number.individuals <- aux.data1$Acum[number.homes]
  # 
  # select <- seq(0.02, 1, by = 0.02)
  # min.acum.pi <- aux.data1$Acum.P_i[1]
  # if(0.02 <= min.acum.pi){
  #   warning(paste('Cuantiles were set to seq(0.01, 1, by = 0.02) 
  #                 instead of seq(0.02, 1, by = 0.02), 
  #                 beacause min.acum.pi is )', min.acum.pi,', greater than 0.02',
  #                 sep = ''))
  #   select <- seq(0.02, 1, by = 0.02)
  # }
  # 
  # n.selected <- length(select)
  # select.aux <- select*number.individuals
  # select.aux <- floor(select.aux)
  # 
  # sigma <- matrix(NA, n.selected, n.selected)
  # vector.gamma.i <- c()
  # acum.pi <- c()
  # 
  # for(i in 1:n.selected){
  #   index.aux1 <- which(aux.data1$Acum.P_i<=select[i])
  #   pos.i <- length(index.aux1)
  #   gamma.i <- sum(aux.data1$pg[1:pos.i]*aux.data1$weights1[1:pos.i])
  #   gamma.i <- gamma.i/select.aux[i]
  #   vector.gamma.i[i] <- gamma.i
  #   acum.pi[i] <- aux.data1$Acum.P_i[pos.i]
  #   
  #   lambda.i <- sum((aux.data1$pg[1:pos.i]-gamma.i)^2*aux.data1$weights1[1:pos.i])
  #   lambda.i <- lambda.i/select.aux[i]
  #   
  #   for(j in i:n.selected){
  #     index.aux2 <- which(aux.data1$Acum.P_i<=select[j])
  #     pos.j <- length(index.aux2)
  #     
  #     gamma.j <- sum(aux.data1$pg[1:pos.j]*aux.data1$weights1[1:pos.j])
  #     gamma.j <- gamma.j/select.aux[j]
  #     sigma[j,i] <- sigma[i,j] <- aux.data1$Acum.P_i[pos.i]*(lambda.i+(1-aux.data1$Acum.P_i[pos.j])*(aux.data1$pg[pos.i]-gamma.i)*(aux.data1$pg[pos.j]-gamma.j)+(aux.data1$pg[pos.i]-gamma.i)*(gamma.j-gamma.i))
  #   }
  # }
  # 
  # dim.sigma <- dim(sigma)
  # aux.R <- mat.or.vec(nr=(dim.sigma[1]-1),nc=(dim.sigma[1]-1))
  # 
  # dim.aux.R <- dim(aux.R)
  # for(k in 1:dim.aux.R[1]){
  #   aux.R[k,(dim.aux.R[1]-k+1)] <- -1
  # }
  # 
  # matrixR <- mat.or.vec(nr=dim(sigma)[1],nc=dim(sigma)[2])
  # dim.matrixR <- dim(matrixR)
  # matrixR[,dim.matrixR[2]] <- rep(1,length.out=dim.matrixR[1])
  # matrixR[1:dim.aux.R[1], 1:dim.aux.R[2]] <- aux.R
  # 
  # Omega.tip <- matrixR %*% sigma %*% t(matrixR)
  # Omega.tip <- Omega.tip/number.individuals
  # 
  # if(norm == FALSE){
  #   Omega.tip <- Omega.tip
  # }else {
  #   Omega.tip <- Omega.tip/z.index^2
  # }
  # 
  # # Obtaining the TIP curve
  # K <- length(vector.gamma.i)
  # gammak <- rep(vector.gamma.i[K], times = K)
  # gammak.pk <- acum.pi*vector.gamma.i
  # rev.gammak.pk <- rev(gammak.pk)
  # 
  # tip.curve <- gammak-rev.gammak.pk
  

  
  list1 <- list(tip.curve = pred1, Omega = )
  list2 <- list(tip.curve = pred2, Omega = )
  
  phi1 <- list1$tip.curve
  phi2 <- list2$tip.curve
  
  phi1 <- phi1[-1]
  phi2 <- phi2[-1]
  
  threshold <- max(which.max(phi1), which.max(phi2))
  phi1 <- phi1[1:threshold]
  phi2 <- phi2[1:threshold]
  
  estim.phi <- phi1 - phi2
  Omega1 <- list1$Omega[1:threshold, 1:threshold]
  Omega2 <- list2$Omega[1:threshold, 1:threshold]
  OmegaTotal <-  Omega1 + Omega2
  chol.OmegaTotal <- chol(OmegaTotal)
  M <- chol2inv(chol.OmegaTotal)
  #M <- solve(OmegaTotal)

  dvec <- estim.phi %*% M
  Amat  <- diag(length(phi1))
  bvec <- rep(0,length(phi1))
  
  sol <- solve.QP(M,dvec,Amat,bvec=bvec) # 
  
  phi.tilde <- sol$solution
  t.value <- t(as.matrix(estim.phi-phi.tilde)) %*% M %*% t(t(as.matrix(estim.phi-phi.tilde)))
  
  
  # Upper and Lower bounds for the critical value for jointly testing equality and inequality restrictions (David & Palm). alpha = 0.05, K = 1 to 17
  bounds4critical.values <- c(2.706, 5.138, 7.045, 8.761, 10.371, 11.911, 13.401, 14.853, 16.274, 17.670, 19.045, 20.410, 21.742, 23.069, 24.384, 25.689, 26.983)
  
  if(t.value < bounds4critical.values[1]){
    print("Do not reject H0")
    p.value = NA
    return(list(t.value = t.value,  sol.QP = phi.tilde, threshold = threshold, p.value = p.value))
  }else if(t.value > bounds4critical.values[threshold]){
    print("Reject H0")
    p.value = NA
    return(list(t.value = t.value,  sol.QP = phi.tilde, threshold = threshold, p.value = p.value))
  }else{
    print("Inconclusive region ... calculating p-value (10000 simulations)")
    
    require(mvtnorm)
    require(plyr)
    require(quadprog)
    
    data.sim <- rmvnorm(n=10000, sigma=OmegaTotal)
    
    solve.QP4sim <- function(vector.simulated, M){
      dvec <- vector.simulated %*% M
      Amat  <- diag(dim(M)[1])
      bvec <- rep(0,dim(M)[1])
      
      sol <- solve.QP(M,dvec,Amat,bvec=bvec)
      
      phi.tilde <- sol$solution
      
      return(phi.tilde)  
    }
    
    vec.solved <- aaply(data.sim, .margins = 1, solve.QP4sim, M)
    
    diff.phi <- vec.solved
    #diff.phi <- data.sim-vec.solved
    
    count.pos <- function(diff.phi.vec){
      positv <- length(which(diff.phi.vec>0))
      return(positv)
      
    }
    
    n.positiv <- aaply(diff.phi,.margins=1, count.pos)
    props.positive <- table(n.positiv)/length(n.positiv)
    prob.chi <- rev(pchisq(t.value, df=0:threshold, lower.tail = FALSE))
    
    pos.weights <- as.numeric(names(props.positive)) + 1
    
    p.value <- sum(props.positive*prob.chi[pos.weights])
    
    return(list(t.value = t.value,  sol.QP = phi.tilde, threshold = threshold, p.value = p.value))
  }
}