#' testGL
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description ToDo
#' @return ToDo
#' @export
testGL <- function(dataset1, dataset2){
  
  list1 <- OmegaGL(dataset1)
  list2 <- OmegaGL(dataset2)
  
  gl1 <- list1$gl.curve
  gl2 <- list2$gl.curve
  
  estim.gl <- gl1 - gl2
  Omega1 <- list1$Omega
  Omega2 <- list2$Omega
  OmegaTotal <-  Omega1 + Omega2
  chol.OmegaTotal <- chol(OmegaTotal)
  M <- chol2inv(chol.OmegaTotal)
  
  dvec <- estim.gl %*% M
  Amat  <- diag(length(gl1))
  bvec <- rep(0,length(gl1))
  
  sol <- solve.QP(M,dvec,Amat,bvec=bvec) # 
  
  gl.tilde <- sol$solution
  t.value <- t(as.matrix(estim.gl-gl.tilde)) %*% M %*% t(t(as.matrix(estim.gl-gl.tilde)))
  
  
  # Upper and Lower bounds for the critical value for jointly testing equality and inequality restrictions (David & Palm). alpha = 0.05, K = 1 to 17
  bounds4critical.values <- c(2.706, 5.138, 7.045, 8.761, 10.371, 11.911, 13.401, 14.853, 16.274, 17.670, 19.045, 20.410, 21.742, 23.069, 24.384, 25.689, 26.983)
  
  if(t.value < bounds4critical.values[1]){
    print("Do not reject H0")
    p.value = NA
    return(list(t.value = t.value,  sol.QP = gl.tilde, p.value = p.value))
  }else if(t.value > bounds4critical.values[10]){
    print("Reject H0")
    p.value = NA
    return(list(t.value = t.value,  sol.QP = gl.tilde, p.value = p.value))
  }else{
    print("Inconclusive region ... calculating p-value (10000 simulations)")
    
    data.sim <- rmvnorm(n=10000, sigma=OmegaTotal)
    
    solve.QP4sim <- function(vector.simulated, M){
      dvec <- vector.simulated %*% M
      Amat  <- diag(dim(M)[1])
      bvec <- rep(0,dim(M)[1])
      
      sol <- solve.QP(M,dvec,Amat,bvec=bvec)
      
      gl.tilde <- sol$solution
      
      return(gl.tilde)  
    }
    
    vec.solved <- aaply(data.sim, .margins = 1, solve.QP4sim, M)
    
    diff.gl <- vec.solved
    #diff.gl <- data.sim-vec.solved
    
    count.pos <- function(diff.gl.vec){
      positv <- length(which(diff.gl.vec>0))
      return(positv)
      
    }
    
    n.positiv <- aaply(diff.gl,.margins=1, count.pos)
    props.positive <- table(n.positiv)/length(n.positiv)
    prob.chi <- rev(pchisq(t.value, df=0:threshold, lower.tail = FALSE))
    
    pos.weights <- as.numeric(names(props.positive)) + 1
    
    p.value <- sum(props.positive*prob.chi[pos.weights])
    
    return(list(t.value = t.value,  sol.QP = gl.tilde, p.value = p.value))
  }
}