#' test.TIP2
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description ToDo
#' @return ToDo
#' @export
testTIP2 <- function(dataset1, dataset2, pz = 0.6,
                    same.arpt.value = NULL,
                    norm = FALSE, samplesize = 50){
  
  if(is.null(same.arpt.value)){
    arpt.value1 <- arpt(dataset1, pz = pz)
    arpt.value2 <- arpt(dataset2, pz = pz)
  }else{
    arpt.value1 <- arpt.value2 <- same.arpt.value
  }
  
  list1 <- OmegaTIP(dataset1, arpt.value1,
                    normalization = norm,
                    samp = samplesize)
  list2 <- OmegaTIP(dataset2, arpt.value2,
                    normalization = norm,
                    samp = samplesize)
  
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
  #M <- solve(OmegaTotal)
  M <- chol2inv(chol.OmegaTotal)
  
#   Dmat <- M
#   dvec <- as.numeric(M %*% estim.phi)
#   Amat  <- diag(length(phi1))
#   bvec <- rep(0,length(phi1))
#   
#  sol <- solve.QP(Dmat,dvec,Amat,bvec=bvec) # 
  
  fr <- function(x){
    (estim.phi - x) %*% M %*% (estim.phi-x)
  }
  
  gr <- function(x){
    -2*M %*% (estim.phi - x)
  }
  
  res <- constrOptim(rep(0.5, threshold), fr, gr,
                     ui = diag(1, threshold), 
                     ci = rep(0, length = threshold))  
  
  #phi.tilde <- sol$solution
  #t.value <- t(as.matrix(estim.phi-phi.tilde)) %*% M %*% t(t(as.matrix(estim.phi-phi.tilde)))
  phi.tilde <- res$par
  Tvalue <- res$value
  
  # Upper and Lower bounds for the critical value for jointly testing equality and inequality restrictions (David & Palm). alpha = 0.05, K = 1 to 17
  bounds4critical.values <- c(2.706, 5.138, 7.045, 8.761, 10.371, 
                              11.911, 13.401, 14.853, 16.274, 17.670, 
                              19.045, 20.410, 21.742, 23.069, 24.384, 
                              25.689, 26.983, 28.268, 29.545, 30.814,
                              32.077, 33.333, 34.583, 35.827, 37.066,
                              38.301, 39.531, 40.756, 41.977, 43.194,
                              44.408, 45.618, 46.825, 48.029, 49.229)
  
  if(Tvalue < bounds4critical.values[1]){
    print("Do not reject H0")
    p.value = NA
    return(list(Tvalue = Tvalue,  solution = phi.tilde, threshold = threshold, p.value = p.value))
  }else if(Tvalue > bounds4critical.values[threshold]){
    print("Reject H0")
    p.value = NA
    return(list(Tvalue = Tvalue,  solution = phi.tilde, threshold = threshold, p.value = p.value))
  }else{
    print("Inconclusive region ... calculating p-value (10000 simulations)")
    vec.solved <- matrix(NA, 1000, threshold)
    i <- 1
    while(i < 1001){
      estim.phi <- as.numeric(rmvnorm(n=1, sigma=OmegaTotal))
      
      res <- try(constrOptim(rep(0.5, threshold), fr, gr,
                  ui = diag(1, threshold), 
                  ci = rep(0, length = threshold))$par, silent = TRUE)
      
      if(is.numeric(res)){
       vec.solved[i,] <- res
       i <- i + 1
      }
    }
    
    diff.phi <- vec.solved
    #diff.phi <- data.sim-vec.solved
    
    count.pos <- function(diff.phi.vec){
      positv <- length(which(diff.phi.vec>1e-15))
      return(positv)
    }
    
    n.positiv <- aaply(diff.phi,.margins=1, count.pos)
    props.positive <- table(n.positiv)/length(n.positiv)
    prob.chi <- rev(pchisq(Tvalue, df=0:threshold, lower.tail = FALSE))
    
    pos.weights <- as.numeric(names(props.positive)) + 1
    
    p.value <- sum(props.positive*prob.chi[pos.weights])
    
    return(list(Tvalue = Tvalue,  solution = phi.tilde, threshold = threshold, p.value = p.value))
  }
}