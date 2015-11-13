#' testGL
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description ToDo
#' @return ToDo
#' @export
testGL <- function(dataset1, dataset2, generalized = FALSE, samplesize = 10){
  
  list1 <- OmegaGL(dataset1, samp = samplesize)
  list2 <- OmegaGL(dataset2, samp = samplesize)
  
  if(generalized == FALSE){
    gl1 <- list1$gl.curve/miuc(dataset1)
    gl2 <- list2$gl.curve/miuc(dataset2)
  }else{
    gl1 <- list1$gl.curve
    gl2 <- list2$gl.curve
  }
  
  estim.gl <- gl1 - gl2
  Omega1 <- list1$Omega
  Omega2 <- list2$Omega
  OmegaTotal <-  Omega1 + Omega2
  chol.OmegaTotal <- chol(OmegaTotal)
  M <- chol2inv(chol.OmegaTotal)
  
  
  fr <- function(x){
    (estim.gl - x) %*% M %*% (estim.gl-x)
  }
  
  gr <- function(x){
    -2*M %*% (estim.gl - x)
  }
  
  res <- constrOptim(rep(0.5, threshold), fr, gr,
                     ui = diag(1, threshold), 
                     ci = rep(0, length = threshold))  
  
  #phi.tilde <- sol$solution
  #Tvalue <- t(as.matrix(estim.phi-phi.tilde)) %*% M %*% t(t(as.matrix(estim.phi-phi.tilde)))
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
    return(list(Tvalue = Tvalue,  solution = gl.tilde, p.value = p.value))
  }else if(Tvalue > bounds4critical.values[10]){
    print("Reject H0")
    p.value = NA
    return(list(Tvalue = Tvalue,  solution = gl.tilde, p.value = p.value))
  }else{
    print("Inconclusive region ... calculating p-value (10000 simulations)")
    vec.solved <- matrix(NA, 1000, threshold)
    i <- 1
    iterations <- 1
    while(i < 1001){
      estim.gl <- as.numeric(rmvnorm(n=1, sigma=OmegaTotal))
      
      res <- try(constrOptim(rep(0.5, threshold), fr, gr,
                             ui = diag(1, threshold), 
                             ci = rep(0, length = threshold))$par, silent = TRUE)
      
      if(is.numeric(res)){
        vec.solved[i,] <- res
        i <- i + 1
      }
      iterations <- iterations + 1
      stopifnot(iterations < 3000)
    }
    
    diff.gl <- vec.solved
  
    count.pos <- function(diff.gl.vec){
      positv <- length(which(diff.gl.vec > 1e-15))
      return(positv)
      
    }
    
    n.positiv <- aaply(diff.gl,.margins=1, count.pos)
    props.positive <- table(n.positiv)/length(n.positiv)
    prob.chi <- rev(pchisq(Tvalue, df=0:threshold, lower.tail = FALSE))
    
    pos.weights <- as.numeric(names(props.positive)) + 1
    
    p.value <- sum(props.positive*prob.chi[pos.weights])
    
    return(list(Tvalue = Tvalue,  solution = gl.tilde, p.value = p.value))
  }
}